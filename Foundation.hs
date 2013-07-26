module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)
import qualified Data.Text as T
import Control.Applicative
import qualified Data.ByteString as B
import Data.ByteString.Base16
import Data.Text.Encoding
import qualified System.Posix as Posix (fileSize, getFileStatus)
import Text.Printf
import Yesod.Form.Jquery
import Control.Monad
import Data.Maybe
import qualified Data.List as L

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    maximumContentLength _ _ = Just $ 25 * 2^20

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <- widgetToPageContent $ do
            setTitle "Wykop.pl najlepsze śmieszne mp3 z papieżem polakiem xD"
            {-addScriptEither $ urlJqueryJs master-}
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId def, authGoogleEmail]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery App

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

uploadForm :: Form (FileInfo, T.Text, T.Text)
uploadForm = renderBootstrap $ (,,)
    <$> fileAFormReq "file"
    <*> areq textField "Tytuł" Nothing
    <*> areq textField "tagi" Nothing

searchForm :: Form (Maybe T.Text)
searchForm = renderBootstrap $ aopt textField "search" Nothing

hexHash :: Hash -> T.Text
hexHash (Hash x) = decodeUtf8 . encode $ x

instance PathPiece Hash where
    toPathPiece (Hash x) = decodeUtf8 . encode $ x
    fromPathPiece x = if B.null wrong && 20 == B.length dec 
                         then Just $ Hash dec
                         else Nothing
                    where (dec,wrong) = decode . encodeUtf8 $ x

getFileSize :: FilePath -> IO Int
getFileSize x = (fromIntegral . Posix.fileSize) <$> Posix.getFileStatus x

showFileSize :: Int -> T.Text
showFileSize n = T.pack $ printf "%.2g " (x/1024^p) ++ case p of
                    0 -> ""
                    1 -> "KB"
                    2 -> "MB"
                    3 -> "GB"
                where x = fromIntegral n :: Double
                      p = floor $ logBase 1024 x :: Int

audioPlayer :: Hash -> Widget
audioPlayer file = do
    toWidget [whamlet| <audio src=@{FileR file} controls preload=none> |]

fileList :: Maybe [FileId] -> Handler Widget
fileList ids = do
    files <- runDB $ do
        f <- case ids of
                  Nothing -> fmap entTuple <$> selectList [] []
                  Just il -> catMaybes <$> forM il ( \x-> liftM ((,) x) <$> get x)
        forM f $ \(k,v) -> do
            t <- getTags k
            return (v, t)
    return $ toWidget $(whamletFile "templates/filelist.hamlet")
    where getTags f = fmap (map entityVal) $ selectList [TagFileId ==. f] []
          entTuple x = (entityKey x, entityVal x)

normalizeTags :: T.Text -> [T.Text]
-- nub is slow; TODO: better (unicode?) replacing
normalizeTags = L.nub . T.words . T.map replacePL . T.toLower
    where replacePL x = case x of
              'ą' -> 'a'
              'ż' -> 'z'
              'ź' -> 'z'
              'ę' -> 'e'
              'ł' -> 'l'
              'ó' -> 'o'
              'ś' -> 's'
              'ć' -> 'c'
              'ń' -> 'n'
              a -> a
