module Handler.Upload where

import Import
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Crypto.Hash.SHA1 as H
import System.FilePath
import Data.Time.Clock
import Prelude (tail)
import System.Random
import System.Directory
import Data.Text.Encoding
import Control.Monad

postUploadR :: Handler Html
postUploadR = do
    ((formResult, formWidget), enctype) <- runFormPost uploadForm
    case formResult of
	 FormSuccess (f, title, tag_s) -> do
             rand <- liftIO $ fmap show (randomRIO (0, 2^(31 :: Int))::IO Int)
             let tags = normalizeTags tag_s
                 ext = tail.takeExtension.T.unpack.fileName $ f
                 tmp_path = "store/tmp/" </> rand
                 mime = encodeUtf8 $ fileContentType f
             allowedMime <- fmap extraMimeTypes getExtra 
             when (mime `notElem` allowedMime) $ do
                  setMessage . toHtml $ "invalid mime type: " `T.append` fileContentType f 
                  tt <- defaultLayout $(widgetFile "upload")
                  sendResponse tt
             liftIO $ fileMove f tmp_path
             hash <- liftIO . fmap (Hash . H.hash) . B.readFile $ tmp_path
             time <- liftIO getCurrentTime
             size <- liftIO $ getFileSize tmp_path
             let f_db = File title hash ext mime time size
             result <- runDB $ do
                 r <- insertBy f_db 
                 case r of
                      Right key -> do
                          forM_ tags $ \t -> insert $ Tag t key -- if this fails, tmp file won't be deleted
                          liftIO $ renameFile tmp_path $ "store/" </> (T.unpack . hexHash) hash
                          return [whamlet| hash #{hexHash hash} |]
                      Left _ -> do
                          liftIO $ removeFile tmp_path
                          setMessage "file exists" 
                          return $(widgetFile "upload")
             defaultLayout result
	 _ -> defaultLayout $ do
             setMessage "upload failed"
             $(widgetFile "upload")

getUploadR :: Handler Html
getUploadR = do
    (formWidget, enctype) <- generateFormPost uploadForm
    defaultLayout $(widgetFile "upload")
