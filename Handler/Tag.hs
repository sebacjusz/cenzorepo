module Handler.Tag where

import Import
import qualified Data.Text as T
import Database.Persist.Sql
import Data.List
import Control.Monad

getTagListR :: Handler Html
getTagListR = do
    ((query, form), enctype) <- runFormGet searchForm
    case query of
        FormSuccess (Just q) -> do
            let tq = normalizeTags q
            when (length tq > 10) $ error "sage"
            il <- forM tq $ \name ->
                (fmap.fmap) (tagFileId . entityVal) . runDB $ selectList [TagName ==. name] []
            filesWidget <- fileList (Just $ foldl1 intersect il)
            defaultLayout [whamlet|
                <form method=get action=@{TagListR} enctype=#{enctype}>
                    ^{form}
                ^{filesWidget} |]
        _ -> do
            _t <- runDB $ rawSql "select name, count(name) from tag group by name" []
            let t = fmap (\(a,b) -> (unSingle a, unSingle b)) _t :: [(T.Text, Int)]
            defaultLayout $ [whamlet|
                <form method=get action=@{TagListR} enctype=#{enctype}>
                    ^{form}
                <ul>
                    $forall (n, c) <- t
                        <li>
                            <a href="@{TagR n}"> #{n}: </a> #{c}  |]
                
getTagR :: T.Text -> Handler Html
getTagR name = do
    ids <- (fmap.fmap) (tagFileId . entityVal) . runDB $ selectList [TagName ==. name] []
    filesWidget <- fileList (Just ids)
    defaultLayout filesWidget
