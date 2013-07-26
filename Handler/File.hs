module Handler.File where

import Import
import System.Directory
import System.FilePath
import qualified Data.Text as T

getFileR :: Hash -> Handler Html
getFileR h = do
    f <- runDB . fmap entityVal . getBy404 .UniqueHash$ h
    let path = "store/" </> (T.unpack . hexHash . fileHash) f
    exists <- liftIO . doesFileExist $ path
    if exists then
        sendFile (fileMime f) path
        else error "file in db, but not on disk"
