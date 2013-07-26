{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    filesWidget <- fileList Nothing
    ((_,sform), enctype) <- generateFormGet searchForm
    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "homepage")
    where getTags f = fmap (map entityVal) $ selectList [TagFileId ==. entityKey f] []

