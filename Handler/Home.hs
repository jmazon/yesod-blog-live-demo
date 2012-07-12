{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Form.Nic

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

postCreateForm :: Form Post
postCreateForm = renderDivs $ Post
    <$> areq textField "Post title" Nothing
    <*> areq nicHtmlField "Post content" Nothing

getBlogR :: Handler RepHtml
getBlogR = do
  posts <- runDB (selectList [] [])
  (widget,enctype) <- generateFormPost postCreateForm
  defaultLayout $(widgetFile "blog")

postBlogR :: Handler RepHtml
postBlogR = do
  ((result,widget),enctype) <- runFormPost postCreateForm
  case result of
    FormSuccess post -> do
      postId <- runDB $ insert post
      redirect (PostR postId)

getPostR :: PostId -> Handler RepHtml
getPostR id = do
  post <- runDB (get404 id)
  defaultLayout $(widgetFile "post")
