{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Voluntario where

import Import
import Database.Persist.Postgresql

patchVoluntarioNomeR :: VoluntarioId -> Text -> Handler Value
patchVoluntarioNomeR vid nome = do
    _ <- runDB $ get404 vid
    runDB $ update vid [VoluntarioNome =. nome]
    sendStatusJSON noContent204 (object [])

putVoluntarioIdR :: VoluntarioId -> Handler Value
putVoluntarioIdR vid = do
    _ <- runDB $ get404 vid
    novoVoluntario <- requireJsonBody :: Handler Voluntario
    runDB $ replace vid novoVoluntario
    sendStatusJSON noContent204 (object [])

deleteVoluntarioIdR :: VoluntarioId -> Handler Value
deleteVoluntarioIdR vid = do
    _ <- runDB $ get404 vid
    runDB $ delete vid
    sendStatusJSON noContent204 (object [])

getVoluntarioR :: Handler Value
getVoluntarioR = do
    todosVoluntarios <- runDB $ selectList [] [Asc VoluntarioNome]
    sendStatusJSON ok200 (object ["resp" .= todosVoluntarios])

-- getVoluntarioEmailR :: Text -> Handler Value
-- getVoluntarioEmailR email = do
    -- voluntario <- runDB $ getBy $ UniqueEmail email
    -- case voluntario of
    -- 	Just (Entity _ colunaVoluntario) -> sendStatusJSON ok200 (object ["nome" .= VoluntarioNome colunaVoluntario])
    -- 	Nothing -> 	sendStatusJSON notFound404 (object [])

getVoluntarioIdR :: VoluntarioId -> Handler Value
getVoluntarioIdR vid = do 
    volunt <- runDB $ get404 vid
    sendStatusJSON ok200 (object ["resp" .= volunt])

postVoluntarioR :: Handler Value
postVoluntarioR = do
    voluntario <- requireJsonBody :: Handler Voluntario
    vid <- runDB $ insert voluntario
    sendStatusJSON created201 (object ["resp" .= fromSqlKey vid])