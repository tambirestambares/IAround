{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Escola where

import Import
import Database.Persist.Postgresql

getEscolaR :: Handler Value
getEscolaR = do
    todasEscolas <- runDB $ selectList [] [Asc EscolaNome]
    sendStatusJSON ok200 (object ["escola" .= todasEscolas])


postEscolaR :: Handler Value
postEscolaR = do 
    escola <- requireJsonBody :: Handler Escola
    eid <- runDB $ insert escola
    sendStatusJSON created201 (object ["escolaId" .= fromSqlKey eid])
    
getEscolaIdR :: EscolaId -> Handler Value
getEscolaIdR eid = do
    escola <- runDB $ get404 eid
    sendStatusJSON ok200 (object ["escola" .= escola])

putEscolaIdR :: EscolaId -> Handler Value
putEscolaIdR eid = do
    _ <- runDB $ get404 eid
    editarEscola <- requireJsonBody :: Handler Escola
    runDB $ replace eid editarEscola
    sendStatusJSON noContent204 (object [])