{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Curso where

import Import
import Database.Persist.Postgresql

getCursoR :: Handler Value
getCursoR = do
    todosCursos <- runDB $ selectList [] [Asc CursoNome]
    sendStatusJSON ok200 (object ["curso" .= todosCursos])


postCursoR :: Handler Value
postCursoR = do 
    curso <- requireJsonBody :: Handler Curso
    cid <- runDB $ insert curso
    sendStatusJSON created201 (object ["cursoId" .= fromSqlKey cid])
    
getCursoIdR :: CursoId -> Handler Value
getCursoIdR cid = do
    curso <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["curso" .= curso])

putCursoIdR :: CursoId -> Handler Value
putCursoIdR cid = do
    _ <- runDB $ get404 cid
    editarCurso <- requireJsonBody :: Handler Curso
    runDB $ replace cid editarCurso
    sendStatusJSON noContent204 (object [])