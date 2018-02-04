{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Liquid
  ( liquidCompiler
  , renderLiquid
  ) where

import Control.Monad.Error.Class
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Validation as Validation
import Hakyll
import Hakyll.Core.Compiler
import Text.Liquid

liquidCompiler :: Aeson.Value -> Compiler (Item String)
liquidCompiler context = getResourceBody >>= renderLiquid context

renderLiquid :: Aeson.Value -> Item String -> Compiler (Item String)
renderLiquid context item = do
  exprs <-
    case parseTemplate (Text.pack $ itemBody item) of
      Attoparsec.Done _ exprs -> return exprs
      Attoparsec.Partial parser ->
        case parser "" of
          Attoparsec.Done _ exprs -> return exprs
          Attoparsec.Partial _ -> throwError ["unexpected end of input"]
          Attoparsec.Fail _ ctx msg -> throwError $ msg:ctx
      Attoparsec.Fail _ ctx msg -> throwError $ msg:ctx
  metadata <- getMetadata =<< getUnderlying
  case interpret (Aeson.Object metadata) exprs of
    Validation.AccSuccess text -> makeItem $ Text.unpack text
    Validation.AccFailure err -> throwError [show err]
