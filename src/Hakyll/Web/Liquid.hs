{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Liquid
  ( parseAndInterpretDefault
  , parseAndInterpret
  , parseAndInterpret'
  , parse
  , interpret
  , interpret'
  ) where

import Control.Monad.Error.Class
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Validation as Validation
import Hakyll
import Hakyll.Core.Compiler
import qualified Text.Liquid as Liquid

parseAndInterpretDefault :: Compiler (Item String)
parseAndInterpretDefault = do
  metadata <- getMetadata =<< getUnderlying
  item <- getResourceBody
  parse item >>= interpret metadata

parseAndInterpret :: Metadata -> Compiler (Item String)
parseAndInterpret metadata = getResourceBody >>= parse >>= interpret metadata

parseAndInterpret' :: Aeson.Value -> Compiler (Item String)
parseAndInterpret' context = getResourceBody >>= parse >>= interpret' context

parse :: Item String -> Compiler (Item [Liquid.Expr])
parse (Item identifier body) =
  case Liquid.parseTemplate (Text.pack body) of
    Attoparsec.Done _ exprs -> return $ Item identifier exprs
    Attoparsec.Partial parser ->
      case parser "" of
        Attoparsec.Done _ exprs -> return $ Item identifier exprs
        Attoparsec.Partial _ -> throwError ["unexpected end of input"]
        Attoparsec.Fail _ ctx msg -> throwError $ msg:ctx
    Attoparsec.Fail _ ctx msg -> throwError $ msg:ctx

interpret :: Metadata -> Item [Liquid.Expr] -> Compiler (Item String)
interpret metadata = interpret' (Aeson.Object metadata)

interpret' :: Aeson.Value -> Item [Liquid.Expr] -> Compiler (Item String)
interpret' context (Item identifier expressions) =
  case Liquid.interpret context expressions of
    Validation.AccSuccess text -> return $ Item identifier $ Text.unpack text
    Validation.AccFailure err -> throwError [show err]
