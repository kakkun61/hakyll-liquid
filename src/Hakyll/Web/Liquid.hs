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

-- | Parse underlying item and compile it with its metadata as context.
parseAndInterpretDefault :: Compiler (Item String)
parseAndInterpretDefault = do
  metadata <- getMetadata =<< getUnderlying
  item <- getResourceBody
  parse item >>= interpret metadata

-- | Parse underlying item and compile it with given metadata as context.
parseAndInterpret :: Metadata -> Compiler (Item String)
parseAndInterpret metadata = getResourceBody >>= parse >>= interpret metadata

-- | Parse underlying item and compile it with given context.
parseAndInterpret' :: Aeson.Value -> Compiler (Item String)
parseAndInterpret' context = getResourceBody >>= parse >>= interpret' context

-- | Parse given item.
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

-- | Compile Liquid expressions with given metadata as context.
interpret :: Metadata -> Item [Liquid.Expr] -> Compiler (Item String)
interpret metadata = interpret' (Aeson.Object metadata)

-- | Compile Liquid expressions with given context.
interpret' :: Aeson.Value -> Item [Liquid.Expr] -> Compiler (Item String)
interpret' context (Item identifier expressions) =
  case Liquid.interpret context expressions of
    Validation.AccSuccess text -> return $ Item identifier $ Text.unpack text
    Validation.AccFailure err -> throwError [show err]
