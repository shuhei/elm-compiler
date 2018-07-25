{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Shader
  ( shader
  )
  where


import qualified Data.ByteString.Internal as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import qualified AST.Source as Src
import qualified AST.Utils.Shader as Shader
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Parse.Primitives (Parser, getPosition, getOffset, getState)
import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Shader as Shader



-- SHADERS


shader :: R.Position -> Parser Src.Expr
shader start@(R.Position row col) =
  do  startState <- getState
      block <- Shader.block
      endOffset <- getOffset
      end@(R.Position row2 col2) <- getPosition
      -- TODO: Rollback to startOffset
      -- setState (startState { _terminal = endOffset })
      -- TODO: Parse only GLSL (set terminal or run parser?)
      shdr <- parse
      let uid = List.intercalate ":" (map show [row, col, row2, col2])
      let src = Text.replace "\n" "\\n" (Text.replace "\r\n" "\\n" block)
      return (A.at start end (Src.Shader (Text.pack uid) src shdr))


data StorageQualifier
  = Attribute
  | Uniform
  | Varying


data GLDeclaration
  = GLDeclaration StorageQualifier Shader.Type N.Name


parse :: Parser Shader.Shader
parse =
  let
    decls =
      -- Dummy value
      [ GLDeclaration Uniform Shader.V2 (N.fromString "vcoord")
      ]
  in
    return (foldr addInput emptyShader decls)


emptyShader :: Shader.Shader
emptyShader =
  Shader.Shader Map.empty Map.empty Map.empty


addInput :: GLDeclaration -> Shader.Shader -> Shader.Shader
addInput (GLDeclaration qual tipe name) glDecls =
  case qual of
    Attribute -> glDecls { Shader._attribute = Map.insert name tipe (Shader._attribute glDecls) }
    Uniform   -> glDecls { Shader._uniform = Map.insert name tipe (Shader._uniform glDecls) }
    Varying   -> glDecls { Shader._varying = Map.insert name tipe (Shader._varying glDecls) }


parserFailure :: Int -> Int -> Int -> Int -> String -> Parser Shader.Shader
parserFailure startRow startCol row col msg =
  if row == 1 then
    Shader.failure startRow (startCol + 6 + col) (Text.pack msg)
  else
    Shader.failure (startRow + row - 1) col (Text.pack msg)
