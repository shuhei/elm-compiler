{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings, UnboxedTuples #-}
module Parse.Shader
  ( shader
  )
  where


import Control.Exception (assert)
import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Internal as B
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8)

import qualified AST.Source as Src
import qualified AST.Utils.Shader as Shader
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R
import Parse.Primitives as PP
import Parse.Primitives (Parser, getPosition)
import Parse.Primitives.Internals (Parser(..), State(..), noError)
import Parse.Primitives.Keyword as Keyword
import Parse.Primitives.Variable (chompInnerChars)
import Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Shader as Shader
import qualified Parse.Primitives.Variable as Var



-- SHADERS


shader :: R.Position -> Parser Src.Expr
shader start@(R.Position row col) =
  do  block <- Shader.block
      end@(R.Position row2 col2) <- getPosition
      shdr <- parseSource row (col + 6) parse block
      let uid = List.intercalate ":" (map show [row, col, row2, col2])
      let src = Text.replace "\n" "\\n" (Text.replace "\r\n" "\\n" (Text.decodeUtf8 block))
      return (A.at start end (Src.Shader (Text.pack uid) src shdr))


data StorageQualifier
  = Attribute
  | Uniform
  | Varying


data GLDeclaration
  = VariableDeclaration StorageQualifier Shader.Type N.Name
  | SomethingElse


parseSource :: Int -> Int -> Parser a -> B.ByteString -> Parser a
parseSource startRow startCol (Parser parser) (B.PS fp offset length) =
  case parser (State fp offset (offset + length) 0 1 1 []) Ok Err Ok Err of
    Ok value _ _ ->
      return value
    Err (E.ParseError row col problem) ->
      if row == 1 then
        shaderFailure startRow (startCol + col - 1) problem
      else
        shaderFailure (startRow + row - 1) col problem


-- TODO: Use E.BadShader?
shaderFailure :: Int -> Int -> E.Problem -> Parser a
shaderFailure row col problem =
  Parser $ \_ _ cerr _ _ ->
    cerr (E.ParseError row col problem)


data Result a
  = Ok a State E.ParseError
  | Err E.ParseError


parse :: Parser Shader.Shader
parse =
  do  whitespace
      -- TODO: Repeat declaration
      decl <- declaration
      return (foldr addInput emptyShader [decl])


emptyShader :: Shader.Shader
emptyShader =
  Shader.Shader Map.empty Map.empty Map.empty


addInput :: GLDeclaration -> Shader.Shader -> Shader.Shader
addInput (VariableDeclaration qual tipe name) glDecls =
  case qual of
    Attribute -> glDecls { Shader._attribute = Map.insert name tipe (Shader._attribute glDecls) }
    Uniform   -> glDecls { Shader._uniform = Map.insert name tipe (Shader._uniform glDecls) }
    Varying   -> glDecls { Shader._varying = Map.insert name tipe (Shader._varying glDecls) }
addInput SomethingElse glDecls =
  glDecls



-- PARSER


declaration :: Parser GLDeclaration
declaration =
  PP.oneOf
    [ variableDeclaration
    -- , somethingElse
    ]


-- Start with whitespace and end with a semicolon.
-- Quantifier, type and name.
-- Precision?
variableDeclaration :: Parser GLDeclaration
variableDeclaration =
    do  qual <- storageQualifier
        -- TODO: Make whitespace required
        whitespace
        tipe <- typeName
        whitespace
        name <- identifier
        whitespace
        Symbol.semicolon
        whitespace
        return $ VariableDeclaration qual tipe name


storageQualifier :: Parser StorageQualifier
storageQualifier =
  PP.oneOf
    [ Keyword.varying >> return Varying
    , Keyword.uniform >> return Uniform
    , Keyword.attribute >> return Attribute
    ]


-- Not `{`, not `semicolon`
somethingElse :: Parser GLDeclaration
somethingElse =
  do  eatSomethingElse
      PP.oneOf
        [ Symbol.semicolon
        -- , curlyPair
        ]
      return SomethingElse

-- Eat until {, } or ;
eatSomethingElse :: Parser ()
eatSomethingElse =
  undefined


bracket :: Int -> Parser ()
bracket depth =
  undefined



-- WHITESPACE


whitespace :: Parser ()
whitespace =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
    case eatSpaces fp offset terminal row col of
      Left err ->
        cerr err

      Right (newOffset, newTerminal, newRow, newCol) ->
        cok
          ()
          (State fp newOffset newTerminal indent newRow newCol ctx)
          noError


eatSpaces :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Either E.ParseError ( Int, Int, Int, Int )
eatSpaces fp offset terminal row col =
  if offset >= terminal then
    Right ( offset, terminal, row, col )

  else
    case I.unsafeIndex fp offset of
      0x0020 {-   -} ->
        eatSpaces fp (offset + 1) terminal row (col + 1)

      0x0009 {- \t -} ->
        eatSpaces fp (offset + 1) terminal row (col + 1)

      0x000A {- \n -} ->
        eatSpaces fp (offset + 1) terminal (row + 1) 1

      0x002F {- / -} ->
        eatComment fp offset terminal row col

      0x000D {- \r -} ->
        eatSpaces fp (offset + 1) terminal row col

      _ ->
        Right ( offset, terminal, row, col )


-- LINE COMMENTS


eatComment :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Either E.ParseError ( Int, Int, Int, Int )
eatComment fp offset terminal row col =
  if offset + 1 >= terminal then
    Right ( offset, terminal, row, col )

  else
    case I.unsafeIndex fp (offset + 1) of
      0x2F {- / -} ->
        eatLineComment fp (offset + 2) terminal row (col + 2)

      0x2A {- * -} ->
        do  (newOffset, newTerminal, newRow, newCol) <-
              eatMultiComment fp (offset + 2) terminal row (col + 2) 1
            eatSpaces fp newOffset newTerminal newRow newCol

      _ ->
        Right ( offset, terminal, row, col )


eatLineComment :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Either E.ParseError ( Int, Int, Int, Int )
eatLineComment fp offset terminal row col =
  if offset >= terminal then
    Right ( offset, terminal, row, col )

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatSpaces fp (offset + 1) terminal (row + 1) 1

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      eatLineComment fp newOffset terminal row (col + 1)



-- MULTI COMMENTS


eatMultiComment :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> Either E.ParseError ( Int, Int, Int, Int )
eatMultiComment fp offset terminal row col openComments =
  if offset >= terminal then
    Left (E.ParseError row col E.EndOfFile_Comment)

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatMultiComment fp (offset + 1) terminal (row + 1) 1 openComments

    else if word == 0x2A {- * -} && I.isWord fp (offset + 1) terminal 0x2F {- / -} then
      if openComments == 1 then
        Right ( offset + 2, terminal, row, col + 2 )
      else
        eatMultiComment fp (offset + 2) terminal row (col + 2) (openComments - 1)

    else if word == 0x2F {- / -} && I.isWord fp (offset + 1) terminal 0x2A {- * -} then
      eatMultiComment fp (offset + 2) terminal row (col + 2) (openComments + 1)

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      eatMultiComment fp newOffset terminal row (col + 1) openComments



-- TYPE NAME


typeName :: Parser Shader.Type
typeName =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompNondigit fp offset terminal col in
    if offset == newOffset then
      eerr (expect row col ctx [E.LowVar, E.CapVar, E.Symbol "_"])
    else
      let !name = N.fromForeignPtr fp offset (newOffset - offset) in
      case typeFromName name of
        Just tipe -> cok tipe (State fp newOffset terminal indent row newCol ctx) noError
        Nothing -> eerr (expect row col ctx [E.Expecting E.Type])


typeFromName :: N.Name -> Maybe Shader.Type
typeFromName name =
  case N.toString name of
    "int" -> Just Shader.Int
    "float" -> Just Shader.Float
    "vec2" -> Just Shader.V2
    "vec3" -> Just Shader.V3
    "vec4" -> Just Shader.V4
    "mat4" -> Just Shader.M4
    "sampler2D" -> Just Shader.Texture
    _ -> Nothing



-- IDENTIFIER


{-# INLINE expect #-}
expect :: Int -> Int -> E.ContextStack -> [E.Theory] -> E.ParseError
expect row col ctx theories =
  E.ParseError row col (E.Theories ctx theories)


-- Based on Parse.Primitives.Variable
-- TODO: Check keywords and reserved words
identifier :: Parser N.Name
identifier =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompNondigit fp offset terminal col in
    if offset == newOffset then
      eerr (expect row col ctx [E.LowVar, E.CapVar, E.Symbol "_"])
    else
      let !name = N.fromForeignPtr fp offset (newOffset - offset) in
      cok name (State fp newOffset terminal indent row newCol ctx) noError


chompNondigit :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int #)
chompNondigit fp offset terminal col =
  let !width = getNondigitWidth fp offset terminal in
  if width == 0 then
    (# offset, col #)
  else
    chompInnerChars fp (offset + width) terminal (col + 1)


{-# INLINE getNondigitWidth #-}
getNondigitWidth :: ForeignPtr Word8 -> Int -> Int -> Int
getNondigitWidth fp offset terminal =
  if offset < terminal then
    getNondigitWidthHelp fp offset terminal (I.unsafeIndex fp offset)
  else
    0


{-# INLINE getNondigitWidthHelp #-}
getNondigitWidthHelp :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getNondigitWidthHelp fp offset terminal word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | word == 0x5F {- _ -} = 1
  | word < 0xc0 = 0
  -- TODO: Why are these necessary?
  | word < 0xe0 = if Char.isAlpha (getChar2 fp offset terminal word) then 2 else 0
  | word < 0xf0 = if Char.isAlpha (getChar3 fp offset terminal word) then 3 else 0
  | word < 0xf8 = if Char.isAlpha (getChar4 fp offset terminal word) then 4 else 0
  | True        = 0



-- EXTRACT CHARACTERS


push :: Word8 -> Int -> Int
push word code =
  assert (word .&. 0xc0 == 0x80) (
    shiftL code 6 .|. fromEnum (word .&. 0x3f)
  )


getChar2 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar2 fp offset terminal word =
  assert (offset + 2 <= terminal) (
    let
      !word1 = word .&. 0x1f
      !word2 = I.unsafeIndex fp (offset + 1)
      !code = push word2 (fromEnum word1)
    in
    assert (0x80 <= code) (
      toEnum code
    )
  )


getChar3 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar3 fp offset terminal word =
  assert (offset + 3 <= terminal) (
    let
      !word1 = word .&. 0x0f
      !word2 = I.unsafeIndex fp (offset + 1)
      !word3 = I.unsafeIndex fp (offset + 2)
      !code = push word3 (push word2 (fromEnum word1))
    in
    assert ((0x800 <= code && code < 0xd800) || (0xdfff < code && code < 0xfffe)) (
      toEnum code
    )
  )


getChar4 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar4 fp offset terminal word =
  assert (offset + 4 <= terminal) (
    let
      !word1 = word .&. 0x07
      !word2 = I.unsafeIndex fp (offset + 1)
      !word3 = I.unsafeIndex fp (offset + 2)
      !word4 = I.unsafeIndex fp (offset + 3)
      !code = push word4 (push word3 (push word2 (fromEnum word1)))
    in
    assert (0x10000 <= code && code < 0x110000) (
      toEnum code
    )
  )
