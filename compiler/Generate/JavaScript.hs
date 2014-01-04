{-# OPTIONS_GHC -W #-}
module Generate.JavaScript (generate) where

import Control.Arrow (first,(***))
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Generate.Cases as Case
import qualified Generate.Markdown as MD
import qualified SourceSyntax.Helpers as Help
import SourceSyntax.Literal
import SourceSyntax.Pattern as Pattern
import SourceSyntax.Location
import SourceSyntax.Expression
import SourceSyntax.Module
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import qualified Transform.SafeNames as MakeSafe

split :: String -> [String]
split = go []
  where
    go vars str =
        case break (=='.') str of
          (x,_:rest) | Help.isOp x -> vars ++ [x ++ '.' : rest]
                     | otherwise -> go (vars ++ [x]) rest
          (x,[]) -> vars ++ [x]

var name = Id () name
ref name = VarRef () (var name)
prop name = PropId () (var name)
f <| x = CallExpr () f [x]
args ==> e = FuncExpr () Nothing (map var args) [ ReturnStmt () (Just e) ]
function args stmts = FuncExpr () Nothing (map var args) stmts
call = CallExpr ()
string = StringLit ()

dotSep vars =
    case vars of
      x:xs -> foldl (DotRef ()) (ref x) (map var xs)
      [] -> error "dotSep must be called on a non-empty list of variables"

obj = dotSep . split

varDecl :: String -> Expression () -> VarDecl ()
varDecl x expr =
    VarDecl () (var x) (Just expr)

include alias moduleName =
    varDecl alias (obj (moduleName ++ ".make") <| ref "_elm")

internalImports name =
    VarDeclStmt () 
    [ varDecl "N" (obj "Elm.Native")
    , include "_N" "N.Utils"
    , include "_L" "N.List"
    , include "_E" "N.Error"
    , include "_J" "N.JavaScript"
    , varDecl "$moduleName" (string name)
    ]

literal :: Literal -> Expression ()
literal lit =
  case lit of
    Chr c -> obj "_N.chr" <| string [c]
    Str s -> string s
    IntNum   n -> IntLit () n
    FloatNum n -> NumLit () n
    Boolean  b -> BoolLit () b

expression :: LExpr -> State Int (Expression ())
expression (L span expr) =
    case expr of
      Var x -> return $ ref x
      Literal lit -> return $ literal lit

      Range lo hi ->
          do lo' <- expression lo
             hi' <- expression hi
             return $ obj "_L.range" `call` [lo',hi']

      Access e x ->
          do e' <- expression e
             return $ DotRef () e' (var x)

      Remove e x ->
          do e' <- expression e
             return $ obj "_N.remove" `call` [string x, e']

      Insert e x v ->
          do v' <- expression v
             e' <- expression e
             return $ obj "_N.insert" `call` [string x, v', e']

      Modify e fs ->
          do e' <- expression e
             fs' <- forM fs $ \(f,v) -> do
                      v' <- expression v
                      return $ ArrayLit () [string f, v']
             return $ obj "_N.replace" `call` [ArrayLit () fs', e']

      Record fields ->
          do fields' <- forM fields $ \(f,e) -> do
                          (,) f <$> expression e
             let fieldMap = List.foldl' combine Map.empty fields'
             return $ ObjectLit () $ (PropId () (var "_"), hidden fieldMap) : visible fieldMap
          where
            combine r (k,v) = Map.insertWith (++) k [v] r
            prop = PropId () . var
            hidden fs = ObjectLit () . map (prop *** ArrayLit ()) .
                        Map.toList . Map.filter (not . null) $ Map.map tail fs
            visible fs = map (first prop) . Map.toList $ Map.map head fs

      Binop op e1 e2 -> binop span op e1 e2

      Lambda p e@(L s _) ->
          do (args, body) <- foldM depattern ([], innerBody) (reverse patterns)
             body' <- expression body
             return $ case length args < 2 || length args > 9 of
                        True  -> foldr (==>) body' (map (:[]) args)
                        False -> ref ("F" ++ show (length args)) <| (args ==> body')
          where
            depattern (args, body) pattern =
                case pattern of
                  PVar x -> return (args ++ [x], body)
                  _ -> do arg <- Case.newVar
                          return (args ++ [arg], L s (Case (L s (Var arg)) [(pattern, body)]))

            (patterns, innerBody) = collect [p] e

            collect patterns lexpr@(L _ expr) =
                case expr of
                  Lambda p e -> collect (p:patterns) e
                  _ -> (patterns, lexpr)

      App e1 e2 ->
          do func' <- expression func
             args' <- mapM expression args
             return $ case args' of
                        [arg] -> func' <| arg
                        _ | length args' <= 9 -> ref aN `call` (func':args')
                          | otherwise         -> foldl1 (<|) (func':args')
          where
            aN = "A" ++ show (length args)
            (func, args) = getArgs e1 [e2]
            getArgs func args =
                case func of
                  (L _ (App f arg)) -> getArgs f (arg : args)
                  _ -> (func, args)

      Let defs e ->
          do let (defs',e') = flattenLets defs e 
             stmts <- concat <$> mapM definition defs'
             exp <- expression e'
             return $ function [] (stmts ++ [ ReturnStmt () (Just exp) ]) `call` []

      MultiIf branches ->
        do branches' <- forM branches $ \(b,e) -> (,) <$> expression b <*> expression e
           return $ case last branches of
             (L _ (Var "Basics.otherwise"), _) -> safeIfs branches'
             (L _ (Literal (Boolean True)), _) -> safeIfs branches'
             _ -> ifs branches' (obj "_E.If" `call` [ ref "$moduleName", string (show span) ])
        where
          safeIfs branches = ifs (init branches) (snd (last branches))
          ifs branches finally = foldr iff finally branches
          iff (if', then') else' = CondExpr () if' then' else'

      Case e cases ->
          do (tempVar,initialMatch) <- Case.toMatch cases
             (revisedMatch, stmt) <-
                 case e of
                   L _ (Var x) -> return (Case.matchSubst [(tempVar,x)] initialMatch, [])
                   _ -> do e' <- expression e
                           return (initialMatch, [VarDeclStmt () [varDecl tempVar e']])
             match' <- match span revisedMatch
             return (function [] (stmt ++ match') `call` [])

      ExplicitList es ->
          do es' <- mapM expression es
             return $ obj "_J.toList" <| ArrayLit () es'

      Data name es ->
          do es' <- mapM expression es
             return $ ObjectLit () (ctor : fields es')
          where
            ctor = (prop "ctor", string name)
            fields = zipWith (\n e -> (prop ("_" ++ show n), e)) [0..]

      Markdown uid doc es ->
          do es' <- mapM expression es
             return $ obj "Text.markdown" `call` (string md : string uid : es')
          where
            pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
            md = pad ++ MD.toHtml doc ++ pad

      PortIn name _ _ handler ->
          do handler' <- expression handler
             return $ obj "_N.portIn" `call` [ string name, handler' ]

      PortOut name _ signal ->
          do signal' <- expression signal
             return $ obj "_N.portOut" `call` [ string name, signal' ]

definition :: Def -> State Int [Statement ()]
definition (Definition pattern expr@(L span _) _) = do
  expr' <- expression expr
  let assign x = varDecl x expr'
  case pattern of
    PVar x
        | Help.isOp x ->
            let op = LBracket () (ref "_op") (string x) in
            return [ ExprStmt () $ AssignExpr () OpAssign op expr' ]
        | otherwise ->
            return [ VarDeclStmt () [ assign x ] ]

    PRecord fields ->
        let setField f = varDecl f (dotSep ["$",f]) in
        return [ VarDeclStmt () (assign "$" : map setField fields) ]

    PData name patterns | vars /= Nothing ->
        return [ VarDeclStmt () (setup (zipWith decl (maybe [] id vars) [0..])) ]
        where
          vars = getVars patterns
          getVars patterns =
              case patterns of
                PVar x : rest -> (x:) `fmap` getVars rest
                [] -> Just []
                _ -> Nothing

          decl x n = varDecl x (dotSep ["$","_" ++ show n])
          setup vars
              | Help.isTuple name = assign "$" : vars
              | otherwise = assign "$raw" : safeAssign : vars

          safeAssign = varDecl "$" (CondExpr () if' (obj "$raw") exception)
          if' = InfixExpr () OpStrictEq (obj "$raw.ctor") (string name)
          exception = obj "_E.Case" `call` [ref "$moduleName", string (show span)]

    _ ->
        do defs' <- concat <$> mapM toDef vars
           return (VarDeclStmt () [assign "$"] : defs')
        where
          vars = Set.toList $ Pattern.boundVars pattern
          mkVar = L span . Var
          toDef y = let expr =  L span $ Case (mkVar "$") [(pattern, mkVar y)]
                    in  definition $ Definition (PVar y) expr Nothing

match :: (Show a) => a -> Case.Match -> State Int [Statement ()]
match span mtch =
  case mtch of
    Case.Match name clauses mtch' ->
        do (isChars, clauses') <- unzip <$> mapM (clause span name) clauses
           mtch'' <- match span mtch'
           return (SwitchStmt () (format isChars (access name)) clauses' : mtch'')
        where
          isLiteral p = case p of
                          Case.Clause (Right _) _ _ -> True
                          _ -> False
          access name = if any isLiteral clauses then ref name else dotSep [name,"ctor"]
          format isChars e
              | or isChars = InfixExpr () OpAdd e (string "")
              | otherwise = e

    Case.Fail ->
        return [ ExprStmt () (obj "_E.Case" `call` [ref "$moduleName", string (show span)]) ]

    Case.Break -> return [BreakStmt () Nothing]
    Case.Other e ->
        do e' <- expression e
           return [ ReturnStmt () (Just e') ]
    Case.Seq ms -> concat <$> mapM (match span) (dropEnd [] ms)
        where
          dropEnd acc [] = acc
          dropEnd acc (m:ms) =
              case m of
                Case.Other _ -> acc ++ [m]
                _ -> dropEnd (acc ++ [m]) ms

clause span variable (Case.Clause value vars mtch) =
    (,) isChar . CaseClause () pattern <$> match span (Case.matchSubst (zip vars vars') mtch)
  where
    vars' = map (\n -> variable ++ "._" ++ show n) [0..]
    (isChar, pattern) =
        case value of
          Right (Chr c) -> (True, string [c])
          _ -> (,) False $ case value of
                             Right (Boolean b) -> BoolLit () b
                             Right lit -> literal lit
                             Left name -> string $ case List.elemIndices '.' name of
                                                     [] -> name
                                                     is -> drop (last is + 1) name

flattenLets defs lexpr@(L _ expr) =
    case expr of
      Let ds body -> flattenLets (defs ++ ds) body
      _ -> (defs, lexpr)

generate :: MetadataModule -> String 
generate unsafeModule =
    show . prettyPrint $ setup (Just "Elm") (names modul ++ ["make"]) ++
             [ assign ("Elm" : names modul ++ ["make"]) (function ["_elm"] programStmts) ]
  where
    modul = MakeSafe.metadataModule unsafeModule
    thisModule = dotSep ("_elm" : names modul ++ ["values"])
    programStmts =
        concat [ setup (Just "_elm") (names modul ++ ["values"])
               , [ IfSingleStmt () thisModule (ReturnStmt () (Just thisModule)) ]
               , [ internalImports (List.intercalate "." (names modul)) ]
               , concatMap jsImport (imports modul)
               , checkInPorts (recvPorts modul)
               , [ assign ["_op"] (ObjectLit () []) ]
               , concat $ evalState (mapM definition . fst . flattenLets [] $ program modul) 0
               , [ jsExports ]
               , [ ReturnStmt () (Just thisModule) ]
               ]

    jsExports = assign ("_elm" : names modul ++ ["values"]) (ObjectLit () exs)
        where
          exs = map entry . filter (not . Help.isOp) $ "_op" : exports modul
          entry x = (PropId () (var x), ref x)
          
    assign path expr =
             case path of
               [x] -> VarDeclStmt () [ varDecl x expr ]
               _   -> ExprStmt () $
                      AssignExpr () OpAssign (LDot () (dotSep (init path)) (last path)) expr

    jsImport (modul,_) = setup Nothing path ++ [ include ]
        where
          path = split modul
          include = assign path $ dotSep ("Elm" : path ++ ["make"]) <| ref "_elm"

    setup namespace path = map create paths
        where
          create name = assign name (InfixExpr () OpLOr (dotSep name) (ObjectLit () []))
          paths = case namespace of
                    Nothing -> tail . init $ List.inits path
                    Just nmspc -> drop 2 . init . List.inits $ nmspc : path

    checkInPorts ports =
        [ ExprStmt () $ obj "_N.portCheck" `call` [ref "$moduleName", names] ]
        where
          names = ArrayLit () [ string name | (name, _, _) <- ports ]

binop span op e1 e2 =
    case op of
      "Basics.." ->
          do es <- mapM expression (e1 : collect [] e2)
             return $ ["$"] ==> foldr (<|) (ref "$") es
      "Basics.<|" ->
          do e2' <- expression e2
             es <- mapM expression (collect [] e1)
             return $ foldr (<|) e2' es
      "List.++" ->
          do e1' <- expression e1
             e2' <- expression e2
             return $ obj "_L.append" `call` [e1', e2']
      "::" -> expression (L span (Data "::" [e1,e2]))
      _ ->
          do e1' <- expression e1
             e2' <- expression e2
             return $ case Map.lookup op opDict of
                        Just f -> f e1' e2'
                        Nothing -> ref "A2" `call` [ func, e1', e2' ]
  where
    collect es e =
        case e of
          L _ (Binop op e1 e2) | op == "Basics.." -> collect (es ++ [e1]) e2
          _ -> es ++ [e]

    func | Help.isOp operator = BracketRef () (dotSep (init parts ++ ["_op"])) (string operator)
         | otherwise     = dotSep parts
        where
          parts = split op
          operator = last parts

    opDict = Map.fromList (infixOps ++ specialOps)

    specialOp str func = [ (str, func), ("Basics." ++ str, func) ]
    infixOp str op = specialOp str (InfixExpr () op)

    infixOps = concat
        [ infixOp "+"  OpAdd
        , infixOp "-"  OpSub
        , infixOp "*"  OpMul
        , infixOp "/"  OpDiv
        , infixOp "&&" OpLAnd
        , infixOp "||" OpLOr
        ]

    specialOps = concat
        [ specialOp "^"   $ \a b -> obj "Math.pow" `call` [a,b]
        , specialOp "|>"  $ flip (<|)
        , specialOp "=="  $ \a b -> obj "_N.eq" `call` [a,b]
        , specialOp "/="  $ \a b -> PrefixExpr () PrefixLNot (obj "_N.eq" `call` [a,b])
        , specialOp "<"   $ cmp OpLT 0
        , specialOp ">"   $ cmp OpGT 0
        , specialOp "<="  $ cmp OpLT 1
        , specialOp ">="  $ cmp OpGT (-1)
        , specialOp "div" $ \a b -> InfixExpr () OpBOr (InfixExpr () OpDiv a b) (IntLit () 0)
        ]

    cmp op n a b = InfixExpr () op (obj "_N.cmp" `call` [a,b]) (IntLit () n)
