
module InBound.Parser where

import Control.Applicative
import Control.Monad

import Data.Either
import Data.Set (Set)
import qualified Data.Set as S

import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

import InBound.Syntax

inboundDef :: P.LanguageDef st
inboundDef =
  P.haskellStyle
    { P.opStart         = P.oneOf ":!#$%&*+./<=>?@\\^|-~,"
    , P.opLetter        = P.oneOf ":!#$%&*+./<=>?@\\^|-~,"
    , P.reservedNames   = [ "namespace"
                          , "sort"
                          , "lhs"
                          , "syn"
                          , "loc"
                          , "inh"
                          , "chn"
                          ]
    , P.reservedOpNames = [ "@", "=", ",", "." ]
    }

inboundTokenParser = P.makeTokenParser inboundDef

pIdentifier = P.identifier inboundTokenParser
pBrackets   = P.brackets inboundTokenParser
pSymbol     = P.symbol inboundTokenParser
pColon      = P.colon inboundTokenParser
pDot        = P.dot inboundTokenParser
pComma      = P.comma inboundTokenParser
pReserved   = P.reserved inboundTokenParser
pParens     = P.parens inboundTokenParser
pBraces     = P.braces inboundTokenParser
pWhiteSpace = P.whiteSpace inboundTokenParser
pCommaSep1  = P.commaSep1 inboundTokenParser

type Parser a = P.Parsec String () a

pSortName :: Parser SortName
pSortName = SN <$> pIdentifier

pCtorName :: Parser CtorName
pCtorName = CN <$> pIdentifier

pFieldName :: Parser FieldName
pFieldName = FN <$> pIdentifier

pFieldNames :: Parser [FieldName]
pFieldNames = some pFieldName

pAttrName :: Parser AttrName
pAttrName = AN <$> pIdentifier

pNamespaceName :: Parser NamespaceName
pNamespaceName = NN <$> pIdentifier

pType :: Parser Type
pType = Context <$> pBrackets pNamespaceName

pNodeLabel :: Parser NodeLabel
pNodeLabel
   =  Lhs <$ pReserved "lhs"
  -- <|> Loc <$ pReserved "loc"
  <|> Sub <$> pFieldName

pAttrRef :: Parser AttrRef
pAttrRef = AttrRef <$> pNodeLabel <* pDot <*> pAttrName

pPrim :: Parser Expr
pPrim
   =  ExprAttrRef <$> pAttrRef
  <|> ExprNil    <$  pSymbol "[]"

pExpr :: Parser Expr
pExpr = pPrim >>= pExpr'

pExpr' :: Expr -> Parser Expr
pExpr' e =
     (do
       e' <- ExprCons e <$ pComma <*> pFieldName
       pExpr' e')
 <|> return e

pAttrDef :: Parser AttrDef
pAttrDef = AttrDef <$> pAttrRef <* pSymbol "=" <*> pExpr

pCtorFieldDecls :: Parser CtorFieldDecls
pCtorFieldDecls =
  msum
  [ pParens $ do
      xs <- pFieldNames
      msum
        [ do
            pColon
            t <- pSortName
            return [CFSubtree x t | x <- xs]
        , do
            pSymbol "@"
            t <- pAttrName
            return [CFRef x t | x <- xs]
        ]
  , pBraces $ msum
    [ {-do
        xs <- some pIdentifier
        pColon
        t <- pIdentifier
        return [CFAtom x t | x <- xs]
    , pBraces $-} do
        xs <- pFieldNames
        pColon
        t <- pIdentifier
        return [CFTerminal x t | x <- xs]
    ]
  ]

pCtorDecl :: Parser CtorDecl
pCtorDecl =
  CtorDecl
    <$  pSymbol "|"
    <*> pCtorName
    <*> (concat <$> many pCtorFieldDecls)
    <*> return []
    <*> many pAttrDef

pAttrDeclInhSyn :: Parser AttrDecl
pAttrDeclInhSyn =
      SynDecl
        <$  pReserved "syn"
        <*> pAttrName
        <*  pColon
        <*> pType
  <|> InhDecl
        <$  pReserved "inh"
        <*> pAttrName
        <*  pColon
        <*> pType

pAttrDeclChn :: Parser [AttrDecl]
pAttrDeclChn =
  do
    pReserved "chn"
    an <- pAttrName
    pColon
    ty <- pType
    return [ SynDecl an ty, InhDecl an ty ]

pAttrDecl :: Parser [AttrDecl]
pAttrDecl = (:[]) <$> pAttrDeclInhSyn <|> pAttrDeclChn

pAttrDecls :: Parser [AttrDecl]
pAttrDecls = concat <$> many pAttrDecl

pSortDecl :: Parser SortDecl
pSortDecl =
  SortDecl
    <$  pReserved "sort"
    <*> pSortName
    <*> pAttrDecls
    <*> many pCtorDecl

pNamespaceDecl :: Parser NamespaceDecls
pNamespaceDecl =
  do
    pReserved "namespace"
    pCommaSep1
      (NamespaceDecl
         <$> pNamespaceName
         <*> msum
             [ Just <$ pSymbol ">" <*> pSortName
             , pure Nothing
             ])

pDecl :: Parser (Either NamespaceDecls SortDecl)
pDecl =
      Left <$> pNamespaceDecl
  <|> Right <$> pSortDecl

pSpecification :: String -> Parser Specification
pSpecification name =
  do
    pWhiteSpace
    decls <- many pDecl
    P.eof
    let (namespaceDeclss,sortDecls) = partitionEithers decls
        namespaceDecls = concat namespaceDeclss
        ns = [ nn | NamespaceDecl nn _ <- namespaceDecls ]
        sortDecls' = map (repairSortDecl ns) sortDecls
    return $ Specification name namespaceDecls sortDecls'

repairSortDecl :: [NamespaceName] -> SortDecl -> SortDecl
repairSortDecl ns (SortDecl s adecls cdecls) =
  SortDecl s adecls (map (repairCtorDecl ns) cdecls)

repairCtorDecl :: [NamespaceName] -> CtorDecl -> CtorDecl
repairCtorDecl ns (CtorDecl k fields locattr defs) =
  CtorDecl k (map (repairField ns) fields) locattr defs

repairField :: [NamespaceName] -> CtorFieldDecl -> CtorFieldDecl
repairField ns (CFSubtree f (SN t)) | elem (NN t) ns = CFAtom f (NN t)
repairField ns x                                     = x

parseSpecification :: String -> String -> Either P.ParseError Specification
parseSpecification name = P.parse (pSpecification name) ""

test1 = parseSpecification "test1" "namespace TermVar"
test2 = parseSpecification "test2" "namespace TermVar TypeVar"
test3 = parseSpecification "test3" "sort Term"
test4 = parseSpecification "test4" "sort Term | Add"

test5 = parseSpecification "test5" . unlines $
  [ "namespace TermVar"
  , "sort Term"
  , "  inh ctx : [TermVar]"
  , "  | Add"
  ]

test6 = parseSpecification "test6" . unlines $
  [ "namespace TermVar"
  , "sort Term"
  , "  inh ctx : [TermVar]"
  , "  | Var (x@ctx)"
  , "  | Add (s : Term) (t : Term)"
  , "    s.ctx = lhs.ctx"
  , "    t.ctx = lhs.ctx"
  , "  | Abs (x : TermVar) (t : Term)"
  , "    t.ctx = lhs.ctx , x"
  ]

test7 = parseSpecification "test7" . unlines $
  [ "namespace TermVar"
  , "sort Term                                          "
  , "  inh ctx : [TermVar]                              "
  , "                                                   "
  , "  | Var (x@ctx)                                    "
  , "  | Lam (x : TermVar) (t : Term)                   "
  , "      t.ctx   =  lhs.ctx , x                       "
  , "  | App (t1 : Term) (t2 : Term)                    "
  , "      t1.ctx  =  lhs.ctx                           "
  , "      t2.ctx  =  lhs.ctx                           "
  , "  | Let (d : Term) (t : Term)                      "
  , "      d.ictx = lhs.ctx                             "
  , "      t.ctx  = d.sctx                              "
  , "                                                   "
  , "sort Decls                                         "
  , "  inh ictx : [TermVar]                             "
  , "  syn sctx : [TermVar]                             "
  , "                                                   "
  , "  | Nil                                            "
  , "      lhs.sctx = lhs.ictx                          "
  , "  | Cons (x : TermVar) (t : Term) (d : Decls)      "
  , "      t.ctx    = lhs.ictx                          "
  , "      d.ictx   = lhs.ictx , x                      "
  , "      lhs.sctx  = d.sctx                           "
  ]

test8 = parseSpecification "test8" . unlines $
  [ "namespace TermVar                                  "
  , "                                                   "
  , "sort Term                                          "
  , "  inh ctx : [TermVar]                              "
  , "                                                   "
  , "  | Var (x@ctx)                                    "
  , "  | Lam (x : TermVar) (t : Term)                   "
  , "      t.ctx   =  lhs.ctx , x                       "
  , "  | App (t1 : Term) (t2 : Term)                    "
  , "      t1.ctx  =  lhs.ctx                           "
  , "      t2.ctx  =  lhs.ctx                           "
  , "  | Let (d : Term) (t : Term)                      "
  , "      d.ictx = lhs.ctx                             "
  , "      d.rctx = d.sctx                              "
  , "      t.ctx  = d.sctx                              "
  , "                                                   "
  , "sort Decls                                         "
  , "  inh ictx : [TermVar]                             "
  , "  inh rctx : [TermVar]                             "
  , "  syn sctx : [TermVar]                             "
  , "                                                   "
  , "  | Nil                                            "
  , "      lhs.sctx = lhs.ictx                          "
  , "  | Cons (x : TermVar) (t : Term) (d : Decls)      "
  , "      t.ctx    = lhs.rctx                          "
  , "      d.ictx   = lhs.ictx , x                      "
  , "      d.rctx   = lhs.rctx                          "
  , "      lhs.sctx  = d.sctx                           "
  ]
