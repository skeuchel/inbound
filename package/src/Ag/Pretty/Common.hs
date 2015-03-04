----------------------------------------------
-- Module created based on source code from --
-- Language.Haskell.Pretty library          --
----------------------------------------------

module Ag.Pretty.Common  where

import Control.Monad (liftM, liftM2)
import qualified Text.PrettyPrint.ANSI.Leijen as P

--infixr 5 </>,<//>,<$>,<$$>
infixr 5 <$>

(<$>) :: Doc -> Doc -> Doc
am <$> bm = do
               a <- am
               b <- bm
               return (a P.<$> b)

indent :: Int -> Doc -> Doc
indent i dm = dm >>= return . P.indent i -- nest

tupled :: [Doc] -> Doc
tupled = parenList

dot :: Doc
dot             = char '.'

infixl 5 $$$

-----------------------------------------------------------------------------

-- | Varieties of layout we can use.
data PPLayout = PPOffsideRule        -- ^ classical layout
              | PPSemiColon        -- ^ classical layout made explicit
              | PPInLine        -- ^ inline decls, with newlines between them
              | PPNoLayout        -- ^ everything on a single line
              deriving Eq

type Indent = Int


-- | Pretty-printing parameters.
--
-- /Note:/ the 'onsideIndent' must be positive and less than all other indents.
data PPHsMode = PPHsMode {
                                -- | indentation of a class or instance
                classIndent :: Indent,
                                -- | indentation of a @do@-expression
                doIndent :: Indent,
                                -- | indentation of the body of a
                                -- @case@ expression
                caseIndent :: Indent,
                                -- | indentation of the declarations in a
                                -- @let@ expression
                letIndent :: Indent,
                                -- | indentation of the declarations in a
                                -- @where@ clause
                whereIndent :: Indent,
                                -- | indentation added for continuation
                                -- lines that would otherwise be offside
                onsideIndent :: Indent,
                                -- | blank lines between statements?
                spacing :: Bool,
                                -- | Pretty-printing style to use
                layout :: PPLayout,
                                -- | add GHC-style @LINE@ pragmas to output?
                linePragmas :: Bool,
                                -- | not implemented yet
                comments :: Bool
                }

-- | The default mode: pretty-print using the offside rule and sensible
-- defaults.
defaultMode :: PPHsMode
defaultMode = PPHsMode{
                      classIndent = 8,
                      doIndent = 3,
                      caseIndent = 5,
                      letIndent = 0,
                      whereIndent = 6,
                      onsideIndent = 2,
                      spacing = True,
                      layout = PPOffsideRule,
                      linePragmas = False,
                      comments = True
                      }

-- | Pretty printing monad
newtype DocM s a = DocM (s -> a)

instance Functor (DocM s) where
         fmap f xs = do x <- xs; return (f x)

instance Monad (DocM s) where
        (>>=) = thenDocM
        (>>) = then_DocM
        return = retDocM

{-# INLINE thenDocM #-}
{-# INLINE then_DocM #-}
{-# INLINE retDocM #-}
{-# INLINE unDocM #-}
{-# INLINE getPPEnv #-}

thenDocM :: DocM s a -> (a -> DocM s b) -> DocM s b
thenDocM m k = DocM $ (\s -> case unDocM m $ s of a -> unDocM (k a) $ s)

then_DocM :: DocM s a -> DocM s b -> DocM s b
then_DocM m k = DocM $ (\s -> case unDocM m $ s of _ -> unDocM k $ s)

retDocM :: a -> DocM s a
retDocM a = DocM (\_s -> a)

unDocM :: DocM s a -> (s -> a)
unDocM (DocM f) = f

-- all this extra stuff, just for this one function.
getPPEnv :: DocM s s
getPPEnv = DocM id

-- So that pp code still looks the same
-- this means we lose some generality though

-- | The document type produced by these pretty printers uses a 'PPHsMode'
-- environment.
type Doc = DocM PPHsMode P.Doc

-- | Things that can be pretty-printed, including all the syntactic objects
-- in "Language.Haskell.Syntax".
class Pretty a where
        -- | Pretty-print something in isolation.
        pretty :: a -> Doc
        -- | Pretty-print something in a precedence context.
        prettyPrec :: Int -> a -> Doc
        pretty = prettyPrec 0
        prettyPrec _ = pretty

-- The pretty printing combinators

empty :: Doc
empty = return P.empty

nest :: Int -> Doc -> Doc
nest i m = m >>= return . P.nest i



-- Literals

text, ptext :: String -> Doc
text = return . P.text
ptext = return . P.text

char :: Char -> Doc
char = return . P.char

int :: Int -> Doc
int = return . P.int

integer :: Integer -> Doc
integer = return . P.integer

float :: Float -> Doc
float = return . P.float

double :: Double -> Doc
double = return . P.double

rational :: Rational -> Doc
rational = return . P.rational



-- Simple Combining Forms

parens, brackets, braces{-,quotes,doubleQuotes-} :: Doc -> Doc
parens d = d >>= return . P.parens
brackets d = d >>= return . P.brackets
braces d = d >>= return . P.braces
{-
quotes d = d >>= return . P.quotes
doubleQuotes d = d >>= return . P.doubleQuotes
-}
parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

-- Constants

semi,comma,colon,space,equals :: Doc
semi = return P.semi
comma = return P.comma
colon = return P.colon
space = return P.space
equals = return P.equals

lparen,rparen{-,lbrack,rbrack-},lbrace,rbrace :: Doc
lparen = return  P.lparen
rparen = return  P.rparen
{-lbrack = return  P.lbrack
rbrack = return  P.rbrack-}
lbrace = return  P.lbrace
rbrace = return  P.rbrace

-- Combinators

(<>),(<+>),($$){-,($+$)-} :: Doc -> Doc -> Doc
aM <> bM = do{a<-aM;b<-bM;return (a P.<> b)}
aM <+> bM = do{a<-aM;b<-bM;return (a P.<+> b)}
aM $$ bM = do{a<-aM;b<-bM;return (P.align (a P.<$> b))}
{-aM $+$ bM = do{a<-aM;b<-bM;return (a P.$+$ b)}-}

{-
vsep :: [Doc] -> Doc
vsep = fold (<$>)
  where  fold f []       = empty
         fold f ds       = foldr1 f ds
-}

hcat,hsep,vcat,sep,cat,fsep,fcat,vsep,list :: [Doc] -> Doc
hcat dl = sequence dl >>= return . P.hcat
hsep dl = sequence dl >>= return . P.hsep
vsep dl = sequence dl >>= return . P.vsep
vcat dl = sequence dl >>= return . P.vcat
sep dl = sequence dl >>= return . P.sep
cat dl = sequence dl >>= return . P.cat
fsep dl = sequence dl >>= return . P.fillSep
fcat dl = sequence dl >>= return . P.fillCat
list dl = sequence dl >>= return . P.list

-- Some More

hang :: Doc -> Int -> Doc -> Doc
hang dM i rM = do{d<-dM;r<-rM;return $ P.sep [d, P.nest i r]}

-- Yuk, had to cut-n-paste this one from Pretty.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (d1:ds) = go d1 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

punctuate' :: Doc -> [Doc] -> [Doc]
punctuate' _ []     = []
punctuate' p (d1:ds) = go d1 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <+> p) : go e es

{-
-- | render the document with a given style and mode.
renderStyleMode :: P.Style -> PPHsMode -> Doc -> String
renderStyleMode ppStyle ppMode d = P.renderStyle ppStyle . unDocM d $ ppMode


-- | render the document with a given mode.
renderWithMode :: PPHsMode -> Doc -> String
renderWithMode = renderStyleMode P.style



-- | render the document with 'defaultMode'.
render :: Doc -> String
render = renderWithMode defaultMode


-- | pretty-print with a given style and mode.
prettyPrintStyleMode :: Pretty a => P.Style -> PPHsMode -> a -> String
prettyPrintStyleMode ppStyle ppMode = renderStyleMode ppStyle ppMode . pretty

-- | pretty-print with the default style and a given mode.

prettyPrintWithMode :: Pretty a => PPHsMode -> a -> String
prettyPrintWithMode = prettyPrintStyleMode P.style

-- | pretty-print with the default style and 'defaultMode'.
--prettyPrint :: Pretty a => a -> String
--prettyPrint = undefined -- TODO -- prettyPrintWithMode defaultMode

fullRenderWithMode :: PPHsMode -> P.Mode -> Int -> Float ->
                      (P.TextDetails -> a -> a) -> a -> Doc -> a
fullRenderWithMode ppMode m i f fn e mD =
                   P.fullRender m i f fn e $ (unDocM mD) ppMode


fullRender :: P.Mode -> Int -> Float -> (P.TextDetails -> a -> a)
              -> a -> Doc -> a
fullRender = fullRenderWithMode defaultMode
-}

putDoc :: Doc -> IO ()
putDoc dm = P.putDoc $ (unDocM dm) defaultMode

------------------------- pp utils -------------------------
maybePP :: (a -> Doc) -> Maybe a -> Doc
maybePP _ Nothing = empty
maybePP pp (Just a) = pp a


parenList :: [Doc] -> Doc
parenList = parens . hsep . punctuate comma

braceList :: [Doc] -> Doc
braceList = braces . hsep . punctuate comma

bracketList :: [Doc] -> Doc
bracketList = brackets . hsep

angleList :: [Doc] -> Doc
angleList ds = char '<' <> myFsepSimple (punctuate comma ds) <> char '>'

-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {- -}
flatBlock :: [Doc] -> Doc
flatBlock = braces . (space <>) . hsep . punctuate semi


-- Same, but put each thing on a separate line
prettyBlock :: [Doc] -> Doc
prettyBlock = braces . (space <>) . vcat . punctuate semi

-- Monadic PP Combinators -- these examine the env

blankline :: Doc -> Doc
blankline dl = do{e<-getPPEnv;if spacing e && layout e /= PPNoLayout
                              then space $$ dl else dl}
topLevel :: Doc -> [Doc] -> Doc
topLevel header dl = do
         e <- fmap layout getPPEnv
         case e of
             PPOffsideRule -> header $$ vcat dl
             PPSemiColon -> header $$ prettyBlock dl
             PPInLine -> header $$ prettyBlock dl
             PPNoLayout -> header <+> flatBlock dl

ppBody :: (PPHsMode -> Int) -> [Doc] -> Doc
ppBody f dl = do
        e <- fmap layout getPPEnv
        i <- fmap f getPPEnv
        case e of
            PPOffsideRule -> indent i . hcat $ dl
            PPSemiColon   -> nest i . prettyBlock $ dl
            _             -> flatBlock dl


ppBindings :: [Doc] -> Doc
ppBindings dl = do
        e <- fmap layout getPPEnv
        case e of
            PPOffsideRule -> vcat dl
            PPSemiColon   -> vcat . punctuate semi $ dl
            _             -> hsep . punctuate semi $ dl

($$$) :: Doc -> Doc -> Doc
a $$$ b = layoutChoice (a $$) (a <+>) b

mySep :: [Doc] -> Doc
mySep = layoutChoice mySep' hsep
        where
        -- ensure paragraph fills with indentation.
        mySep' [x]    = x
        mySep' (x:xs) = x <+> fsep xs
        mySep' []     = error "Internal error: mySep"

myVcat :: [Doc] -> Doc
myVcat = layoutChoice vcat hsep

myFsepSimple :: [Doc] -> Doc
myFsepSimple = layoutChoice fsep hsep

-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
myFsep :: [Doc] -> Doc
myFsep = layoutChoice fsep' hsep
        where   fsep' [] = empty
                fsep' (d:ds) = do
                        e <- getPPEnv
                        let n = onsideIndent e
                        --nest n (fsep (nest (-n) d:ds))
                        nest n (hsep (nest (-n) d:ds))

layoutChoice :: (a -> Doc) -> (a -> Doc) -> a -> Doc
layoutChoice a b dl = do e <- getPPEnv
                         if layout e == PPOffsideRule ||
                            layout e == PPSemiColon
                          then a dl else b dl

-- Colors
yellow, green, red, magenta, cyan :: Doc -> Doc
yellow  = liftM P.yellow
green   = liftM P.green
red     = liftM P.red
cyan    = liftM P.cyan
magenta = liftM P.magenta

underline, bold :: Doc -> Doc
underline = liftM P.underline
bold      = liftM P.bold

showWidth :: Int -> Doc -> String
showWidth w dm = P.displayS (P.renderPretty 0.4 w (unDocM dm defaultMode)) ""
