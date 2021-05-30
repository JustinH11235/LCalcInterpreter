module Main where

import System.Environment ( getArgs )
import Data.List ( elemIndex )
import Control.Applicative ( Alternative(empty, (<|>)) )
import Data.Char ( isAlphaNum, isSpace )
import Text.Read ( readMaybe )

-- GRAMMAR:
-- statement ::= LCID := term  -- assignment
--        | [int/bool/function ::] term -- printing

-- term ::= application
--        | LAMBDA LCID DOT term

-- application ::= atom application'

-- application' ::= atom application'
--                | ε

-- atom ::= LPAREN term RPAREN
--        | LCID

data LCalcStatement =
    LCalcStatementAssignment String LCalcTerm |
    LCalcStatementPrinting String LCalcTerm
    deriving (Show, Eq)

data LCalcTerm =
    LCalcTermFromApp LCalcApp |
    LCalcTermLiteral String LCalcTerm
    deriving (Show, Eq)

-- removed because left recursion will loop, even though we want left recursion when we execute
-- data LCalcApp =
--     LCalcAppLiteral LCalcApp LCalcAtom
--     | LCalcAppFromAtom LCalcAtom
--     deriving (Show, Eq)

data LCalcApp =
    LCalcAppLiteral LCalcAtom LCalcApp'
    deriving (Show, Eq)

data LCalcApp' =
    LCalcApp'Literal LCalcAtom LCalcApp'
    | LCalcApp'Empty
    deriving (Show, Eq)

data LCalcAtom =
    LCalcAtomLiteral LCalcTerm
    | LCalcAtomFromString String
    | LCalcAtomFromInt Int
    deriving (Show, Eq)


-- change to make ints show as ints and bools show as bools
statementToString :: LCalcStatement -> String
statementToString statement = case statement of
    LCalcStatementAssignment str term ->
        str ++ " := " ++ termToString term
    LCalcStatementPrinting str term ->
        if str == "function" then
            termToString term
        else
            str ++ " :: " ++ termToString term

termToString :: LCalcTerm -> String
termToString term = case term of
    LCalcTermLiteral str term' ->
        "λ" ++ str ++ "." ++ termToString term'
    LCalcTermFromApp app ->
        appToString app

atomToString :: LCalcAtom -> String
atomToString atom = case atom of
    LCalcAtomLiteral term ->
        "(" ++ termToString term ++ ")"
    LCalcAtomFromString str ->
        str
    LCalcAtomFromInt int ->
        show int

app'ToString :: LCalcApp' -> String
app'ToString app = case app of
    LCalcApp'Empty ->
        ""
    LCalcApp'Literal atom' app' ->
        atomToString atom' ++ " " ++ app'ToString app'

appToString :: LCalcApp -> String
appToString (LCalcAppLiteral atom app) =
    atomToString atom ++ " " ++ app'ToString app


newtype Parser a = Parser
    {
        runParser :: String -> Maybe (a, String)
    }

instance Functor Parser where
    fmap f (Parser a) =
        Parser $ \input -> do
            (res, input') <- a input
            Just (f res, input')

instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    (<*>) (Parser f) (Parser a) =
        Parser $ \input -> do
            (res, input') <- f input
            (res2, input'') <- a input'
            Just (res res2, input'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (<|>) (Parser p1) (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input





charP :: Char -> Parser Char
charP c = Parser f
    where
        f (x:xs)
            | x == c = Just (x, xs)
            | otherwise = Nothing
        f [] = Nothing

charP' :: (Char -> Bool) -> Parser Char
charP' c = Parser f
    where
        f (x:xs)
            | c x = Just (x, xs)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
    Parser $ \input ->
        let (token, rest) = span f input
        in if token /= "" then Just (token, rest) else Nothing

spanPOptional :: (Char -> Bool) -> Parser String
spanPOptional f =
    Parser $ \input ->
        let (token, rest) = span f input
        in Just (token, rest)

lcidP :: Parser String
lcidP = whiteSpace *> spanP (\c -> c == '_' || (isAlphaNum c && c /= 'λ'))

whiteSpace :: Parser String
whiteSpace = spanPOptional isSpace

throwAwayChar :: Char -> Parser Char
throwAwayChar c = whiteSpace *> charP c

throwAwayString :: String -> Parser String
throwAwayString s = whiteSpace *> stringP s





parse :: String -> Maybe LCalcStatement
parse inp = do
    -- let res = runParser (lCalcTerm <* whiteSpace) inp
    let res = runParser (lCalcStatement <* whiteSpace) inp
    case res of
        Just (term, leftover) ->
            case leftover of
                "" ->
                    Just term
                '#':_ -> -- leftover is a comment
                    Just term
                _ ->
                    Nothing
        Nothing ->
            Nothing

parseUnstable :: String -> LCalcTerm
parseUnstable inp = do
    let Just (term, leftover) = runParser (lCalcTerm <* whiteSpace) inp
    term

lCalcStatementAssignment :: Parser LCalcStatement -- LCID := term
lCalcStatementAssignment = LCalcStatementAssignment <$> lcidP <*> (throwAwayString ":=" *> lCalcTerm)

lCalcStatementPrinting :: Parser LCalcStatement -- [int/bool ::] term
lCalcStatementPrinting = LCalcStatementPrinting <$> (((stringP "int" <|> stringP "bool" <|> stringP "function") <* throwAwayString "::") <|> Parser (\input -> Just ("function", input))) <*> lCalcTerm

lCalcStatement :: Parser LCalcStatement
lCalcStatement = lCalcStatementAssignment <|> lCalcStatementPrinting

lCalcTermLiteral :: Parser LCalcTerm -- \foo. foo bar   LAMBDA LCID DOT term
-- lCalcTermLiteral = uncurry LCalcTermLiteral <$> ((,) <$> (throwAwayChar '\\' *> lcidP) <*> (throwAwayChar '.' *> lCalcTerm))
lCalcTermLiteral = LCalcTermLiteral <$> ((throwAwayChar '\\' <|> throwAwayChar 'λ') *> lcidP) <*> (throwAwayChar '.' *> lCalcTerm)

lCalcTerm :: Parser LCalcTerm
lCalcTerm = LCalcTermFromApp <$> lCalcApp <|> lCalcTermLiteral

-- lCalcAppLiteral :: Parser LCalcApp
-- lCalcAppLiteral = (LCalcAppLiteral <$> lCalcApp) <*> lCalcAtom

lCalcApp :: Parser LCalcApp
lCalcApp = LCalcAppLiteral <$> lCalcAtom <*> lCalcApp'

lCalcApp' :: Parser LCalcApp'
lCalcApp' = ((LCalcApp'Literal <$> lCalcAtom) <*> lCalcApp') <|> Parser (\s -> Just(LCalcApp'Empty, s))

lCalcAtomLiteral :: Parser LCalcAtom
lCalcAtomLiteral = LCalcAtomLiteral <$> (throwAwayChar '(' *> lCalcTerm <* throwAwayChar ')')

lCalcAtom :: Parser LCalcAtom
lCalcAtom = lCalcAtomLiteral <|> LCalcAtomFromString <$> lcidP



-- Convert AST of named identifiers to De Bruijn indexed identifiers
makeDeBruijn :: LCalcTerm -> LCalcTerm
makeDeBruijn term = deBruijnTerm term []

deBruijnTerm :: LCalcTerm -> [String] -> LCalcTerm
deBruijnTerm term context = case term of
    LCalcTermLiteral str term' ->
        LCalcTermLiteral str (deBruijnTerm term' (str:context))
    LCalcTermFromApp app ->
        LCalcTermFromApp $ deBruijnApp app context

deBruijnAtom :: LCalcAtom -> [String] -> LCalcAtom
deBruijnAtom atom context = case atom of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ deBruijnTerm term context
    LCalcAtomFromString str -> case ind of
        Nothing ->
            LCalcAtomFromInt (-1) -- shouldn't happen if user is correct
        Just i ->
            LCalcAtomFromInt i
        where
            ind = elemIndex str context
    LCalcAtomFromInt ind ->
        atom

deBruijnApp' :: LCalcApp' -> [String] -> LCalcApp'
deBruijnApp' app context = case app of
    LCalcApp'Empty ->
        LCalcApp'Empty
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (deBruijnAtom atom' context) (deBruijnApp' app' context)

deBruijnApp :: LCalcApp -> [String] -> LCalcApp
deBruijnApp (LCalcAppLiteral atom app) context =
    LCalcAppLiteral (deBruijnAtom atom context) (deBruijnApp' app context)


-- Version of unusedIdentifier which adds on ' until it finds an available name
-- unusedIdentifierWith' :: String -> [String] -> String
-- unusedIdentifierWith' str context =
--     if str `elem` context then -- if we already have a variable bound of this name
--         unusedIdentifier (str ++ "'") context -- add on a '
--     else
--         str

unusedIdentifier :: String -> [String] -> String
unusedIdentifier str context
    | str `elem` context = unusedIdentifier' str 1 context
    | otherwise = str

unusedIdentifier' :: String -> Int -> [String] -> String
unusedIdentifier' str num context =
    if (str ++ show num) `elem` context then -- if we already have a variable bound of this name
        unusedIdentifier' str (num + 1) context -- add to num
    else
        str ++ show num

-- Convert AST of De Bruijn indexed identifiers to named identifiers
makeNormal :: LCalcTerm -> LCalcTerm
makeNormal term = normalizeTerm term []

normalizeTerm :: LCalcTerm -> [String] -> LCalcTerm
normalizeTerm term context = case term of
    LCalcTermLiteral str term' ->
        LCalcTermLiteral newStr (normalizeTerm term' (newStr:context))
        where
            newStr = unusedIdentifier str context
    LCalcTermFromApp app ->
        LCalcTermFromApp $ normalizeApp app context

normalizeAtom :: LCalcAtom -> [String] -> LCalcAtom
normalizeAtom atom context = case atom of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ normalizeTerm term context
    LCalcAtomFromString str ->
        atom
    LCalcAtomFromInt ind ->
        LCalcAtomFromString $ context !! ind

normalizeApp' :: LCalcApp' -> [String] -> LCalcApp'
normalizeApp' app context = case app of
    LCalcApp'Empty ->
        LCalcApp'Empty
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (normalizeAtom atom' context) (normalizeApp' app' context)

normalizeApp :: LCalcApp -> [String] -> LCalcApp
normalizeApp (LCalcAppLiteral atom app) context =
    LCalcAppLiteral (normalizeAtom atom context) (normalizeApp' app context)





-- old substitute functions that substitute based on string identifiers
substituteTermStr :: String -> LCalcAtom -> LCalcTerm -> LCalcTerm
substituteTermStr str atom term = case term of
    LCalcTermFromApp app ->
        LCalcTermFromApp $ substituteAppStr str atom app
    LCalcTermLiteral str' term' ->
        LCalcTermLiteral str' (substituteTermStr str atom term')

substituteAtomStr :: String -> LCalcAtom -> LCalcAtom -> LCalcAtom
substituteAtomStr str atom atom' = case atom' of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ substituteTermStr str atom term
    LCalcAtomFromString str' ->
        if str' == str then atom else atom' -- this is where we replace the occurence

substituteApp'Str :: String -> LCalcAtom -> LCalcApp' -> LCalcApp'
substituteApp'Str str atom app = case app of
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (substituteAtomStr str atom atom') (substituteApp'Str str atom app')
    LCalcApp'Empty ->
        app

substituteAppStr :: String -> LCalcAtom -> LCalcApp -> LCalcApp
substituteAppStr str atom (LCalcAppLiteral atom' app') =
    LCalcAppLiteral (substituteAtomStr str atom atom') (substituteApp'Str str atom app')





shiftTerm :: Int -> LCalcTerm -> Int -> LCalcTerm
shiftTerm shift term depth = case term of
    LCalcTermFromApp app ->
        LCalcTermFromApp $ shiftApp shift app depth
    LCalcTermLiteral str' term' ->
        LCalcTermLiteral str' (shiftTerm shift term' (depth + 1))

shiftAtom :: Int -> LCalcAtom -> Int -> LCalcAtom
shiftAtom shift atom depth = case atom of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ shiftTerm shift term depth
    LCalcAtomFromInt ind -> -- should never get string
        LCalcAtomFromInt $ -- this is where we shift
            if ind > depth then -- if a free var (w/ respect to starting node)
                ind + shift
            else
                ind

shiftApp' :: Int -> LCalcApp' -> Int -> LCalcApp'
shiftApp' shift app depth = case app of
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (shiftAtom shift atom' depth) (shiftApp' shift app' depth)
    LCalcApp'Empty ->
        app

shiftApp :: Int -> LCalcApp -> Int -> LCalcApp
shiftApp shift (LCalcAppLiteral atom' app') depth =
    LCalcAppLiteral (shiftAtom shift atom' depth) (shiftApp' shift app' depth)





substitute :: LCalcAtom -> LCalcTerm -> LCalcTerm
substitute atom term = shiftTerm (-1) (substituteTerm (shiftAtom 1 atom (-1)) term (-1 + 1)) 0

substituteTerm :: LCalcAtom -> LCalcTerm -> Int -> LCalcTerm
substituteTerm atom term depth = case term of
    LCalcTermFromApp app ->
        LCalcTermFromApp $ substituteApp atom app depth
    LCalcTermLiteral str' term' ->
        LCalcTermLiteral str' (substituteTerm atom term' (depth + 1))

substituteAtom :: LCalcAtom -> LCalcAtom -> Int -> LCalcAtom
substituteAtom atom atom' depth = case atom' of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ substituteTerm atom term depth
    LCalcAtomFromInt ind -> -- this is where we replace the occurence
        if ind == depth then -- if this refers to the variable we are replacing
            shiftAtom depth atom 0 -- shift by our current depth
        else
            atom'

substituteApp' :: LCalcAtom -> LCalcApp' -> Int -> LCalcApp'
substituteApp' atom app depth = case app of
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (substituteAtom atom atom' depth) (substituteApp' atom app' depth)
    LCalcApp'Empty ->
        app

substituteApp :: LCalcAtom -> LCalcApp -> Int -> LCalcApp
substituteApp atom (LCalcAppLiteral atom' app') depth =
    LCalcAppLiteral (substituteAtom atom atom' depth) (substituteApp' atom app' depth)





evaluateTerm :: LCalcTerm -> LCalcTerm
evaluateTerm term = case term of
    LCalcTermLiteral str term' ->
        LCalcTermLiteral str (evaluateTerm term')
    LCalcTermFromApp app -> case res of
        LCalcAppLiteral (LCalcAtomLiteral term) LCalcApp'Empty -> -- simply useless parens around term
            term
        _ ->
            LCalcTermFromApp res
        where
            res = evaluateApp app

evaluateAtom :: LCalcAtom -> LCalcAtom
evaluateAtom atom = case atom of
    LCalcAtomLiteral term -> case res of
        LCalcTermFromApp (LCalcAppLiteral (LCalcAtomFromInt ind) LCalcApp'Empty) ->
            LCalcAtomFromInt ind -- simplify identifiers wrapped in an atom
        LCalcTermFromApp (LCalcAppLiteral (LCalcAtomLiteral (LCalcTermFromApp app)) LCalcApp'Empty) ->
            LCalcAtomLiteral $ LCalcTermFromApp app -- simplify useless parens around terms inside atoms
        _ ->
            LCalcAtomLiteral res
        where
            res = evaluateTerm term
    LCalcAtomFromString str -> atom -- should never see if using De Bruijn
    LCalcAtomFromInt ind -> atom

-- helper function that evaluateApp(') uses to simplify remainder of applications after finding a nonvalue
evaluateApp'NoSub :: LCalcApp' -> LCalcApp'
evaluateApp'NoSub app = case app of
    LCalcApp'Empty ->
        app
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (evaluateAtom atom') (evaluateApp'NoSub app')

evaluateApp' :: LCalcApp' -> LCalcApp'
evaluateApp' (LCalcApp'Literal atom app) = case app of -- input shouldn't be empty
    LCalcApp'Empty -> -- if the second part is empty
        LCalcApp'Literal (evaluateAtom atom) app
    LCalcApp'Literal atom' app' ->
        case evaluateAtom atom of
            LCalcAtomLiteral (LCalcTermLiteral str term) -> -- left is a term literal
                evaluateApp' $ LCalcApp'Literal (LCalcAtomLiteral (substitute atom' term)) app'
            _ -> -- left is an identifier, can't simplify any further down the chain due to implicit parens (left association)
                LCalcApp'Literal (evaluateAtom atom) (evaluateApp'NoSub app)

evaluateApp :: LCalcApp -> LCalcApp
evaluateApp (LCalcAppLiteral atom app) = case app of
    LCalcApp'Empty -> -- if the second part is empty
        LCalcAppLiteral (evaluateAtom atom) app
    LCalcApp'Literal atom' app' ->
        case evaluateAtom atom of
            LCalcAtomLiteral (LCalcTermLiteral str term) -> -- left is a term literal
                evaluateApp $ LCalcAppLiteral (LCalcAtomLiteral (substitute atom' term)) app'
            _ -> -- left is an identifier, can't simplify any further down the chain due to implicit parens (left association)
                LCalcAppLiteral (evaluateAtom atom) (evaluateApp'NoSub app)





appFromIntLiteral :: Int -> LCalcApp
appFromIntLiteral 0 = LCalcAppLiteral (LCalcAtomFromString "x") LCalcApp'Empty
appFromIntLiteral 1 = LCalcAppLiteral (LCalcAtomFromString "f") (LCalcApp'Literal (LCalcAtomFromString "x") LCalcApp'Empty)
appFromIntLiteral left = LCalcAppLiteral (LCalcAtomFromString "f") (LCalcApp'Literal (LCalcAtomLiteral $ LCalcTermFromApp $ appFromIntLiteral $ left - 1) LCalcApp'Empty)

atomFromIntLiteral :: Int -> LCalcAtom
atomFromIntLiteral left = LCalcAtomLiteral $ LCalcTermLiteral "f" (LCalcTermLiteral "x" (LCalcTermFromApp $ appFromIntLiteral left))

getIntAliasesTerm :: LCalcTerm -> [(String, LCalcAtom)]
getIntAliasesTerm term = case term of
    LCalcTermLiteral str term' ->
        getIntAliasesTerm term'
    LCalcTermFromApp app ->
        getIntAliasesApp app

getIntAliasesAtom :: LCalcAtom -> [(String, LCalcAtom)]
getIntAliasesAtom atom = case atom of
    LCalcAtomLiteral term ->
        getIntAliasesTerm term
    LCalcAtomFromString str ->
        case toInt of
            Just int ->
                [(str, atomFromIntLiteral int)] -- this is where we add to aliases
            Nothing ->
                []
        where
            toInt = readMaybe str :: Maybe Int

getIntAliasesApp' :: LCalcApp' -> [(String, LCalcAtom)]
getIntAliasesApp' app = case app of -- input shouldn't be empty
    LCalcApp'Empty ->
        []
    LCalcApp'Literal atom' app' ->
        getIntAliasesAtom atom' ++ getIntAliasesApp' app'

getIntAliasesApp :: LCalcApp -> [(String, LCalcAtom)]
getIntAliasesApp (LCalcAppLiteral atom' app') =
    getIntAliasesAtom atom' ++ getIntAliasesApp' app'

-- doesn't work for recursive functions, replaced with direct substitution
-- wrapWithAliases :: LCalcTerm -> [(String, LCalcAtom)] -> LCalcTerm
-- wrapWithAliases term ((alias, def):rest) =
--     wrapWithAliases (LCalcTermFromApp $ LCalcAppLiteral (LCalcAtomLiteral $ LCalcTermLiteral alias term) (LCalcApp'Literal def LCalcApp'Empty)) rest
-- wrapWithAliases app [] =
--     app

stdLibStrings :: [(String, String)]
stdLibStrings = [ -- IMPORTANT: functions must be defined before they are used in another
    ("0", "(λf.λx.x)"),
    ("1", "(λf.λx.f x)"),
    ("2", "(λf.λx.f (f x))"),

    ("Y", "(λf.(λx.f (x x)) (λx.f (x x)))"),

    ("SUCC", "(λn.λf.λx.f (n f x))"),
    ("ADD", "(λm.λn.λf.λx.m f (n f x))"),
    ("PRED", "(λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u))"),
    ("SUB", "(λm.λn.n PRED m)"),
    ("MULT", "(λm.λn.λf.m (n f))"),
    ("POW", "(λb.λe.e b)"),

    ("TRUE", "(λx.λy.x)"),
    ("FALSE", "(λx.λy.y)"),
    ("AND", "(λp.λq.p q p)"),
    ("OR", "(λp.λq.p p q)"),
    ("NOT", "(λp.p FALSE TRUE)"),
    ("IF_THEN_ELSE", "(λp.λa.λb.p a b)"),
    ("IS_ZERO", "(λn.n (λp.FALSE) TRUE)"),
    ("LEQ", "(λm.λn.IS_ZERO (SUB m n))"),
    ("LT", "(λm.λn.LEQ (SUCC m) n)"),
    ("GT", "(λm.λn. NOT (LEQ m n))"),
    ("GEQ", "(λm.λn.GT (SUCC m) n)"),
    ("EQ", "(λm.λn.AND (LEQ m n) (LEQ n m))"),
    ("IS_EVEN", "(λn.n NOT TRUE)"),
    ("IS_ODD", "(λn.n NOT FALSE)"),

    ("PAIR", "(λx.λy.λf.f x y)"),
    ("FIRST", "(λp.p TRUE)"),
    ("SECOND", "(λp.p FALSE)"),
    ("SHIFT_AND_INC", "(λp. PAIR (SECOND p) (SUCC (SECOND p)))"),

    ("NIL", "FALSE"),
    ("IS_NULL", "(λl.l (λv.λn.λ_.FALSE) TRUE)"),
    ("SINGLE_NODE", "(λv.PAIR v NIL)"),
    ("UNSHIFT", "PAIR"),
    ("SHIFT", "SECOND"),
    ("VAL", "FIRST"),
    ("NEXT", "SECOND"),
    ("PUSH", "Y (λr.λp.λl.l (λv.λn.λ_.UNSHIFT v (r p n)) (SINGLE_NODE p))"),
    ("POP", "Y (λr.λl.(NEXT l) (λv.λn.λ_.UNSHIFT (VAL l) (r (NEXT l))) NIL)"),
    ("MAP", "Y (λr.λf.λl.l (λv.λn.λ_.UNSHIFT (f v) (r f n)) NIL)"),
    ("FILTER", "Y (λr.λf.λl.l (λv.λn.λ_.IF_THEN_ELSE (f v) (UNSHIFT v (r f n)) (r f n)) NIL)"),
    ("REDUCE", "Y (λr.λf.λl.(NEXT l) (λv.λn.λ_.f (VAL l) (r f (NEXT l))) (VAL l))"),
    ("REDUCE1", "Y (λr.λf.λi.λl.l (λv.λn.λ_.f v (r f i n)) i)"),
    ("RANGE", "Y (λr.λs.λe.(LT s e) (UNSHIFT s (r (SUCC s) e)) NIL)"), -- [s,e)
    ("RANGE1", "Y (λr.λs.λe.λstep.(LT s e) (UNSHIFT s (r (ADD s step) e step)) NIL)"), -- [s, e)
    ("LENGTH", "REDUCE1 (λm.λn.SUCC n) 0"),
    ("COUNT", "(λf.λl.LENGTH (FILTER f l))"),
    ("REVERSE", "Y (λr.λl.l (λv.λn.λ_.PUSH v (r n)) NIL)"),
    ("TAKE", "(λn.λl.(SUB (LENGTH l) n) POP l)"),
    ("DROP", "(λn.λl.n SHIFT l)"),
    ("SLICE", "(λs.λe.λl.DROP s (TAKE e l))"),
    ("SORT_HELPER", "Y (λr.λl.(NEXT l) (λv.λn.λ_.IF_THEN_ELSE (GT (VAL l) v) (UNSHIFT v (r (UNSHIFT (VAL l) n))) (UNSHIFT (VAL l) (r (NEXT l)))) l)"),
    ("SORT", "(λl.(LENGTH l) SORT_HELPER l)"),

    ("SUM", "REDUCE1 ADD 0"),
    ("MAX", "(λm.λn.(GT m n) m n)"),
    ("MAXIMUM", "REDUCE MAX"),
    ("MIN", "(λm.λn.(LT m n) m n)"),
    ("MINIMUM", "REDUCE MIN"),

    ("DIV", "Y (λr.λm.λn.λf.λx.(LT m n) (0 f x) (f (r (SUB m n) n f x)))"),
    ("MOD", "Y (λr.λm.λn.(LT m n) m (r (SUB m n) n))")
    ]

stdlib :: [(String, LCalcAtom)]
stdlib = map (\(name, def) -> (name, LCalcAtomLiteral $ parseUnstable def)) (reverse stdLibStrings)

replaceAliasTerm :: LCalcTerm -> (String, LCalcAtom) -> LCalcTerm
replaceAliasTerm term tuple = case term of
    LCalcTermLiteral str term' ->
        LCalcTermLiteral str (replaceAliasTerm term' tuple)
    LCalcTermFromApp app ->
        LCalcTermFromApp $ replaceAliasApp app tuple

replaceAliasAtom :: LCalcAtom -> (String, LCalcAtom) -> LCalcAtom
replaceAliasAtom atom tuple@(alias, def) = case atom of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ replaceAliasTerm term tuple
    LCalcAtomFromString str ->
        if str == alias then
            def -- this is where we replace
        else
            LCalcAtomFromString str

replaceAliasApp' :: LCalcApp' -> (String, LCalcAtom) -> LCalcApp'
replaceAliasApp' app tuple = case app of -- input shouldn't be empty
    LCalcApp'Empty ->
        LCalcApp'Empty
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (replaceAliasAtom atom' tuple) (replaceAliasApp' app' tuple)

replaceAliasApp :: LCalcApp -> (String, LCalcAtom) -> LCalcApp
replaceAliasApp (LCalcAppLiteral atom' app') tuple =
    LCalcAppLiteral (replaceAliasAtom atom' tuple) (replaceAliasApp' app' tuple)

replaceAliases :: LCalcTerm -> [(String, LCalcAtom)] -> LCalcTerm
replaceAliases = foldl replaceAliasTerm

isInt :: LCalcTerm -> Bool
isInt (LCalcTermLiteral f (LCalcTermLiteral x (LCalcTermFromApp app))) = isIntApp app
isInt _ = False

isIntApp :: LCalcApp -> Bool
isIntApp (LCalcAppLiteral (LCalcAtomFromInt 1) (LCalcApp'Literal (LCalcAtomLiteral (LCalcTermFromApp app)) LCalcApp'Empty)) = isIntApp app
isIntApp (LCalcAppLiteral (LCalcAtomFromInt 1) (LCalcApp'Literal (LCalcAtomFromInt 0) LCalcApp'Empty)) = True 
isIntApp (LCalcAppLiteral (LCalcAtomFromInt 0) LCalcApp'Empty) = True
isIntApp _ = False

intLiteralFromTerm :: LCalcTerm -> Int
intLiteralFromTerm (LCalcTermLiteral f (LCalcTermLiteral x (LCalcTermFromApp app))) = intLiteralFromApp app

intLiteralFromApp :: LCalcApp -> Int
intLiteralFromApp (LCalcAppLiteral (LCalcAtomFromInt 1) (LCalcApp'Literal (LCalcAtomLiteral (LCalcTermFromApp app)) LCalcApp'Empty)) = 1 + intLiteralFromApp app
intLiteralFromApp (LCalcAppLiteral (LCalcAtomFromInt 1) (LCalcApp'Literal (LCalcAtomFromInt 0) LCalcApp'Empty)) = 1
intLiteralFromApp (LCalcAppLiteral (LCalcAtomFromInt 0) LCalcApp'Empty) = 0

isBool :: LCalcTerm -> Bool
isBool (LCalcTermLiteral x (LCalcTermLiteral y (LCalcTermFromApp (LCalcAppLiteral (LCalcAtomFromInt 0) LCalcApp'Empty)))) = True
isBool (LCalcTermLiteral x (LCalcTermLiteral y (LCalcTermFromApp (LCalcAppLiteral (LCalcAtomFromInt 1) LCalcApp'Empty)))) = True 
isBool _ = False

boolLiteralFromTerm :: LCalcTerm -> Bool 
boolLiteralFromTerm (LCalcTermLiteral x (LCalcTermLiteral y (LCalcTermFromApp (LCalcAppLiteral (LCalcAtomFromInt 0) LCalcApp'Empty)))) = False
boolLiteralFromTerm (LCalcTermLiteral x (LCalcTermLiteral y (LCalcTermFromApp (LCalcAppLiteral (LCalcAtomFromInt 1) LCalcApp'Empty)))) = True 

runLines :: [String] -> Int -> [(String, LCalcAtom)] -> IO ()
runLines (line:rest) lineNum aliases = do
    if dropWhile isSpace line == "" || head (dropWhile isSpace line) == '#' then
        runLines rest (lineNum + 1) aliases
    else do
        let parsed = parse line
        case parsed of
            Just (LCalcStatementAssignment str term) -> do
                let replaced = replaceAliases term (getIntAliasesTerm term ++ aliases)

                runLines rest (lineNum + 1) ((str, LCalcAtomLiteral replaced):aliases)
            Just (LCalcStatementPrinting datatype term) -> do
                let replaced = replaceAliases term (getIntAliasesTerm term ++ aliases)
                let evaluatedAst = evaluateTerm $ makeDeBruijn replaced
                let normalizedAst = makeNormal evaluatedAst

                -- Debugging
                -- putStrLn $ termToString replaced
                -- putStrLn $ termToString $ makeDeBruijn replaced
                -- putStrLn $ termToString evaluatedAst
                -- putStrLn $ termToString normalizedAst

                putStrLn $ case datatype of
                    "int" ->
                        if isInt evaluatedAst then
                            show $ intLiteralFromTerm evaluatedAst
                        else
                            termToString normalizedAst
                    "bool" ->
                        if isBool evaluatedAst then
                            show $ boolLiteralFromTerm evaluatedAst
                        else
                            termToString normalizedAst
                    "function" ->
                        termToString normalizedAst

                runLines rest (lineNum + 1) aliases
            Nothing ->
                error $ "Syntax error on line " ++ show lineNum ++ "."
runLines [] lineNum aliases = return ()

runFile :: FilePath -> IO ()
runFile path = do
    input <- lines <$> readFile path

    runLines input 1 stdlib

main :: IO ()
main = do
    args <- getArgs

    case args of
        [arg] ->
            runFile $ head args
        _ ->
            error "Incorrect number of arguments; Program accepts one command line argument."
