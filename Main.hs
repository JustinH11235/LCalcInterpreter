module Main where

import System.Environment ( getArgs )
import Data.List ( elemIndex )
import Control.Applicative ( Alternative(empty, (<|>)) )
import Data.Char ( isAlphaNum, isSpace )

-- GRAMMAR:
-- term ::= application
--        | LAMBDA LCID DOT term

-- application ::= atom application'

-- application' ::= atom application'
--                | ε

-- atom ::= LPAREN term RPAREN
--        | LCID

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
            | x == c = Just (c, xs)
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





parse :: String -> Maybe LCalcTerm
parse inp = do
    let res = runParser (lCalcTerm <* whiteSpace) inp
    case res of
        Just (term, leftover) -> 
            case leftover of 
                "" -> 
                    Just term
                '#':_ -> 
                    Just term
                _ -> 
                    Nothing
        Nothing -> 
            Nothing

lCalcTermLiteral :: Parser LCalcTerm -- \foo. foo bar   LAMBDA LCID DOT term
-- lCalcTermLiteral = uncurry LCalcTermLiteral <$> ((,) <$> (throwAwayChar '\\' *> lcidP) <*> (throwAwayChar '.' *> lCalcTerm))
lCalcTermLiteral = LCalcTermLiteral <$> ((throwAwayChar '\\' <|> throwAwayChar 'λ') *> lcidP) <*> (throwAwayChar '.' *> lCalcTerm)

lCalcTerm :: Parser LCalcTerm
lCalcTerm = LCalcTermFromApp <$> lCalcApp <|> lCalcTermLiteral

-- lCalcAppLiteral :: Parser LCalcApp
-- lCalcAppLiteral = (LCalcAppLiteral <$> lCalcApp) <*> lCalcAtom

lCalcApp :: Parser LCalcApp
lCalcApp = (LCalcAppLiteral <$> lCalcAtom) <*> lCalcApp'

lCalcApp' :: Parser LCalcApp'
lCalcApp' = ((LCalcApp'Literal <$> lCalcAtom) <*> lCalcApp') <|> Parser (\s -> Just(LCalcApp'Empty, s))

lCalcAtomLiteral :: Parser LCalcAtom
lCalcAtomLiteral = LCalcAtomLiteral <$> (throwAwayChar '(' *> lCalcTerm <* throwAwayChar ')')

lCalcAtom :: Parser LCalcAtom
lCalcAtom = lCalcAtomLiteral <|> LCalcAtomFromString <$> lcidP




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


makeNormal :: LCalcTerm -> LCalcTerm
makeNormal term = normalizeTerm term []

normalizeTerm :: LCalcTerm -> [String] -> LCalcTerm
normalizeTerm term context = case term of
    LCalcTermLiteral str term' ->
        LCalcTermLiteral str (normalizeTerm term' (str:context))
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




runLine :: String -> Int -> IO ()
runLine line lineNum = do
    let parsed = parse line
    case parsed of
        Just ast -> do
            let astNew = makeDeBruijn ast

            putStrLn $ termToString ast
            putStrLn ""
            putStrLn $ termToString astNew
            putStrLn ""
            putStrLn $ termToString $ evaluateTerm astNew
            putStrLn $ termToString $ makeNormal $ evaluateTerm astNew
        Nothing ->
            error $ "Syntax error on line " ++ show lineNum ++ "."

runFile :: FilePath -> IO ()
runFile path = do
    input <- lines <$> readFile path

    foldl (>>) (return ()) (zipWith runLine input [1..])

main :: IO ()
main = do
    args <- getArgs

    case args of
        [arg] ->
            runFile $ head args
        _ ->
            error "Incorrect number of arguments; Program accepts one command line argument."
