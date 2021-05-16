module Main where

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
    deriving (Eq)

-- removed because left recursion will loop, even though we want left recursion when we execute
-- data LCalcApp =
--     LCalcAppLiteral LCalcApp LCalcAtom
--     | LCalcAppFromAtom LCalcAtom
--     deriving (Show, Eq)

data LCalcApp =
    LCalcAppLiteral LCalcAtom LCalcApp'
    deriving (Eq)

data LCalcApp' =
    LCalcApp'Literal LCalcAtom LCalcApp'
    | LCalcApp'Empty
    deriving (Eq)

data LCalcAtom =
    LCalcAtomLiteral LCalcTerm
    | LCalcAtomFromString String
    | LCalcAtomFromInt Int
    deriving (Eq)


instance Show LCalcTerm where
    show term = case term of
        LCalcTermLiteral str term' ->
            "\\" ++ str ++ "." ++ show term'
        LCalcTermFromApp app ->
            show app

instance Show LCalcAtom where
    show atom = case atom of
        LCalcAtomLiteral term ->
            "(" ++ show term ++ ")"
        LCalcAtomFromString str ->
            str
        LCalcAtomFromInt int ->
            show int

instance Show LCalcApp' where
    show app = case app of
        LCalcApp'Empty ->
            ""
        LCalcApp'Literal atom' app' ->
            show atom' ++ " " ++ show app'

instance Show LCalcApp where
    show (LCalcAppLiteral atom app) =
        show atom ++ " " ++ show app


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
lcidP = whiteSpace *> spanP isAlphaNum

whiteSpace :: Parser String
whiteSpace = spanPOptional isSpace

throwAwayChar :: Char -> Parser Char
throwAwayChar c = whiteSpace *> charP c





parse :: String -> Maybe LCalcTerm
parse inp = do
    let res = runParser lCalcTerm inp
    case res of
        Just (term, leftover) -> Just term
        Nothing -> Nothing

lCalcTermLiteral :: Parser LCalcTerm -- \foo. foo bar   LAMBDA LCID DOT term
-- lCalcTermLiteral = uncurry LCalcTermLiteral <$> ((,) <$> (throwAwayChar '\\' *> lcidP) <*> (throwAwayChar '.' *> lCalcTerm))
lCalcTermLiteral = LCalcTermLiteral <$> (throwAwayChar '\\' *> lcidP) <*> (throwAwayChar '.' *> lCalcTerm)

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

deBruijnApp' :: LCalcApp' -> [String] -> LCalcApp'
deBruijnApp' app context = case app of
    LCalcApp'Empty ->
        LCalcApp'Empty
    LCalcApp'Literal atom' app' ->
        LCalcApp'Literal (deBruijnAtom atom' context) (deBruijnApp' app' context)

deBruijnApp :: LCalcApp -> [String] -> LCalcApp
deBruijnApp (LCalcAppLiteral atom app) context = 
    LCalcAppLiteral (deBruijnAtom atom context) (deBruijnApp' app context)


-- TODO: add a converter back to normal variable names





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
    LCalcTermLiteral str term' ->
        LCalcTermLiteral str (shiftTerm shift term' (depth + 1))

shiftAtom :: Int -> LCalcAtom -> Int -> LCalcAtom
shiftAtom shift atom depth = case atom of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ shiftTerm shift term (depth + 1)
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
substitute atom term = shiftTerm (-1) (substituteTerm (shiftAtom 1 atom 0) term 0) 0

substituteTerm :: LCalcAtom -> LCalcTerm -> Int -> LCalcTerm
substituteTerm atom term depth = case term of
    LCalcTermFromApp app ->
        LCalcTermFromApp $ substituteApp atom app depth
    LCalcTermLiteral str' term' ->
        LCalcTermLiteral str' (substituteTerm atom term' (depth + 1))

substituteAtom :: LCalcAtom -> LCalcAtom -> Int -> LCalcAtom
substituteAtom atom atom' depth = case atom' of
    LCalcAtomLiteral term ->
        LCalcAtomLiteral $ substituteTerm atom term (depth + 1)
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
    LCalcTermFromApp app ->
        LCalcTermFromApp $ evaluateApp app

evaluateAtom :: LCalcAtom -> LCalcAtom
evaluateAtom atom = case atom of
    LCalcAtomLiteral term -> case res of
        LCalcTermFromApp (LCalcAppLiteral (LCalcAtomFromInt int) LCalcApp'Empty) ->
            LCalcAtomFromInt int -- simplify identifiers wrapped in an atom
        LCalcTermFromApp (LCalcAppLiteral (LCalcAtomLiteral (LCalcTermFromApp app)) LCalcApp'Empty) ->
            LCalcAtomLiteral $ LCalcTermFromApp app -- simplify useless parens around terms
        _ ->
            LCalcAtomLiteral res
        where
            res = evaluateTerm term
    LCalcAtomFromString str -> atom -- should never see
    LCalcAtomFromInt ind -> atom

evaluateApp' :: LCalcApp' -> LCalcApp'
evaluateApp' (LCalcApp'Literal atom app) = case app of -- input shouldn't be empty
    LCalcApp'Empty -> -- if the second part is empty
        LCalcApp'Literal (evaluateAtom atom) app
    LCalcApp'Literal atom' app' ->
        case (atom, atom') of
            (LCalcAtomLiteral (LCalcTermLiteral str term), _) -> -- left is a term literal
                evaluateApp' $ LCalcApp'Literal (LCalcAtomLiteral (substitute (evaluateAtom atom') (evaluateTerm term))) app'
            (_, _) -> -- left is not a term literal
                LCalcApp'Literal atom (evaluateApp' app)

evaluateApp :: LCalcApp -> LCalcApp
evaluateApp (LCalcAppLiteral atom app) = case app of
    LCalcApp'Empty -> -- if the second part is empty
        LCalcAppLiteral (evaluateAtom atom) app
    LCalcApp'Literal atom' app' ->
        case (atom, atom') of
            (LCalcAtomLiteral (LCalcTermLiteral str term), _) -> -- left is a term literal
                evaluateApp $ LCalcAppLiteral (LCalcAtomLiteral (substitute (evaluateAtom atom') (evaluateTerm term))) app'
            (_, _) -> -- left is an identifier, can't simplify further
                LCalcAppLiteral atom (evaluateApp' app)





main :: IO ()
main = do
    inp <- getLine
    let (Just ast) = parse inp
    let astNew = makeDeBruijn ast
    print ast
    putStrLn ""
    print astNew
    putStrLn ""
    print $ evaluateTerm astNew
