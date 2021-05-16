module Main where

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





substituteAtom :: String -> LCalcAtom -> LCalcAtom -> LCalcAtom
substituteAtom str atom atom' = case atom' of
    LCalcAtomLiteral term -> LCalcAtomLiteral $ substituteTerm str atom term
    LCalcAtomFromString str' -> if str' == str then atom else atom' -- this is where we replace the occurence

substituteApp' :: String -> LCalcAtom -> LCalcApp' -> LCalcApp'
substituteApp' str atom app = case app of
    LCalcApp'Literal atom' app' -> LCalcApp'Literal (substituteAtom str atom atom') (substituteApp' str atom app')
    LCalcApp'Empty -> app

substituteApp :: String -> LCalcAtom -> LCalcApp -> LCalcApp
substituteApp str atom (LCalcAppLiteral atom' app') =
    LCalcAppLiteral (substituteAtom str atom atom') (substituteApp' str atom app')

substituteTerm :: String -> LCalcAtom -> LCalcTerm -> LCalcTerm
substituteTerm str atom term = case term of
    LCalcTermFromApp app -> LCalcTermFromApp $ substituteApp str atom app
    LCalcTermLiteral str' term' -> LCalcTermLiteral str' (substituteTerm str atom term')





evaluateTerm :: LCalcTerm -> LCalcTerm
evaluateTerm term = case term of
    LCalcTermLiteral str term' -> LCalcTermLiteral str (evaluateTerm term')
    LCalcTermFromApp app -> LCalcTermFromApp $ evaluateApp app

evaluateAtom :: LCalcAtom -> LCalcAtom
evaluateAtom atom = case atom of
    LCalcAtomLiteral term -> case res of
        LCalcTermFromApp (LCalcAppLiteral (LCalcAtomFromString str) LCalcApp'Empty) ->
            LCalcAtomFromString str -- simplify identifiers wrapped in an atom
        LCalcTermFromApp (LCalcAppLiteral (LCalcAtomLiteral (LCalcTermFromApp app)) LCalcApp'Empty) ->
            LCalcAtomLiteral $ LCalcTermFromApp app -- simplify useless parens around terms
        _ ->
            LCalcAtomLiteral res
        where
            res = evaluateTerm term
    LCalcAtomFromString str -> atom

evaluateApp' :: LCalcApp' -> LCalcApp'
evaluateApp' (LCalcApp'Literal atom app) = case app of -- input shouldn't be empty
    LCalcApp'Empty -> -- if the second part is empty
        LCalcApp'Literal (evaluateAtom atom) app
    LCalcApp'Literal atom' app' ->
        case (atom, atom') of
            (LCalcAtomLiteral (LCalcTermLiteral str term), _) -> -- left is a term literal
                evaluateApp' $ LCalcApp'Literal (LCalcAtomLiteral (substituteTerm str (evaluateAtom atom') (evaluateTerm term))) app'
            (_, _) -> -- left is not a term literal
                LCalcApp'Literal atom (evaluateApp' app)

evaluateApp :: LCalcApp -> LCalcApp
evaluateApp (LCalcAppLiteral atom app) = case app of
    LCalcApp'Empty -> -- if the second part is empty
        LCalcAppLiteral (evaluateAtom atom) app
    LCalcApp'Literal atom' app' ->
        case (atom, atom') of
            (LCalcAtomLiteral (LCalcTermLiteral str term), _) -> -- left is a term literal
                evaluateApp $ LCalcAppLiteral (LCalcAtomLiteral (substituteTerm str (evaluateAtom atom') (evaluateTerm term))) app'
            (_, _) -> -- left is an identifier, can't simplify further
                LCalcAppLiteral atom (evaluateApp' app)





main :: IO ()
main = do
    inp <- getLine
    let (Just ast) = parse inp

    print ast
    putStrLn ""
    print ast
    putStrLn ""
    print $ evaluateTerm ast
    putStrLn ""
    print $ evaluateTerm ast
