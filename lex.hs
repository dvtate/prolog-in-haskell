module CS440.Lex (
    next_token,
    Token,
    Ptree,
) where

import CS440.RE (
    capture
)

import Data.List (
    any
)

-- tokens that can interrupt an identifier
-- and need to be their own token
ops = "(){},=+-/*"

-- ignore whitespace
whitespace = "\t\n\r "

-- fn tokenize(s): 
--     let tok, rem;
--     while ([tok, rem] = next_token(s) && rem)
--         s = rem;
--     return tok;

-- fn next_token(s):

tokenize str = 


-- 
next_token str =
    let dsstr = dropWhile isSpace str in
        case next_token' dsstr [] of
            Nothing -> Nothing
            ok @ (Just (rt, ))


next_token' str ret =


-- scan: 
-- runs all the token regexpressions on the input
--  finds longest token
--  if longest token is `Nothing` throw syntax error
--  push longest onto return
-- scan input produces Just (list of tokens, remaining input)
--
data Token = Const Int | Id String | Op String | Punct String | Space
    deriving (Show, Read, Eq)


-- scan input runs the scan algorithm on the input and returns the list of
-- resulting tokens (if any).  It calls a helper routine with an empty list
-- of result tokens.
scan :: String -> [Token]
scan input = case scan' scan_rexps [] input of
    Just(tokens, remaining) -> tokens
    Nothing -> []


-- scan' rexps revtokens input applies the first regular expression of
-- the list rexps to the input.  If the match succeeds, the captured
-- string is passed to a token-making routine associated with the reg
-- expr, and we add the new token to the head of our reversed list of
-- tokens found so far.  Exception: We don't add a Space token.
scan' _ revtokens [] = Just (filter (\t -> t /= Space) (reverse revtokens), [])
scan' (p : ps) revtokens input =
    case capture (fst p) input of
        Just(tok, input') -> scan' ps ((snd p tok) : revtokens) input'
        Nothing -> scan' ps revtokens input


-- scan_rexps are the regexprs to use to break up the input.  The format of each
-- pair is (regexpr, fcn); if capture regexpr produces a string str, apply the function
-- to it to get a token.  scan_rexps is an infinite cycle of the regular expressions
-- the scanner is looking for
-- write tokenizer
scan_rexps = [ (num_const, \t -> Const (read t :: Int)), 
    (identifier, Id), (operator, Op), (punctuation, Punct), (spaces, \_ -> Space) ]


num_const       = RE_and [digit1_9, RE_star digit]
digit1_9        = RE_in_set "123456789"
digit           = RE_in_set "0123456789"
operator        = RE_in_set "+-*/"
punctuation     = RE_in_set "[](){},;:.?!&#$%"
lc_letter       = RE_in_set "abcdefghijklmnopqrstuvwxyz"
uc_letter       = RE_in_set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letter          = RE_or [ lc_letter, uc_letter ]
identifier	    = RE_and [ lc_letter, RE_star letter ]
spaces          = RE_in_set " \n\t"



