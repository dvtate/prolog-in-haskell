module CS440.RE (
    RegExpr, capture, capture'
) where


-- Regular expressions
data RegExpr a
    = RE_const a
    | RE_or [RegExpr a]
    | RE_and [RegExpr a]
    | RE_star (RegExpr a)
    | RE_in_set [a]           -- [...] - any symbol in list
    | RE_empty                -- epsilon - empty (no symbols)
        deriving (Eq, Read, Show)

-- Regular expressions have their own types for token, reversed token,
-- and input.  (They're used to make type annotations more understandable.)
--
type RE_Token a = [a]
type RE_RevToken a = [a]
type RE_Input a = [a]

-- capture matches a regular expression against some input; on success,
-- it returns the matching token (= list of input symbols) and the
-- remaining input.  E.g. capture abcd abcdef = Just(abcd, ef)
--
capture  :: Eq a => RegExpr a -> RE_Input a -> Maybe(RE_Token a, RE_Input a)

-- top-level capture routine calls assistant with empty built-up token to start
--
capture rexp input = case capture' rexp ([], input) of
    Nothing -> Nothing
    Just (revtoken, input') -> Just (reverse revtoken, input')

-- capture' rexp (partial_token input) matches the expression against
-- the input given a reversed partial token; on success, it returns
-- the completed token and remaining input. The token is in reverse
-- order. E.g., capture' cd (ba, cdef) = (dcba, ef)
--
capture' :: Eq a => RegExpr a -> (RE_RevToken a, RE_Input a) -> Maybe(RE_RevToken a, RE_Input a)

-- *** STUB *** add code for RE_const, RE_or, RE_and, RE_in_set, RE_star, RE_empty
-- *** and for any other kinds of regular expressions you need.
--

capture' (RE_const _) (_, []) = Nothing
capture' (RE_const symbol) (revtoken, head_inp : input')
    | head_inp == symbol = Just((head_inp : revtoken), input')
    | otherwise = Nothing

-- the OR of no clauses fails
capture' (RE_or []) _ = Nothing
capture' (RE_or (rexp : regexprs')) (revtoken, input) =
    case capture' rexp (revtoken, input) of
        Nothing -> capture' (RE_or regexprs') (revtoken, input)
        ok @ (Just (revtoken', input')) -> ok

-- the AND of no clauses succeeds
capture' (RE_and []) (revtoken, input) = Just (revtoken, input)
capture' (RE_and (rexp : regexprs)) (revtoken, input) =
    case capture' rexp (revtoken, input) of
        Nothing -> Nothing 
        ok @ (Just (revtoken', input')) -> capture' (RE_and regexprs) (revtoken', input')

capture' (RE_in_set (symbol : symbols')) (revtoken, (head_inp : input'))
    | head_inp == symbol     = Just((head_inp : revtoken), input')
    | (length symbols') == 0 = Nothing
    | otherwise              = capture' (RE_in_set symbols') (revtoken, (head_inp : input'))

capture' (RE_star rexp) (revtoken, input) = 
    case cstar rexp input [] of
        ([], input') -> Nothing
        (tok, input') -> Just ((reverse tok) ++ revtoken, input')
    where 
        cstar :: Eq a => RegExpr a -> [a] -> [a] -> ([a], [a])
        cstar rxp input ret = 
            case capture rxp input of
                Just (cv, input') -> cstar rxp input' (ret ++ cv)
                Nothing -> (ret, input)

capture' (RE_empty) (revtoken, input) = Just(revtoken, input)