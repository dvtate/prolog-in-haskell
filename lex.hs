module CS440.Lex where

-- import CS440.RE


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

capture' (RE_in_set []) _ = Nothing
capture' (RE_in_set _) (_, []) = Nothing
capture' (RE_in_set (symbol : symbols')) (revtoken, (head_inp : input'))
    | head_inp == symbol     = Just((head_inp : revtoken), input')
    | (length symbols') == 0 = Nothing
    | otherwise              = capture' (RE_in_set symbols') (revtoken, (head_inp : input'))


capture' (RE_star rexp) (revtoken, input) = 
    case cstar rexp input [] of
        ([], input') -> Just(revtoken, input')
        (tok, input') -> Just ((reverse tok) ++ revtoken, input')
    where 
        cstar :: Eq a => RegExpr a -> [a] -> [a] -> ([a], [a])
        cstar rxp input ret = 
            case capture rxp input of
                Just (cv, input') -> cstar rxp input' (ret ++ cv)
                Nothing -> (ret, input)

capture' (RE_empty) (revtoken, input) = Just(revtoken, input)



-- paying for 'purity' w/ performance...
digit1_9        = RE_in_set "123456789"
digit           = RE_in_set "0123456789"
int_literal     = RE_and [digit1_9, RE_star digit]

operator        = RE_in_set "+-*/,(){}="

lc_letter       = RE_in_set "abcdefghijklmnopqrstuvwxyz"
uc_letter       = RE_in_set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letter          = RE_or [ lc_letter, uc_letter]

alphanum        = RE_or [ lc_letter, uc_letter, RE_const '_', digit ]
identifier      = RE_and [ lc_letter, RE_star alphanum ]
variable        = RE_and [ uc_letter, RE_star alphanum ]

spaces          = RE_in_set " \n\t"

-- data Token  = Const String    -- integer constant
--                 | Id String    -- labelled constant identifier
--                 | Var String   -- variable identifier
--                 | Op Char      -- operator
--                 | Space        -- whitespace to ignore
--     deriving (Show, Read, Eq)

-- scan_rexps are the regexprs to use to break up the input.  The format of each
-- pair is (regexpr, fcn); if capture regexpr produces a string str, apply the function
-- to it to get a token.  scan_rexps is an infinite cycle of the regular expressions
-- the scanner is looking for
scanners :: [ (RegExpr Char, a -> (String, a) ) ]
scanners = 
    [ (int_literal, \t -> ("const", t))
    , (identifier,  \t -> ("id", t))
    , (variable,    \t -> ("var", t))
    , (operator,    \t -> ("op", t))
    , (spaces,      \t -> ("space", t)) ]

-- resulting tokens (if any).  It calls a helper routine with an empty list
-- of result tokens.
scan :: String -> [(String, String)]
scan input = case scan' scanners [] input of
    Just(tokens, remaining) -> tokens
    Nothing -> []

-- scan' rexps revtokens input applies the first regular expression of
-- the list rexps to the input.  If the match succeeds, the captured
-- string is passed to a token-making routine associated with the reg
-- expr, and we add the new token to the head of our reversed list of
-- tokens found so far.  Exception: We don't add a Space token.
-- procedural equivalent:
    -- runs all the token regexpressions on the input
    --  finds longest token
    --  if longest token is `Nothing` throw syntax error
    --  push longest onto return
    -- scan input produces Just (list of tokens, remaining input)
-- scan' :: [ (RegExpr Char, a -> (String, a) ) ] -> [(String, String)] -> [Char] -> Maybe ([(String, String)], [Char])
scan' _ revtokens [] = Just(filter (\t -> (fst t) /= "space") (reverse revtokens), [])
scan' patterns revt input
    | snd ret == input = error ("Lexical scan failed on `" ++ input ++ "`\n") --Just(filter (\t -> t /= Space) (reverse revt), snd ret)
    | otherwise = scan' patterns ((fst ret) : revt) (snd ret)
    where
        -- find index of maximum element in list
        maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

        -- invoke scanner on input string
        -- scan_p :: (RegExpr Char, a -> (String, a)) -> [Char] -> (Int, ((String, a), RE_Input Char))
        scan_p scanner inp = 
            case capture (fst scanner) inp of
                Just(tok, input') -> (length tok, ((snd scanner) tok, input'))
                Nothing -> (0, (("space", ""), input))

        -- invoke all the scanners on the input
        candToks = map (\s -> scan_p s input) patterns

        -- find longest token
        ret = snd (candToks !! (maxIndex (map (\r -> fst r) candToks)))



---------- Parsing:

---------- UTILITIES --------------------------------------------------
-- maybe next token is same value specified
parse_token_v :: String -> [(String, String)] -> Maybe ((String, String), [(String, String)])
parse_token_v val (h:rem)
    | val == (snd h) = Just(h, rem)
    | otherwise = Nothing
parse_token_v _ [] = Nothing

-- maybe next token is same type specified
parse_token_t :: String -> [(String, String)] -> Maybe ((String, String), [(String, String)])
parse_token_t t (h:rem)
    | t == (fst h) = Just(h, rem)
    | otherwise = Nothing
parse_token_t _ [] = Nothing

--
-- The bind routine lets us take a Just val and run a function on the val.
-- If given Nothing instead, bind also yields Nothing.
--
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just val) f = f val

--
-- The fails routine lets you call a function() if given Nothing; if
-- given Just val, the fails routine just yields that.
--
fails :: Maybe a -> (() -> Maybe a) -> Maybe a
fails Nothing f = f()
fails ok _ = ok

--
-- make_tail builds an expr using a term and term tail; it builds a term
-- using a factor and factor tail.  If the tail is empty, make_tail just
-- returns the given term or factor.  This optimization reduces the
-- number of skinny paths through the parse tree, which becomes shorter.
--
-- make_tail :: (Ptree -> Ptree -> Ptree) -> Ptree -> Ptree -> Ptree
-- make_tail _ ptree Empty = ptree
-- make_tail build ptree tailtree =
--     build ptree tailtree

-- AST
data Expr = 
    Empty
    | Binary Char Expr Expr     -- +, -, etc.
    | Negative Expr           -- !, -, etc.?
    | FnCall String [Expr]      -- f(a,b,c)
    | Id String                 -- xY2_z
    | Var String                -- XY2_z
    | Num String                -- 123
    deriving (Eq, Show, Read)

type Equation = (Expr, Expr)
    -- deriving (Eq, Show, Read)

type Problem = [Equation]
    -- deriving (Eq, Show, Read)


-- Problem → { Equations }
-- Equations → Equation EquationsTail
-- EquationsTail -> , Equations | empty
-- Equation → Expr = Expr

-- Expr   -> Term Ttail
-- Ttail  -> \+ Term Ttail | empty
-- Term   -> Factor Ftail
-- Ftail  -> \* Factor Ftail | empty
-- Paren  -> \( Expr \)
-- Negative -> \- Factor
-- Factor → identifier | variable | constant | Paren | Function_call | Negative
-- Function_call → identifier (Arguments)
-- Arguments  -> Expr Argtail | empty
-- Argtail    -> \, Expr Argtail | empty

-- parse :: String -> Problem
parse input = parse_problem (scan input)

-- Problem -> { Equations }
parse_problem input = 
    (parse_token_v "{" input)    `bind` (\ (_, in1) ->
        (parse_equations in1)       `bind` (\ (ret, in2) ->
            (parse_token_v "}" in2)      `bind` (\ (_, in3) ->
                Just(ret, in3)
            )
        )
    )

-- Equations -> Equation EquationsTail
parse_equations input =
    (parse_equation input)      `bind` (\ (eq, in1) ->
        (parse_equations_tail in1)  `bind` (\ (eqs, in2) ->
            Just(eq : eqs, in2)
        )
    )

-- EquationsTail -> , Equations | empty
parse_equations_tail input =
    (parse_token_v "," input)   `bind` (\ (_, in1) -> 
        (parse_equations in1)   `bind` (\ (eqs, in2) ->
            Just(eqs, in2)
        )
    ) `fails` (\() -> Just([], input))

-- Equation -> Expr = Expr
parse_equation input = 
    (parse_expr input) `bind` (\ (e1, in1) ->
        (parse_token_v "=" in1) `bind` (\ (_, in2) ->
            (parse_expr in2) `bind` (\ (e2, in3) ->
                Just((e1, e2), in3)
            )
        )
    )

-- Expr -> Term \+ Ttail
parse_expr input =
    (parse_term input) `bind` (\ (t1, in1) ->
        (parse_term_tail in1 t1) `bind` (\ (tt, in2) ->
            Just(tt, in2)
        ) `fails` (\ () -> 
            Just(t1, in1)
        )
    )


-- Term   -> Factor Ftail
parse_term input = 
    (parse_factor input) `bind` (\ (f, in1) -> 
        (parse_factor_tail in1) `bind` (\ (ft, in2) ->
            Just(Binary '*' f ft, in2)
        ) `fails` (\ () -> 
            Just(f, in1)
        )
    )

-- Ttail  -> \+ Term | empty
parse_term_tail input term = 
    (parse_token_v "+" input) `bind` (\ (_, in1) -> 
        (parse_term in1) `bind` (\ (t, in2) -> 
            Just(Binary '+' term t, in2)
        )
    ) `fails` (\ () -> Just(term, input))
    

-- Factor →  Function_call | identifier | variable | constant | Paren | Negative
parse_factor input = 
    (parse_function_call input) 
    `fails` (\() -> parse_id input)
    `fails` (\() -> parse_variable input)
    `fails` (\() -> parse_const input)
    `fails` (\() -> parse_paren input)
    `fails` (\() -> parse_negative input)
    
-- Ftail  -> \* Factor Ftail | empty
parse_factor_tail input =
    (parse_token_v "*" input) `bind` (\ (_, in1) ->
        (parse_term in1) -- AKA: Ftail -> \* Term | empty
    )

-- parse identifier
parse_id input = 
    (parse_token_t "id" input) `bind` (\ (token, in1) ->
        Just(Id (snd token), in1)
    )

parse_const input = 
    (parse_token_t "const" input) `bind` (\ (token, in1) -> 
        Just (Num (snd token), in1)
    )

parse_variable input =
    (parse_token_t "var" input) `bind` (\ (token, in1) -> 
        Just (Var (snd token), in1)
    )

-- Function_call → identifier (Arguments)
parse_function_call input =
    (parse_id input) `bind` (\ (Id id, in1) -> 
        (parse_token_v "(" in1) `bind` (\ (_, in2) ->
            (parse_arguments in2) `bind` (\ (args, in3) -> 
                (parse_token_v ")" in3) `bind` (\ (_, in4) -> 
                    Just(FnCall id args, in4)
                )
            )
        )
    )

-- Arguments  -> Expr Argtail | empty
parse_arguments input =
    (parse_expr input) `bind` (\ (e, in1) ->
        (parse_arguments_tail in1) `bind` (\ (es, in2) -> 
            Just(e : es, in2)    
        )
    )

-- Argtail    -> \, Expr Argtail | empty
parse_arguments_tail input =
    (parse_token_v "," input) `bind` (\ (_, in1) ->
        (parse_arguments in1)     
    ) `fails` (\ () -> 
        Just([], input)
    )

-- \( expr \)
parse_paren input = 
    (parse_token_v "(" input) `bind` (\ (_, in1) ->
        (parse_expr in1) `bind` (\ (e, in2) -> 
            (parse_token_v ")" in2) `bind` (\ (_, in3) ->
                Just(e, in3)
            )
        )
    )

-- - Factor
parse_negative input = 
    (parse_token_v "-" input) `bind` (\ (_, in1) ->
        (parse_factor in1) `bind` (\ (f, in2) -> 
            Just (Negative f, in2)
        )    
    )
