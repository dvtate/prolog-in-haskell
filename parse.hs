

-- Problem → { Equations }
-- Equations → Equation | Equation , Equations
-- Equation → Expr = Expr
-- Expr → … as before, with +, * ...

-- Expr   -> Term \+ Ttail
-- Ttail  -> \+ Term Ttail | empty
-- Term   -> Factor Ftail
-- Ftail  -> \* Factor Ftail | empty
-- Factor -> Id_or_Call | Paren | Negative
-- Paren  -> \( Expr \)
-- Negative -> \- Factor
--
-- Id_or_Call -> Id (Arguments | empty)
-- Arguments  -> \( Expr Argtail \)
-- Argtail    -> \, Expr Argtail | empty


-- Factor → identifier | variable
-- Factor → constant | \( Expr \)| Function_call
-- Function_call → identifier \( (Arguments|ε) \) // includes calls like f()
-- Arguments → Term | Term , Arguments


-- recursive descent parser...