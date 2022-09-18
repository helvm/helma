[ Comment " lambda calculus in FALSE
    [] blocks are lambda functions already, so it isn't all that hard... "
, Lambda
    [ Inst ( IAL Swap )
    , Inst ( IAL Discard )
    ]
, Ref 0
, Store
, Comment " true "
, Lambda
    [ Inst ( IAL Discard ) ]
, Ref 1
, Store
, Comment " false "
, Lambda
    [ Ref 0
    , Fetch
    , Ref 1
    , Fetch
    , Inst ( IAL Rot )
    , Exec
    ]
, Ref 3
, Store
, Comment " not "
, Lambda
    [ Ref 1
    , Fetch
    , Inst ( IAL Rot )
    , Inst ( IAL Rot )
    , Inst ( IAL Swap )
    , Exec
    ]
, Ref 4
, Store
, Comment " and "
, Lambda
    [ Inst ( IAL Swap )
    , Ref 0
    , Fetch
    , Inst ( IAL Swap )
    , Exec
    ]
, Ref 5
, Store
, Comment " or "
, Lambda
    [ Inst ( IAL Dup )
    , Ref 3
    , Fetch
    , Exec
    , Inst ( IAL Swap )
    , Inst ( IAL Rot )
    , Exec
    ]
, Ref 6
, Store
, Comment " xnor "
, Lambda
    [ Inst ( IAL Dup )
    , Ref 0
    , Fetch
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Str "true"
        , Inst
            ( IAL
                ( Cons 10 )
            )
        , Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , Cond
    , Ref 1
    , Fetch
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Str "false"
        , Inst
            ( IAL
                ( Cons 10 )
            )
        , Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , Cond
    ]
, Ref 25
, Store
, Comment " print true/false for funcs a/b "
, Str "not testcase:
  "
, Ref 0
, Fetch
, Ref 3
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 1
, Fetch
, Ref 3
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Str "and testcase:
  "
, Ref 0
, Fetch
, Ref 0
, Fetch
, Ref 4
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Comment " this would be: print(and(true,true)) "
, Ref 0
, Fetch
, Ref 1
, Fetch
, Ref 4
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 1
, Fetch
, Ref 0
, Fetch
, Ref 4
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 1
, Fetch
, Ref 1
, Fetch
, Ref 4
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Str "or testcase:
  "
, Ref 0
, Fetch
, Ref 0
, Fetch
, Ref 5
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 0
, Fetch
, Ref 1
, Fetch
, Ref 5
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 1
, Fetch
, Ref 0
, Fetch
, Ref 5
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 1
, Fetch
, Ref 1
, Fetch
, Ref 5
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Str "xnor testcase:
  "
, Comment " equality "
, Ref 0
, Fetch
, Ref 0
, Fetch
, Ref 6
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 0
, Fetch
, Ref 1
, Fetch
, Ref 6
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 1
, Fetch
, Ref 0
, Fetch
, Ref 6
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
, Ref 1
, Fetch
, Ref 1
, Fetch
, Ref 6
, Fetch
, Exec
, Ref 25
, Fetch
, Exec
]