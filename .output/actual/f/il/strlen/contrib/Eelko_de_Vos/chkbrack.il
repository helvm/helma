[ Comment "chkbrack.f"
, Comment "checks brackets"
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 0
, Store
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 1
, Store
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 2
, Store
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 40 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Add ) )
        , Ref 0
        , Store
        ]
    , Cond
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 41 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Ref 0
        , Store
        ]
    , Cond
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 91 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Add ) )
        , Ref 1
        , Store
        ]
    , Cond
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 93 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Ref 1
        , Store
        ]
    , Cond
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 123 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Add ) )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 125 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Ref 2
        , Store
        ]
    , Cond
    ]
, While
, Str "#of () "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( Cons 0 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Inst
    ( IAL ( Unary BNot ) )
, Inst ( IAL Dup )
, Inst
    ( IAL ( Unary BNot ) )
, Lambda
    [ Str "is ok.
    " ]
, Cond
, Lambda
    [ Str "NOT ok !!!
    " ]
, Cond
, Str "#of [] "
, Ref 1
, Fetch
, Inst
    ( IAL
        ( Cons 0 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Inst
    ( IAL ( Unary BNot ) )
, Inst ( IAL Dup )
, Inst
    ( IAL ( Unary BNot ) )
, Lambda
    [ Str "is ok.
    " ]
, Cond
, Lambda
    [ Str "NOT ok !!!
    " ]
, Cond
, Str "#of {} "
, Ref 2
, Fetch
, Inst
    ( IAL
        ( Cons 0 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Inst
    ( IAL ( Unary BNot ) )
, Inst ( IAL Dup )
, Inst
    ( IAL ( Unary BNot ) )
, Lambda
    [ Str "is ok.
    " ]
, Cond
, Lambda
    [ Str "NOT OK !!!
    " ]
, Cond
]