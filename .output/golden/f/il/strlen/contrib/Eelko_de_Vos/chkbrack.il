[ Comment "chkbrack.f"
, Comment "checks brackets"
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Ref 1
, Store
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Ref 2
, Store
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Unary Neg ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    ]
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 40 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Add ) )
            )
        , Ref 0
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 41 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Sub ) )
            )
        , Ref 0
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 91 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Add ) )
            )
        , Ref 1
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 93 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Sub ) )
            )
        , Ref 1
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 123 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Add ) )
            )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 125 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Sub ) )
            )
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
        ( SAL
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
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
        ( SAL
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
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
        ( SAL
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
, Lambda
    [ Str "is ok.
    " ]
, Cond
, Lambda
    [ Str "NOT OK !!!
    " ]
, Cond
]