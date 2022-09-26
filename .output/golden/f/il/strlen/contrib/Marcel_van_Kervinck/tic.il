[ Comment " tic tac toe "
, Comment " written in false "
, Comment " by Marcel van Kervinck "
, Comment " e-mail <marcelk@stack.urc.tue.nl> "
, Comment " June 1994 "
, Comment " for all "
, Lambda
    [ Lambda
        [ Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Inst
            ( IAL
                ( Cons 0 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst
            ( IAL ( Unary BNot ) )
        ]
    , Lambda
        [ Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Inst
            ( IAL ( Unary Neg ) )
        , Inst
            ( IAL ( Binary BAnd ) )
        , Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Inst
            ( IAL ( Unary Neg ) )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Inst
            ( IAL
                ( SStatic 2 Move )
            )
        , Inst
            ( IAL ( Binary BAnd ) )
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Inst
            ( IAL
                ( Cons 2 )
            )
        , Ref 14
        , Exec
        ]
    , While
    , Inst ( IAL Discard )
    , Inst ( IAL Discard )
    ]
, Ref 5
, Store
, Comment " print board "
, Lambda
    [ Lambda
        [ Str "   "
        , Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Ref 23
        , Fetch
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Inst
            ( IAL ( Binary Div ) )
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Ref 14
        , Fetch
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Inst
            ( IAL ( Binary Div ) )
        , Lambda
            [ Inst
                ( IAL
                    ( Cons 32 )
                )
            , Inst
                ( IAL ( SIO OutputChar ) )
            , Inst
                ( IAL
                    ( SStatic 0 Copy )
                )
            , Inst
                ( IAL
                    ( Cons 4 )
                )
            , Ref 14
            , Inst
                ( IAL ( Binary BAnd ) )
            , Inst
                ( IAL
                    ( Cons 0 )
                )
            , Inst
                ( IAL ( Binary LEQ ) )
            , Inst
                ( IAL ( Unary BNot ) )
            , Inst
                ( IAL
                    ( SStatic 0 Copy )
                )
            , Lambda
                [ Str "O"
                , Inst
                    ( IAL
                        ( SStatic 1 Move )
                    )
                , Inst ( IAL Discard )
                ]
            , Cond
            , Inst
                ( IAL ( Unary BNot ) )
            , Lambda
                [ Inst
                    ( IAL
                        ( Cons 4 )
                    )
                , Ref 14
                , Inst
                    ( IAL ( Binary BAnd ) )
                , Inst
                    ( IAL
                        ( Cons 0 )
                    )
                , Inst
                    ( IAL ( Binary LEQ ) )
                , Inst
                    ( IAL ( Unary BNot ) )
                , Inst
                    ( IAL
                        ( SStatic 0 Copy )
                    )
                , Lambda
                    [ Str "X" ]
                , Cond
                , Inst
                    ( IAL ( Unary BNot ) )
                , Lambda
                    [ Str "." ]
                , Cond
                ]
            , Cond
            ]
        , Inst
            ( IAL
                ( Cons 7 )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( IAL
                ( Cons 10 )
            )
        , Inst
            ( IAL ( SIO OutputChar ) )
        , Inst ( IAL Discard )
        , Inst ( IAL Discard )
        ]
    , Str "7 8 9"
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 64 )
        )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Exec
    , Str "4 5 6"
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 8 )
        )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Exec
    , Str "1 2 3"
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Exec
    ]
, Ref 1
, Store
, Comment " test three-in-a-row "
]