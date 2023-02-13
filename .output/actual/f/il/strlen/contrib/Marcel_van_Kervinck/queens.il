[ Comment " queens "
, Comment " solves the problem of placing n queens on a "
, Comment " n*n chessboard without attacking eachother "
, Comment " written in false "
, Comment " by Marcel van Kervinck "
, Comment " e-mail <marcelk@stack.urc.tue.nl> "
, Comment " June 1994 "
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
                ( Cons 0 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 1 Move )
                )
            )
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( Cons 0 )
                )
            )
        , Inst
            ( IAL
                ( SAL
                    ( Cons 2 )
                )
            )
        , Ref 14
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
                        ( SStatic 0 Copy )
                    )
                )
            , Inst
                ( IAL
                    ( SAL ( Unary Neg ) )
                )
            , Inst
                ( IAL
                    ( SAL ( Binary BAnd ) )
                )
            , Inst
                ( IAL
                    ( SAL
                        ( SStatic 0 Copy )
                    )
                )
            , Inst
                ( IAL
                    ( SAL
                        ( Cons 7 )
                    )
                )
            , Ref 14
            , Inst
                ( IAL
                    ( SAL
                        ( Cons 7 )
                    )
                )
            , Ref 14
            , Inst
                ( IAL
                    ( SAL ( Binary BOr ) )
                )
            , Inst
                ( IAL
                    ( SAL ( Unary Neg ) )
                )
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
            , Inst
                ( IAL
                    ( SAL ( Binary BAnd ) )
                )
            , Lambda
                [ Inst
                    ( IAL
                        ( SAL
                            ( SStatic 2 Move )
                        )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 1 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SAL ( Binary BOr ) )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( SStatic 0 Copy )
                        )
                    )
                , Inst
                    ( IAL
                        ( SAL ( Binary Add ) )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 2 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SAL ( Binary BOr ) )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 2 )
                        )
                    )
                , Inst
                    ( IAL
                        ( SAL ( Binary Div ) )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 3 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SAL ( Unary Neg ) )
                    )
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
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SAL ( Binary BAnd ) )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( Cons 6 )
                        )
                    )
                , Ref 14
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
                , Ref 16
                , Fetch
                , Exec
                , Inst
                    ( IAL
                        ( SAL ( Binary Add ) )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( SStatic 2 Move )
                        )
                    )
                , Inst
                    ( IAL
                        ( SAL
                            ( SStatic 2 Move )
                        )
                    )
                ]
            , Cond
            , Inst
                ( IAL
                    ( SAL ( Unary Neg ) )
                )
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
            , Inst
                ( IAL
                    ( SAL ( Binary BAnd ) )
                )
            ]
        , While
        , Inst
            ( IAL ( SAL Discard ) )
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
            )
        )
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
            )
        )
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
            )
        )
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
            )
        )
    , Inst
        ( IAL ( SAL Discard ) )
    ]
, Ref 16
, Store
, Str "enter number of queens [0..9] "
, Ref 1
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 1
, Inst
    ( IAL
        ( SAL
            ( Cons 48 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary Sub ) )
    )
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 2 Move )
        )
    )
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
]