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
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 0 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        ]
    , Cond
    , Inst
        ( IAL ( Unary BNot ) )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 0 )
            )
        , Inst
            ( IAL
                ( Cons 2 )
            )
        , Ref 14
        , Lambda
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
                ( IAL
                    ( Cons 7 )
                )
            , Ref 14
            , Inst
                ( IAL
                    ( Cons 7 )
                )
            , Ref 14
            , Inst
                ( IAL ( Binary BOr ) )
            , Inst
                ( IAL ( Unary Neg ) )
            , Inst
                ( IAL
                    ( Cons 1 )
                )
            , Inst
                ( IAL ( Binary Sub ) )
            , Inst
                ( IAL ( Binary BAnd ) )
            , Lambda
                [ Inst
                    ( IAL
                        ( SStatic 2 Move )
                    )
                , Inst
                    ( IAL
                        ( Cons 1 )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( Cons 7 )
                    )
                , Ref 14
                , Inst
                    ( IAL ( Binary BOr ) )
                , Inst
                    ( IAL
                        ( SStatic 0 Copy )
                    )
                , Inst
                    ( IAL ( Binary Add ) )
                , Inst
                    ( IAL
                        ( Cons 2 )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( Cons 7 )
                    )
                , Ref 14
                , Inst
                    ( IAL ( Binary BOr ) )
                , Inst
                    ( IAL
                        ( Cons 2 )
                    )
                , Inst
                    ( IAL ( Binary Div ) )
                , Inst
                    ( IAL
                        ( Cons 3 )
                    )
                , Ref 14
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
                        ( Cons 7 )
                    )
                , Ref 14
                , Inst
                    ( IAL ( Binary BAnd ) )
                , Inst
                    ( IAL
                        ( Cons 6 )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( Cons 1 )
                    )
                , Inst
                    ( IAL ( Binary Sub ) )
                , Ref 16
                , Fetch
                , Exec
                , Inst
                    ( IAL ( Binary Add ) )
                , Inst
                    ( IAL
                        ( SStatic 2 Move )
                    )
                , Inst
                    ( IAL
                        ( SStatic 2 Move )
                    )
                ]
            , Cond
            , Inst
                ( IAL ( Unary Neg ) )
            , Inst
                ( IAL
                    ( Cons 1 )
                )
            , Inst
                ( IAL ( Binary Sub ) )
            , Inst
                ( IAL ( Binary BAnd ) )
            ]
        , While
        , Inst ( IAL Discard )
        ]
    , Cond
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Inst ( IAL Discard )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Inst ( IAL Discard )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Inst ( IAL Discard )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Inst ( IAL Discard )
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
        ( Cons 48 )
    )
, Inst
    ( IAL ( Binary Sub ) )
, Inst
    ( IAL
        ( Cons 0 )
    )
, Inst
    ( IAL
        ( SStatic 0 Copy )
    )
, Inst
    ( IAL
        ( SStatic 2 Move )
    )
, Inst
    ( IAL
        ( SStatic 0 Copy )
    )
, Inst
    ( IAL
        ( Cons 1 )
    )
]