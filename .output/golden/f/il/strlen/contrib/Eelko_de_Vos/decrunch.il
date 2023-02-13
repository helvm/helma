[ Comment "Depacker, by E. de Vos"
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
                ( Cons 10 )
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
, Lambda []
, While
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 0
, Store
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Unary Neg ) )
        )
    , Ref 0
    , Fetch
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    ]
, Comment "while a<>EOF"
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( Cons 169 )
            )
        )
    , Ref 0
    , Fetch
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
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    , Lambda
        [ Comment "if char!=escapecode"
        , Ref 0
        , Fetch
        , Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , Cond
    , Lambda
        [ Comment "if char=escapecode"
        , Inst
            ( IAL ( SIO InputChar ) )
        , Ref 2
        , Store
        , Comment "read counter"
        , Inst
            ( IAL ( SIO InputChar ) )
        , Ref 0
        , Store
        , Comment "read char"
        , Lambda
            [ Inst
                ( IAL
                    ( SAL
                        ( Cons 0 )
                    )
                )
            , Ref 2
            , Fetch
            , Inst
                ( IAL
                    ( SAL ( Binary LEQ ) )
                )
            , Inst
                ( IAL
                    ( SAL ( Unary BNot ) )
                )
            ]
        , Comment "while counter!=0"
        , Lambda
            [ Ref 0
            , Fetch
            , Inst
                ( IAL ( SIO OutputChar ) )
            , Comment "write the character"
            , Ref 2
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
            , Comment "decrement counter"
            ]
        , While
        ]
    , Cond
    , Inst
        ( IAL ( SIO InputChar ) )
    , Ref 0
    , Store
    , Comment " a is read "
    ]
, While
]