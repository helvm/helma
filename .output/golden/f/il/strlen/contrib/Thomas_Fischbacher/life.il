[ Comment "
  "Life" in FALSE

  input is for example:

  ..-.
  --..
  ....
  ...-
  "
, Ref 1
, Inst
    ( IAL
        ( SAL
            ( Cons 4 )
        )
    )
, Ref 6
, Store
, Ref 6
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
, Lambda
    [ Inst
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
                ( SStatic 0 Copy )
            )
        )
    ]
, Lambda
    [ Ref 6
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
    , Lambda
        [ Inst
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
                    ( SStatic 0 Copy )
                )
            )
        ]
    , Lambda
        [ Inst
            ( IAL ( SIO InputChar ) )
        , Inst
            ( IAL
                ( SAL
                    ( Cons 42 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary LEQ ) )
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
    , While
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL ( SAL Discard ) )
    ]
, While
, Inst
    ( IAL ( SAL Discard ) )
, Ref 6
, Fetch
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Ref 25
, Store
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary Mul ) )
    )
, Ref 4
, Store
, Lambda
    [ Ref 25
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
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Ref 25
    , Store
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
]