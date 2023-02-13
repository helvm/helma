[ Comment "
  Do ASCII, Types the ASCII character for a value.
  $VER: DoASCII 1.21 (31.03.95)
  Peter Bengtsson <a94petbe@ida.his.se>
  "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SAL
            ( Cons 4 )
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
, Fetch
, Inst
    ( IAL
        ( SAL
            ( Cons 255 )
        )
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
            ( Cons 10 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Lambda
    [ Str "
    \x1b[32mDoASC v1.21 By Peter Bengtsson\x1b[0m
    This program will print the ASCII character
    for a decimal value.

    Syntax: DoASC <value1> <value2> ...
    " ]
, Cond
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
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
            ( SAL ( Binary Add ) )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
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
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( Cons 0 )
            )
        )
    , Ref 1
    , Store
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 255 )
                )
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
                ( SAL
                    ( SStatic 1 Move )
                )
            )
        , Inst
            ( IAL
                ( SAL
                    ( Cons 32 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SAL ( Binary BOr ) )
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
                    ( Cons 48 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Sub ) )
            )
        , Ref 1
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 10 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Mul ) )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Add ) )
            )
        , Ref 1
        , Store
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
        ]
    , While
    , Ref 1
    , Fetch
    , Inst
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
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Inst
            ( IAL ( SIO OutputDec ) )
        , Str " = "
        ]
    , Cond
    , Lambda []
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL ( SIO OutputChar ) )
    , Str "
      "
    ]
, While
]