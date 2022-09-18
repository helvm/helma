[ Comment "
  Do ASCII, Types the ASCII character for a value.
  $VER: DoASCII 1.21 (31.03.95)
  Peter Bengtsson <a94petbe@ida.his.se>
  "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( Cons 4 )
    )
, Inst
    ( IAL ( Binary Sub ) )
, Inst ( IAL Dup )
, Inst
    ( IAL
        ( Cons 1 )
    )
, Inst
    ( IAL ( Binary Add ) )
, Fetch
, Inst
    ( IAL
        ( Cons 255 )
    )
, Inst
    ( IAL ( Binary BAnd ) )
, Inst ( IAL Dup )
, Inst
    ( IAL
        ( Cons 10 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Lambda
    [ Str "
    \x1b[32mDoASC v1.21 By Peter Bengtsson\x1b[0m
    This program will print the ASCII character
    for a decimal value.

    Syntax: DoASC <value1> <value2> ...
    " ]
, Cond
, Lambda
    [ Inst ( IAL Swap )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Add ) )
    , Inst ( IAL Swap )
    , Inst
        ( IAL
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Inst
        ( IAL
            ( Cons 0 )
        )
    , Ref 1
    , Store
    , Lambda
        [ Inst ( IAL Dup )
        , Fetch
        , Inst
            ( IAL
                ( Cons 255 )
            )
        , Inst
            ( IAL ( Binary BAnd ) )
        , Inst ( IAL Dup )
        , Inst ( IAL Dup )
        , Inst
            ( IAL
                ( Cons 10 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst ( IAL Swap )
        , Inst
            ( IAL
                ( Cons 32 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst
            ( IAL ( Binary BOr ) )
        , Inst
            ( IAL ( Unary BNot ) )
        ]
    , Lambda
        [ Inst
            ( IAL
                ( Cons 48 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Ref 1
        , Fetch
        , Inst
            ( IAL
                ( Cons 10 )
            )
        , Inst
            ( IAL ( Binary Mul ) )
        , Inst
            ( IAL ( Binary Add ) )
        , Ref 1
        , Store
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Add ) )
        ]
    , While
    , Ref 1
    , Fetch
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 0 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    , Lambda
        [ Inst ( IAL Dup )
        , Inst
            ( IAL ( SIO OutputDec ) )
        , Str " = "
        ]
    , Cond
    , Lambda []
    , Inst ( IAL Discard )
    , Inst
        ( IAL ( SIO OutputChar ) )
    , Str "
      "
    ]
, While
]