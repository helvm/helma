[ Comment "
  Do ASCII, Types the ASCII character for a value.
  $VER: DoASCII 1.21 (31.03.95)
  Peter Bengtsson <a94petbe@ida.his.se>
  "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 4 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary Sub ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Cons 1 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary Add ) )
    )
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 255 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary BAnd ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Cons 10 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
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
            ( SPure
                ( Indexed Move
                    ( ImmediateO 1 )
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Add ) )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Move
                    ( ImmediateO 1 )
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 10 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    ]
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Cons 0 )
            )
        )
    , Ref 1
    , Store
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Indexed Copy
                        ( ImmediateO 0 )
                    )
                )
            )
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 255 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary BAnd ) )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Copy
                        ( ImmediateO 0 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Copy
                        ( ImmediateO 0 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 10 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Move
                        ( ImmediateO 1 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 32 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SPure ( Binary BOr ) )
            )
        , Inst
            ( IAL
                ( SPure ( Unary BNot ) )
            )
        ]
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Cons 48 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Sub ) )
            )
        , Ref 1
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 10 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Mul ) )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Add ) )
            )
        , Ref 1
        , Store
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Add ) )
            )
        ]
    , While
    , Ref 1
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 0 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Indexed Copy
                        ( ImmediateO 0 )
                    )
                )
            )
        , Inst
            ( IAL ( SIO OutputDec ) )
        , Str " = "
        ]
    , Cond
    , Lambda []
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL ( SIO OutputChar ) )
    , Str "
      "
    ]
, While
]