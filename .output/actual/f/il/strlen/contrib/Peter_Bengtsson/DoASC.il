[ Comment "
  Do ASCII, Types the ASCII character for a value.
  $VER: DoASCII 1.21 (31.03.95)
  Peter Bengtsson <a94petbe@ida.his.se>
  "
, Ref 0
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Cons 4 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary Sub ) )
    )
, Inst
    ( ISM
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Inst
    ( ISM
        ( SPure
            ( Cons 1 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary Add ) )
    )
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Cons 255 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary BAnd ) )
    )
, Inst
    ( ISM
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Inst
    ( ISM
        ( SPure
            ( Cons 10 )
        )
    )
, Inst
    ( ISM
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
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary Add ) )
        )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Cons 10 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
        )
    ]
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Cons 0 )
            )
        )
    , Ref 1
    , Store
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 255 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary BAnd ) )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 10 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 32 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( ISM
                ( SPure ( Binary BOr ) )
            )
        , Inst
            ( ISM
                ( SPure ( Unary BNot ) )
            )
        ]
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Cons 48 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Sub ) )
            )
        , Ref 1
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 10 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Mul ) )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Add ) )
            )
        , Ref 1
        , Store
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Add ) )
            )
        ]
    , While
    , Ref 1
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Cons 0 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Inst
            ( ISM ( SIO OutputDec ) )
        , Str " = "
        ]
    , Cond
    , Lambda []
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM ( SIO OutputChar ) )
    , Str "
      "
    ]
, While
]