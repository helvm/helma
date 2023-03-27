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
        ( SPure
            ( Cons 4 )
        )
    )
, Ref 6
, Store
, Ref 6
, Fetch
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
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Sub ) )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    ]
, Lambda
    [ Ref 6
    , Fetch
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
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Sub ) )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        ]
    , Lambda
        [ Inst
            ( IAL ( SIO InputChar ) )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 42 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        ]
    , While
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL ( SPure Discard ) )
    ]
, While
, Inst
    ( IAL ( SPure Discard ) )
, Ref 6
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Ref 25
, Store
, Inst
    ( IAL
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary Mul ) )
    )
, Ref 4
, Store
, Lambda
    [ Ref 25
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Sub ) )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Ref 25
    , Store
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Unary Neg ) )
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
]