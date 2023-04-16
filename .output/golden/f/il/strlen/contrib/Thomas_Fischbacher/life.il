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
    ( ISM
        ( SPure
            ( Cons 4 )
        )
    )
, Ref 6
, Store
, Ref 6
, Fetch
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
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Cons 1 )
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
    ]
, Lambda
    [ Ref 6
    , Fetch
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
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
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
        ]
    , Lambda
        [ Inst
            ( ISM ( SIO InputChar ) )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 42 )
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
                        ( IImmediate 2 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        ]
    , While
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM ( SIO InputChar ) )
    , Inst
        ( ISM ( SPure Discard ) )
    ]
, While
, Inst
    ( ISM ( SPure Discard ) )
, Ref 6
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Ref 25
, Store
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
        ( SPure ( Binary Mul ) )
    )
, Ref 4
, Store
, Lambda
    [ Ref 25
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
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
    , Ref 25
    , Store
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Unary Neg ) )
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
]