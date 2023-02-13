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
                ( Indexed Copy
                    ( ImmediateO 0 )
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
                    ( Indexed Copy
                        ( ImmediateO 0 )
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
                    ( Indexed Move
                        ( ImmediateO 2 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Move
                        ( ImmediateO 2 )
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
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Ref 25
, Store
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
                ( Indexed Copy
                    ( ImmediateO 0 )
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