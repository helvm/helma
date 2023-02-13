[ Comment "countwl.f"
, Comment "count lines and words "
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 1
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 2
, Store
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
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
        [ Ref 0
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
        , Ref 0
        , Store
        , Inst
            ( IAL
                ( SPure
                    ( Cons 0 )
                )
            )
        , Ref 2
        , Store
        ]
    , Cond
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
    , Ref 3
    , Store
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
    , Ref 3
    , Fetch
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
    , Ref 3
    , Fetch
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
            ( SPure ( Binary BOr ) )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( SPure ( Unary BNot ) )
            )
        , Lambda
            [ Ref 1
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
                    ( SPure ( Unary Neg ) )
                )
            , Ref 2
            , Store
            ]
        , Cond
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Cons 0 )
                )
            )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure ( Binary BOr ) )
        )
    ]
, Ref 0
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
, Ref 0
, Store
, Ref 1
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
, Ref 1
, Store
, While
, Str "Number of words: "
, Ref 1
, Fetch
, Inst
    ( IAL ( SIO OutputDec ) )
, Str "
  Number of lines: "
, Ref 0
, Fetch
, Inst
    ( IAL ( SIO OutputDec ) )
, Str "
  "
]