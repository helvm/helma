[ Comment "countwl.f"
, Comment "count lines and words "
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 1
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 2
, Store
, Lambda
    [ Inst
        ( ISM ( SIO InputChar ) )
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
        [ Ref 0
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
        , Ref 0
        , Store
        , Inst
            ( ISM
                ( SPure
                    ( Cons 0 )
                )
            )
        , Ref 2
        , Store
        ]
    , Cond
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
    , Ref 3
    , Store
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
    , Ref 3
    , Fetch
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
    , Ref 3
    , Fetch
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
            ( SPure ( Binary BOr ) )
        )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( ISM
                ( SPure ( Unary BNot ) )
            )
        , Lambda
            [ Ref 1
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
                    ( SPure ( Unary Neg ) )
                )
            , Ref 2
            , Store
            ]
        , Cond
        ]
    , Cond
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Cons 0 )
                )
            )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst
        ( ISM
            ( SPure ( Binary BOr ) )
        )
    ]
, Ref 0
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
, Ref 0
, Store
, Ref 1
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
, Ref 1
, Store
, While
, Str "Number of words: "
, Ref 1
, Fetch
, Inst
    ( ISM ( SIO OutputDec ) )
, Str "
  Number of lines: "
, Ref 0
, Fetch
, Inst
    ( ISM ( SIO OutputDec ) )
, Str "
  "
]