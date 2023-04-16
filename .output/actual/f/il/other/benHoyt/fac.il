[ Comment " factorial program -- from original False distribution "
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
                ( Cons 1 )
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
                    ( IImmediate 0 ) Copy
                )
            )
        )
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
            ( ISM ( SPure Discard ) )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
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
                ( SPure ( Binary Sub ) )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( ISM
                ( SPure ( Binary Mul ) )
            )
        ]
    , Cond
    ]
, Ref 5
, Store
, Comment " factorial() function in False "
, Str "calculate the factorial of [1..8]: "
, Ref 1
, Inst
    ( ISM ( SIO InputChar ) )
, Ref 1
, Inst
    ( ISM
        ( SPure
            ( Cons 48 )
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
]