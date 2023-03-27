[ Comment " factorial program -- from original False distribution "
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
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
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( IAL ( SPure Discard ) )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
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
                    ( Indexed
                        ( IImmediate 0 ) Copy
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
                ( SPure ( Binary Sub ) )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( IAL
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
    ( IAL ( SIO InputChar ) )
, Ref 1
, Inst
    ( IAL
        ( SPure
            ( Cons 48 )
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
        ( SPure
            ( Cons 0 )
        )
    )
]