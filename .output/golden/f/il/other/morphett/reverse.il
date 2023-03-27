[ Lambda []
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
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
    [ Lambda
        [ Inst
            ( IAL ( SIO OutputChar ) )
        , Exec
        ]
    ]
, While
, Inst
    ( IAL ( SPure Discard ) )
, Exec
]