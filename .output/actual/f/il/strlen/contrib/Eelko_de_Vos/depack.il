[ Comment " depack.f "
, Comment "remove first until carriage return (redirection gives this shit) "
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Ref 11
    , Store
    ]
, Lambda
    [ Ref 11
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
            ( SPure ( Unary BNot ) )
        )
    ]
, While
, Comment "is first char a packcode?"
, Ref 11
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 35 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
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