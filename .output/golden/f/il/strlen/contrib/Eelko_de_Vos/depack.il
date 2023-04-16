[ Comment " depack.f "
, Comment "remove first until carriage return (redirection gives this shit) "
, Lambda
    [ Inst
        ( ISM ( SIO InputChar ) )
    , Ref 11
    , Store
    ]
, Lambda
    [ Ref 11
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
            ( SPure ( Unary BNot ) )
        )
    ]
, While
, Comment "is first char a packcode?"
, Ref 11
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Cons 35 )
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
]