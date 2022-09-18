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
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, While
, Comment "is first char a packcode?"
, Ref 11
, Fetch
, Inst
    ( IAL
        ( Cons 35 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Inst ( IAL Dup )
]