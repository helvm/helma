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
            ( SAL
                ( Cons 10 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    ]
, While
, Comment "is first char a packcode?"
, Ref 11
, Fetch
, Inst
    ( IAL
        ( SAL
            ( Cons 35 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
]