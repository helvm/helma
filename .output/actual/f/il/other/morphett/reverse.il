[ Lambda []
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Lambda
        [ Inst
            ( IAL ( SIO OutputChar ) )
        , Exec
        ]
    ]
, While
, Inst ( IAL Discard )
, Exec
]