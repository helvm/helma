[ Flush
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Dup )
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
    [ Inst
        ( IAL ( SIO OutputChar ) )
    ]
, While
]