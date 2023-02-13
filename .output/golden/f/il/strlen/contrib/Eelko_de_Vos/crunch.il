[ Comment "Packer, by E. de Vos"
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
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
, Lambda []
, While
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 0
, Store
, Comment " a is char-read "
, Inst
    ( IAL
        ( SAL
            ( Cons 1 )
        )
    )
, Ref 2
, Store
, Comment " counter:=1"
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Unary Neg ) )
        )
    , Ref 0
    , Fetch
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    ]
, Comment "while a<>EOF"
]