[ Comment "Packer, by E. de Vos"
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
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
        ( Cons 1 )
    )
, Ref 2
, Store
, Comment " counter:=1"
, Lambda
    [ Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Ref 0
    , Fetch
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Comment "while a<>EOF"
]