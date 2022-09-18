[ Comment "split.f"
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 0
, Store
, Lambda
    [ Ref 0
    , Fetch
    , Inst
        ( IAL
            ( Cons 32 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    ]
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Ref 0
    , Store
    ]
, While
, Comment " read operand for split: front/back "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( Cons 102 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Ref 0
, Fetch
, Inst
    ( IAL
        ( Cons 98 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Inst
    ( IAL ( Binary BOr ) )
, Inst
    ( IAL ( SIO InputChar ) )
, Inst
    ( IAL
        ( Cons 32 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Inst
    ( IAL ( Binary BAnd ) )
, Inst ( IAL Dup )
, Comment " a has to be 'f' of 'b' followed by a space "
]