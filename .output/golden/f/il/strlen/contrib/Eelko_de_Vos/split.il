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
            ( SAL
                ( Cons 32 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
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
        ( SAL
            ( Cons 102 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SAL
            ( Cons 98 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SAL ( Binary BOr ) )
    )
, Inst
    ( IAL ( SIO InputChar ) )
, Inst
    ( IAL
        ( SAL
            ( Cons 32 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SAL ( Binary BAnd ) )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Comment " a has to be 'f' of 'b' followed by a space "
]