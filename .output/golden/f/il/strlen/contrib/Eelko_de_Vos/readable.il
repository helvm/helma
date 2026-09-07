[ Comment "readable.f"
, Comment "gets a textfile and makes it readable (cuts off after Xth pos at space) "
, Inst
    ( ISM ( SIO InputChar ) )
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 1
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 2
, Store
]