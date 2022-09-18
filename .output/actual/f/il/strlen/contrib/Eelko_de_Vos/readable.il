[ Comment "readable.f"
, Comment "gets a textfile and makes it readable (cuts off after Xth pos at space) "
, Inst
    ( IAL ( SIO InputChar ) )
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 1
, Store
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 2
, Store
]