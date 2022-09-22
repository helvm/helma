[ Comment " NOTE: this program only works on 020+     "
, Comment " arg.f: pointer to argstring is put in var "a" at startup,
           this program prints it.                             "
, Str "you typed: "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( Cons 3 )
    )
, Inst
    ( IAL ( Binary Sub ) )
, Ref 0
, Store
, Lambda
    [ Ref 0
    , Fetch
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Add ) )
    , Ref 0
    , Store
    , Fetch
    , Inst
        ( IAL
            ( Cons 255 )
        )
    , Inst
        ( IAL ( Binary BAnd ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Ref 2
    , Store
    , Inst
        ( IAL
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Ref 2
    , Fetch
    , Inst
        ( IAL ( SIO OutputChar ) )
    ]
, While
, Str "
  "
]