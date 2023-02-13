[ Comment " NOTE: this program only works on 020+     "
, Comment " arg.f: pointer to argstring is put in var "a" at startup,
           this program prints it.                             "
, Str "you typed: "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SAL
            ( Cons 3 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary Sub ) )
    )
, Ref 0
, Store
, Lambda
    [ Ref 0
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary Add ) )
        )
    , Ref 0
    , Store
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( Cons 255 )
            )
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
    , Ref 2
    , Store
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