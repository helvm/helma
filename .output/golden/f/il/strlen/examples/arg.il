[ Comment " NOTE: this program only works on 020+     "
, Comment " arg.f: pointer to argstring is put in var "a" at startup,
           this program prints it.                             "
, Str "you typed: "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 3 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary Sub ) )
    )
, Ref 0
, Store
, Lambda
    [ Ref 0
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Add ) )
        )
    , Ref 0
    , Store
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Cons 255 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary BAnd ) )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Ref 2
    , Store
    , Inst
        ( IAL
            ( SPure
                ( Cons 10 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
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