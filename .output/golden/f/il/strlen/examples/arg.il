[ Comment " NOTE: this program only works on 020+     "
, Comment " arg.f: pointer to argstring is put in var "a" at startup,
           this program prints it.                             "
, Str "you typed: "
, Ref 0
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Cons 3 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary Sub ) )
    )
, Ref 0
, Store
, Lambda
    [ Ref 0
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary Add ) )
        )
    , Ref 0
    , Store
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Cons 255 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary BAnd ) )
        )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Ref 2
    , Store
    , Inst
        ( ISM
            ( SPure
                ( Cons 10 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
        )
    ]
, Lambda
    [ Ref 2
    , Fetch
    , Inst
        ( ISM ( SIO OutputChar ) )
    ]
, While
, Str "
  "
]