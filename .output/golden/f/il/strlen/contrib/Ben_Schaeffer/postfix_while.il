[ Comment " postfix while loops (##)! "
, Inst
    ( ISM
        ( SPure
            ( Cons 5 )
        )
    )
, Ref 13
, Store
, Lambda
    [ Ref 13
    , Fetch
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
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
    , Ref 13
    , Store
    ]
, While
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Cons 0 )
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
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary Sub ) )
        )
    , Ref 13
    , Store
    ]
, While
, Inst
    ( ISM
        ( SPure
            ( Cons 5 )
        )
    )
, Ref 13
, Store
, Lambda
    [ Ref 13
    , Fetch
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
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
    , Ref 13
    , Store
    ]
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Cons 0 )
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
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary Sub ) )
        )
    , Ref 13
    , Store
    ]
, While
, While
]