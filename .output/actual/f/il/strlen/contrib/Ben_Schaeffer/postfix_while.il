[ Comment " postfix while loops (##)! "
, Inst
    ( IAL
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
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
    , Ref 13
    , Store
    ]
, While
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Cons 0 )
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
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Sub ) )
        )
    , Ref 13
    , Store
    ]
, While
, Inst
    ( IAL
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
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
    , Ref 13
    , Store
    ]
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Cons 0 )
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
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Sub ) )
        )
    , Ref 13
    , Store
    ]
, While
, While
]