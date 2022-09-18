[ Comment " postfix while loops (##)! "
, Inst
    ( IAL
        ( Cons 5 )
    )
, Ref 13
, Store
, Lambda
    [ Ref 13
    , Fetch
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Add ) )
    , Ref 13
    , Store
    ]
, While
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( IAL
            ( Cons 0 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Sub ) )
    , Ref 13
    , Store
    ]
, While
, Inst
    ( IAL
        ( Cons 5 )
    )
, Ref 13
, Store
, Lambda
    [ Ref 13
    , Fetch
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Add ) )
    , Ref 13
    , Store
    ]
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( IAL
            ( Cons 0 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Sub ) )
    , Ref 13
    , Store
    ]
, While
, While
]