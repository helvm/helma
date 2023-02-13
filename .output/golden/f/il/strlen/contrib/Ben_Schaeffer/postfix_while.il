[ Comment " postfix while loops (##)! "
, Inst
    ( IAL
        ( SAL
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
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
    , Ref 13
    , Store
    ]
, While
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( Cons 0 )
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
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary Sub ) )
        )
    , Ref 13
    , Store
    ]
, While
, Inst
    ( IAL
        ( SAL
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
    [ Str "Still going up
      "
    , Ref 13
    , Fetch
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
    , Ref 13
    , Store
    ]
, Lambda
    [ Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( Cons 0 )
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
    [ Str "Down...
      "
    , Ref 13
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary Sub ) )
        )
    , Ref 13
    , Store
    ]
, While
, While
]