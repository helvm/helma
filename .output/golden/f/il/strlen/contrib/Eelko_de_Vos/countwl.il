[ Comment "countwl.f"
, Comment "count lines and words "
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 0
, Store
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
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Add ) )
        , Ref 0
        , Store
        , Inst
            ( IAL
                ( Cons 0 )
            )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst ( IAL Dup )
    , Inst ( IAL Dup )
    , Ref 3
    , Store
    , Inst
        ( IAL
            ( Cons 32 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Ref 3
    , Fetch
    , Inst
        ( IAL
            ( Cons 32 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Binary BOr ) )
    , Ref 3
    , Fetch
    , Inst
        ( IAL
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Binary BOr ) )
    , Inst ( IAL Dup )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL ( Unary BNot ) )
        , Lambda
            [ Ref 1
            , Fetch
            , Inst
                ( IAL
                    ( Cons 1 )
                )
            , Inst
                ( IAL ( Binary Add ) )
            , Ref 1
            , Store
            , Inst
                ( IAL
                    ( Cons 1 )
                )
            , Inst
                ( IAL ( Unary Neg ) )
            , Ref 2
            , Store
            ]
        , Cond
        ]
    , Cond
    , Inst
        ( IAL ( Unary BNot ) )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 0 )
            )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst
        ( IAL ( Binary BOr ) )
    ]
, Ref 0
, Fetch
, Inst
    ( IAL
        ( Cons 1 )
    )
, Inst
    ( IAL ( Binary Sub ) )
, Ref 0
, Store
, Ref 1
, Fetch
, Inst
    ( IAL
        ( Cons 1 )
    )
, Inst
    ( IAL ( Binary Sub ) )
, Ref 1
, Store
, While
, Str "Number of words: "
, Ref 1
, Fetch
, Inst
    ( IAL ( SIO OutputDec ) )
, Str "
  Number of lines: "
, Ref 0
, Fetch
, Inst
    ( IAL ( SIO OutputDec ) )
, Str "
  "
]