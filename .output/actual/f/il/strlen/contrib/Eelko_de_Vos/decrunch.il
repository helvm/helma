[ Comment "Depacker, by E. de Vos"
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda []
, While
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 0
, Store
, Lambda
    [ Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Ref 0
    , Fetch
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Comment "while a<>EOF"
, Lambda
    [ Inst
        ( IAL
            ( Cons 169 )
        )
    , Ref 0
    , Fetch
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst ( IAL Dup )
    , Inst
        ( IAL ( Unary BNot ) )
    , Lambda
        [ Comment "if char!=escapecode"
        , Ref 0
        , Fetch
        , Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , Cond
    , Lambda
        [ Comment "if char=escapecode"
        , Inst
            ( IAL ( SIO InputChar ) )
        , Ref 2
        , Store
        , Comment "read counter"
        , Inst
            ( IAL ( SIO InputChar ) )
        , Ref 0
        , Store
        , Comment "read char"
        , Lambda
            [ Inst
                ( IAL
                    ( Cons 0 )
                )
            , Ref 2
            , Fetch
            , Inst
                ( IAL ( Binary LEQ ) )
            , Inst
                ( IAL ( Unary BNot ) )
            ]
        , Comment "while counter!=0"
        , Lambda
            [ Ref 0
            , Fetch
            , Inst
                ( IAL ( SIO OutputChar ) )
            , Comment "write the character"
            , Ref 2
            , Fetch
            , Inst
                ( IAL
                    ( Cons 1 )
                )
            , Inst
                ( IAL ( Binary Sub ) )
            , Ref 2
            , Store
            , Comment "decrement counter"
            ]
        , While
        ]
    , Cond
    , Inst
        ( IAL ( SIO InputChar ) )
    , Ref 0
    , Store
    , Comment " a is read "
    ]
, While
]