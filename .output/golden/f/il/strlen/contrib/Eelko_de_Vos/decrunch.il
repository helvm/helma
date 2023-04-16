[ Comment "Depacker, by E. de Vos"
, Lambda
    [ Inst
        ( ISM ( SIO InputChar ) )
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
, Lambda []
, While
, Inst
    ( ISM ( SIO InputChar ) )
, Ref 0
, Store
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Unary Neg ) )
        )
    , Ref 0
    , Fetch
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
        )
    ]
, Comment "while a<>EOF"
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Cons 169 )
            )
        )
    , Ref 0
    , Fetch
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
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
            ( SPure ( Unary BNot ) )
        )
    , Lambda
        [ Comment "if char!=escapecode"
        , Ref 0
        , Fetch
        , Inst
            ( ISM ( SIO OutputChar ) )
        ]
    , Cond
    , Lambda
        [ Comment "if char=escapecode"
        , Inst
            ( ISM ( SIO InputChar ) )
        , Ref 2
        , Store
        , Comment "read counter"
        , Inst
            ( ISM ( SIO InputChar ) )
        , Ref 0
        , Store
        , Comment "read char"
        , Lambda
            [ Inst
                ( ISM
                    ( SPure
                        ( Cons 0 )
                    )
                )
            , Ref 2
            , Fetch
            , Inst
                ( ISM
                    ( SPure ( Binary LEQ ) )
                )
            , Inst
                ( ISM
                    ( SPure ( Unary BNot ) )
                )
            ]
        , Comment "while counter!=0"
        , Lambda
            [ Ref 0
            , Fetch
            , Inst
                ( ISM ( SIO OutputChar ) )
            , Comment "write the character"
            , Ref 2
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
            , Ref 2
            , Store
            , Comment "decrement counter"
            ]
        , While
        ]
    , Cond
    , Inst
        ( ISM ( SIO InputChar ) )
    , Ref 0
    , Store
    , Comment " a is read "
    ]
, While
]