[ Comment "Packer, by E. de Vos"
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
, Comment " a is char-read "
, Inst
    ( ISM
        ( SPure
            ( Cons 1 )
        )
    )
, Ref 2
, Store
, Comment " counter:=1"
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
]