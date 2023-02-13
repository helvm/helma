[ Comment "Packer, by E. de Vos"
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
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
, Lambda []
, While
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 0
, Store
, Comment " a is char-read "
, Inst
    ( IAL
        ( SPure
            ( Cons 1 )
        )
    )
, Ref 2
, Store
, Comment " counter:=1"
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Unary Neg ) )
        )
    , Ref 0
    , Fetch
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    ]
, Comment "while a<>EOF"
]