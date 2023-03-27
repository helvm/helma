[ Comment " convert binary executable code to 80386 False source "
, Str "["
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
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
    ]
, Lambda
    [ Inst
        ( IAL ( SIO OutputDec ) )
    , Str "`"
    ]
, While
, Inst
    ( IAL ( SPure Discard ) )
, Str "]"
]