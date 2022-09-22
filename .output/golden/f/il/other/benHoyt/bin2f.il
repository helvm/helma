[ Comment " convert binary executable code to 80386 False source "
, Str "["
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Add ) )
    ]
, Lambda
    [ Inst
        ( IAL ( SIO OutputDec ) )
    , Str "`"
    ]
, While
, Inst ( IAL Discard )
, Str "]"
]