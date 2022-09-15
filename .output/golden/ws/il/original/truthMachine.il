[ IAL
    ( Cons 0 )
, IAL Dup
, ILS ( MIO InputDec )
, ILS Load
, ICF
    ( CStatic "\x0" ( Branch EZ ) )
, ICF
    ( Mark "\x1" )
, IAL
    ( Cons 1 )
, IAL ( SIO OutputDec )
, ICF
    ( CStatic "\x1" Jump )
, ICF
    ( Mark "\x0" )
, IAL
    ( Cons 0 )
, IAL ( SIO OutputDec )
, End
]