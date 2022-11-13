[ IAL
    ( Cons 0 )
, IAL
    ( SStatic 0 Copy )
, ILS ( MIO InputDec )
, ILS Load
, ICF
    ( CStatic "\x0" ( Branch EZ ) )
, ICF
    ( SMark "\x1" )
, IAL
    ( Cons 1 )
, IAL ( SIO OutputDec )
, ICF
    ( CStatic "\x1" Jump )
, ICF
    ( SMark "\x0" )
, IAL
    ( Cons 0 )
, IAL ( SIO OutputDec )
, End
]