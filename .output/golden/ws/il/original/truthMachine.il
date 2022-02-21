[ IAL
    ( Cons 0 )
, IAL Dup
, ILS ( MIO InputDec )
, ILS Load
, IControl
    ( CStatic "\x0" ( Branch EZ ) )
, IControl
    ( Mark "\x1" )
, IAL
    ( Cons 1 )
, IAL ( SIO OutputDec )
, IControl
    ( CStatic "\x1" Jump )
, IControl
    ( Mark "\x0" )
, IAL
    ( Cons 0 )
, IAL ( SIO OutputDec )
, End
]