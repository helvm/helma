[ IAL
    ( Cons 1 )
, ICF
    ( Mark "C" )
, IAL Dup
, IAL ( SIO OutputDec )
, IAL
    ( Cons 10 )
, IAL ( SIO OutputChar )
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IAL Dup
, IAL
    ( Cons 11 )
, IAL ( Binary Sub )
, ICF
    ( CStatic "E" ( Branch EZ ) )
, ICF
    ( CStatic "C" Jump )
, ICF
    ( Mark "E" )
, IAL Discard
, End
, ICF
    ( Mark "add" )
, IAL ( Binary Add )
, ICF Return
, ICF
    ( Mark "write" )
, IAL Dup
, ILS Load
, IAL Dup
, ICF
    ( CStatic "write_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, ICF
    ( CStatic "write" Jump )
, ICF
    ( Mark "write_end" )
, IAL Discard
, IAL Discard
, ICF Return
, ICF
    ( Mark "read" )
, IAL Dup
, IAL Dup
, ILS ( MIO InputChar )
, ILS Load
, IAL Dup
, IAL
    ( Cons 10 )
, IAL ( Binary Sub )
, ICF
    ( CStatic "read_end" ( Branch EZ ) )
, IAL Discard
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, ICF
    ( CStatic "read" Jump )
, ICF
    ( Mark "read_end" )
, IAL Discard
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IAL
    ( Cons 0 )
, ILS Store
, ICF Return
, ICF
    ( Mark "newline" )
, IAL
    ( Cons 10 )
, IAL
    ( Cons 13 )
, IAL ( SIO OutputChar )
, IAL ( SIO OutputChar )
, ICF Return
]