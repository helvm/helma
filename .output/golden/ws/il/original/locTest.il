[ IAL
    ( Cons 0 )
, IAL
    ( Cons 69 )
, ILS Store
, IAL
    ( Cons 1 )
, IAL
    ( Cons 110 )
, ILS Store
, IAL
    ( Cons 2 )
, IAL
    ( Cons 116 )
, ILS Store
, IAL
    ( Cons 3 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 4 )
, IAL
    ( Cons 114 )
, ILS Store
, IAL
    ( Cons 5 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 6 )
, IAL
    ( Cons 97 )
, ILS Store
, IAL
    ( Cons 7 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 8 )
, IAL
    ( Cons 110 )
, ILS Store
, IAL
    ( Cons 9 )
, IAL
    ( Cons 117 )
, ILS Store
, IAL
    ( Cons 10 )
, IAL
    ( Cons 109 )
, ILS Store
, IAL
    ( Cons 11 )
, IAL
    ( Cons 98 )
, ILS Store
, IAL
    ( Cons 12 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 13 )
, IAL
    ( Cons 114 )
, ILS Store
, IAL
    ( Cons 14 )
, IAL
    ( Cons 58 )
, ILS Store
, IAL
    ( Cons 15 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 16 )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 0 )
, IControl
    ( CStatic "write" Call )
, IAL
    ( Cons 100 )
, ILS ( MIO InputDec )
, IAL
    ( Cons 0 )
, IControl
    ( CStatic "write" Call )
, IAL
    ( Cons 101 )
, ILS ( MIO InputDec )
, IAL
    ( Cons 100 )
, ILS Load
, IAL
    ( Cons 101 )
, ILS Load
, IControl
    ( CStatic "loctest" Call )
, IAL ( SIO OutputDec )
, End
, IControl
    ( Mark "loctest" )
, IAL
    ( SStatic 0 Copy )
, IAL
    ( SStatic 2 Copy )
, IAL ( Binary Add )
, IAL
    ( SStatic 2 Slide )
, IControl Return
, IControl
    ( Mark "add" )
, IAL ( Binary Add )
, IControl Return
, IControl
    ( Mark "write" )
, IAL Dup
, ILS Load
, IAL Dup
, IControl
    ( CStatic "write_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IControl
    ( CStatic "write" Jump )
, IControl
    ( Mark "write_end" )
, IAL Discard
, IAL Discard
, IControl Return
, IControl
    ( Mark "read" )
, IAL Dup
, IAL Dup
, ILS ( MIO InputChar )
, ILS Load
, IAL Dup
, IAL
    ( Cons 10 )
, IAL ( Binary Sub )
, IControl
    ( CStatic "read_end" ( Branch EZ ) )
, IAL Discard
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IControl
    ( CStatic "read" Jump )
, IControl
    ( Mark "read_end" )
, IAL Discard
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IAL
    ( Cons 0 )
, ILS Store
, IControl Return
, IControl
    ( Mark "newline" )
, IAL
    ( Cons 10 )
, IAL
    ( Cons 13 )
, IAL ( SIO OutputChar )
, IAL ( SIO OutputChar )
, IControl Return
]