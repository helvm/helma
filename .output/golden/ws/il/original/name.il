[ IAL
    ( Cons 0 )
, IAL
    ( Cons 80 )
, ILS Store
, IAL
    ( Cons 1 )
, IAL
    ( Cons 108 )
, ILS Store
, IAL
    ( Cons 2 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 3 )
, IAL
    ( Cons 97 )
, ILS Store
, IAL
    ( Cons 4 )
, IAL
    ( Cons 115 )
, ILS Store
, IAL
    ( Cons 5 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 6 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 7 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 8 )
, IAL
    ( Cons 110 )
, ILS Store
, IAL
    ( Cons 9 )
, IAL
    ( Cons 116 )
, ILS Store
, IAL
    ( Cons 10 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 11 )
, IAL
    ( Cons 114 )
, ILS Store
, IAL
    ( Cons 12 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 13 )
, IAL
    ( Cons 121 )
, ILS Store
, IAL
    ( Cons 14 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 15 )
, IAL
    ( Cons 117 )
, ILS Store
, IAL
    ( Cons 16 )
, IAL
    ( Cons 114 )
, ILS Store
, IAL
    ( Cons 17 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 18 )
, IAL
    ( Cons 110 )
, ILS Store
, IAL
    ( Cons 19 )
, IAL
    ( Cons 97 )
, ILS Store
, IAL
    ( Cons 20 )
, IAL
    ( Cons 109 )
, ILS Store
, IAL
    ( Cons 21 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 22 )
, IAL
    ( Cons 58 )
, ILS Store
, IAL
    ( Cons 23 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 24 )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 30 )
, IAL
    ( Cons 72 )
, ILS Store
, IAL
    ( Cons 31 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 32 )
, IAL
    ( Cons 108 )
, ILS Store
, IAL
    ( Cons 33 )
, IAL
    ( Cons 108 )
, ILS Store
, IAL
    ( Cons 34 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 35 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 36 )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 0 )
, IControl
    ( CStatic "write" Call )
, IAL
    ( Cons 100 )
, IControl
    ( CStatic "read" Call )
, IAL
    ( Cons 30 )
, IControl
    ( CStatic "write" Call )
, IAL
    ( Cons 100 )
, IControl
    ( CStatic "write" Call )
, IControl
    ( CStatic "newline" Call )
, End
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