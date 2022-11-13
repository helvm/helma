[ IAL
    ( Cons 0 )
, IAL
    ( Cons 72 )
, ILS Store
, IAL
    ( Cons 1 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 2 )
, IAL
    ( Cons 108 )
, ILS Store
, IAL
    ( Cons 3 )
, IAL
    ( Cons 108 )
, ILS Store
, IAL
    ( Cons 4 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 5 )
, IAL
    ( Cons 44 )
, ILS Store
, IAL
    ( Cons 6 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 7 )
, IAL
    ( Cons 119 )
, ILS Store
, IAL
    ( Cons 8 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 9 )
, IAL
    ( Cons 114 )
, ILS Store
, IAL
    ( Cons 10 )
, IAL
    ( Cons 108 )
, ILS Store
, IAL
    ( Cons 11 )
, IAL
    ( Cons 100 )
, ILS Store
, IAL
    ( Cons 12 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 13 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 14 )
, IAL
    ( Cons 102 )
, ILS Store
, IAL
    ( Cons 15 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 16 )
, IAL
    ( Cons 115 )
, ILS Store
, IAL
    ( Cons 17 )
, IAL
    ( Cons 112 )
, ILS Store
, IAL
    ( Cons 18 )
, IAL
    ( Cons 97 )
, ILS Store
, IAL
    ( Cons 19 )
, IAL
    ( Cons 99 )
, ILS Store
, IAL
    ( Cons 20 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 21 )
, IAL
    ( Cons 115 )
, ILS Store
, IAL
    ( Cons 22 )
, IAL
    ( Cons 33 )
, ILS Store
, IAL
    ( Cons 23 )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 0 )
, ICF
    ( CStatic "write" Call )
, ICF
    ( CStatic "newline" Call )
, End
, ICF
    ( SMark "add" )
, IAL ( Binary Add )
, ICF Return
, ICF
    ( SMark "write" )
, IAL
    ( SStatic 0 Copy )
, ILS Load
, IAL
    ( SStatic 0 Copy )
, ICF
    ( CStatic "write_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, ICF
    ( CStatic "write" Jump )
, ICF
    ( SMark "write_end" )
, IAL Discard
, IAL Discard
, ICF Return
, ICF
    ( SMark "read" )
, IAL
    ( SStatic 0 Copy )
, IAL
    ( SStatic 0 Copy )
, ILS ( MIO InputChar )
, ILS Load
, IAL
    ( SStatic 0 Copy )
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
    ( SMark "read_end" )
, IAL Discard
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IAL
    ( Cons 0 )
, ILS Store
, ICF Return
, ICF
    ( SMark "newline" )
, IAL
    ( Cons 10 )
, IAL
    ( Cons 13 )
, IAL ( SIO OutputChar )
, IAL ( SIO OutputChar )
, ICF Return
]