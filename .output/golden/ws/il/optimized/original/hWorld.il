[ IAL
    ( SAL
        ( Cons 0 )
    )
, IAL
    ( SAL
        ( Cons 72 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL
        ( Cons 101 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 2 )
    )
, IAL
    ( SAL
        ( Cons 108 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 3 )
    )
, IAL
    ( SAL
        ( Cons 108 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 4 )
    )
, IAL
    ( SAL
        ( Cons 111 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 5 )
    )
, IAL
    ( SAL
        ( Cons 44 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 6 )
    )
, IAL
    ( SAL
        ( Cons 32 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 7 )
    )
, IAL
    ( SAL
        ( Cons 119 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 8 )
    )
, IAL
    ( SAL
        ( Cons 111 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 9 )
    )
, IAL
    ( SAL
        ( Cons 114 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 10 )
    )
, IAL
    ( SAL
        ( Cons 108 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 11 )
    )
, IAL
    ( SAL
        ( Cons 100 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 12 )
    )
, IAL
    ( SAL
        ( Cons 32 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 13 )
    )
, IAL
    ( SAL
        ( Cons 111 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 14 )
    )
, IAL
    ( SAL
        ( Cons 102 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 15 )
    )
, IAL
    ( SAL
        ( Cons 32 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 16 )
    )
, IAL
    ( SAL
        ( Cons 115 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 17 )
    )
, IAL
    ( SAL
        ( Cons 112 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 18 )
    )
, IAL
    ( SAL
        ( Cons 97 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 19 )
    )
, IAL
    ( SAL
        ( Cons 99 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 20 )
    )
, IAL
    ( SAL
        ( Cons 101 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 21 )
    )
, IAL
    ( SAL
        ( Cons 115 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 22 )
    )
, IAL
    ( SAL
        ( Cons 33 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 23 )
    )
, IAL
    ( SAL
        ( Cons 0 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 0 )
    )
, ICF
    ( CStatic "write" Call )
, ICF
    ( CStatic "newline" Call )
, End
, ICF
    ( SMark "add" )
, IAL
    ( SAL ( Binary Add ) )
, ICF Return
, ICF
    ( SMark "write" )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, ILS Load
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, ICF
    ( CStatic "write_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL ( Binary Add ) )
, ICF
    ( CStatic "write" Jump )
, ICF
    ( SMark "write_end" )
, IAL ( SAL Discard )
, IAL ( SAL Discard )
, ICF Return
, ICF
    ( SMark "read" )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, ILS ( MIO InputChar )
, ILS Load
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 10 )
    )
, IAL
    ( SAL ( Binary Sub ) )
, ICF
    ( CStatic "read_end" ( Branch EZ ) )
, IAL ( SAL Discard )
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL ( Binary Add ) )
, ICF
    ( CStatic "read" Jump )
, ICF
    ( SMark "read_end" )
, IAL ( SAL Discard )
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL ( Binary Add ) )
, IAL
    ( SAL
        ( Cons 0 )
    )
, ILS Store
, ICF Return
, ICF
    ( SMark "newline" )
, IAL
    ( SAL
        ( Cons 10 )
    )
, IAL
    ( SAL
        ( Cons 13 )
    )
, IAL ( SIO OutputChar )
, IAL ( SIO OutputChar )
, ICF Return
]