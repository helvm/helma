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
    ( Cons 115 )
, ILS Store
, IAL
    ( Cons 7 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 8 )
, IAL
    ( Cons 109 )
, ILS Store
, IAL
    ( Cons 9 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 10 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 11 )
, IAL
    ( Cons 110 )
, ILS Store
, IAL
    ( Cons 12 )
, IAL
    ( Cons 117 )
, ILS Store
, IAL
    ( Cons 13 )
, IAL
    ( Cons 109 )
, ILS Store
, IAL
    ( Cons 14 )
, IAL
    ( Cons 98 )
, ILS Store
, IAL
    ( Cons 15 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 16 )
, IAL
    ( Cons 114 )
, ILS Store
, IAL
    ( Cons 17 )
, IAL
    ( Cons 115 )
, ILS Store
, IAL
    ( Cons 18 )
, IAL
    ( Cons 44 )
, ILS Store
, IAL
    ( Cons 19 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 20 )
, IAL
    ( Cons 116 )
, ILS Store
, IAL
    ( Cons 21 )
, IAL
    ( Cons 104 )
, ILS Store
, IAL
    ( Cons 22 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 23 )
, IAL
    ( Cons 110 )
, ILS Store
, IAL
    ( Cons 24 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 25 )
, IAL
    ( Cons 45 )
, ILS Store
, IAL
    ( Cons 26 )
, IAL
    ( Cons 49 )
, ILS Store
, IAL
    ( Cons 27 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 28 )
, IAL
    ( Cons 116 )
, ILS Store
, IAL
    ( Cons 29 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 30 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 31 )
, IAL
    ( Cons 102 )
, ILS Store
, IAL
    ( Cons 32 )
, IAL
    ( Cons 105 )
, ILS Store
, IAL
    ( Cons 33 )
, IAL
    ( Cons 110 )
, ILS Store
, IAL
    ( Cons 34 )
, IAL
    ( Cons 105 )
, ILS Store
, IAL
    ( Cons 35 )
, IAL
    ( Cons 115 )
, ILS Store
, IAL
    ( Cons 36 )
, IAL
    ( Cons 104 )
, ILS Store
, IAL
    ( Cons 37 )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 42 )
, IAL
    ( Cons 78 )
, ILS Store
, IAL
    ( Cons 43 )
, IAL
    ( Cons 117 )
, ILS Store
, IAL
    ( Cons 44 )
, IAL
    ( Cons 109 )
, ILS Store
, IAL
    ( Cons 45 )
, IAL
    ( Cons 98 )
, ILS Store
, IAL
    ( Cons 46 )
, IAL
    ( Cons 101 )
, ILS Store
, IAL
    ( Cons 47 )
, IAL
    ( Cons 114 )
, ILS Store
, IAL
    ( Cons 48 )
, IAL
    ( Cons 58 )
, ILS Store
, IAL
    ( Cons 49 )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 60 )
, IAL
    ( Cons 84 )
, ILS Store
, IAL
    ( Cons 61 )
, IAL
    ( Cons 111 )
, ILS Store
, IAL
    ( Cons 62 )
, IAL
    ( Cons 116 )
, ILS Store
, IAL
    ( Cons 63 )
, IAL
    ( Cons 97 )
, ILS Store
, IAL
    ( Cons 64 )
, IAL
    ( Cons 108 )
, ILS Store
, IAL
    ( Cons 65 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 66 )
, IAL
    ( Cons 105 )
, ILS Store
, IAL
    ( Cons 67 )
, IAL
    ( Cons 115 )
, ILS Store
, IAL
    ( Cons 68 )
, IAL
    ( Cons 32 )
, ILS Store
, IAL
    ( Cons 69 )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 0 )
, ICF
    ( CStatic "write" Call )
, ICF
    ( CStatic "newline" Call )
, IAL
    ( Cons 100 )
, IAL
    ( Cons 0 )
, ILS Store
, ICF
    ( Mark "loop" )
, IAL
    ( Cons 42 )
, ICF
    ( CStatic "write" Call )
, IAL
    ( Cons 101 )
, ILS ( MIO InputDec )
, IAL
    ( Cons 101 )
, ILS Load
, IAL Dup
, IAL
    ( Cons
        ( -1 )
    )
, IAL ( Binary Sub )
, ICF
    ( CStatic "progend" ( Branch EZ ) )
, IAL
    ( Cons 100 )
, ILS Load
, IAL ( Binary Add )
, IAL
    ( Cons 100 )
, IAL Swap
, ILS Store
, ICF
    ( CStatic "loop" Jump )
, ICF
    ( Mark "progend" )
, IAL Discard
, IAL
    ( Cons 60 )
, ICF
    ( CStatic "write" Call )
, IAL
    ( Cons 100 )
, ILS Load
, IAL ( SIO OutputDec )
, ICF
    ( CStatic "newline" Call )
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