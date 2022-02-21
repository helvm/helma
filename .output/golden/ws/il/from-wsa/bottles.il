[ IAL
    ( Cons 3 )
, IControl
    ( Mark "LOOP" )
, IAL Dup
, IAL ( SIO OutputDec )
, IAL
    ( Cons 0 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 44 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 97 )
, IAL
    ( Cons 119 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 104 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 110 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 114 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 98 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 102 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 115 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 98 )
, IAL
    ( Cons 32 )
, IControl
    ( CStatic "prints" Call )
, IAL Dup
, IAL ( SIO OutputDec )
, IAL
    ( Cons 0 )
, IAL
    ( Cons 114 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 98 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 102 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 115 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 98 )
, IAL
    ( Cons 32 )
, IControl
    ( CStatic "printsln" Call )
, IAL
    ( Cons 0 )
, IAL
    ( Cons 100 )
, IAL
    ( Cons 110 )
, IAL
    ( Cons 117 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 114 )
, IAL
    ( Cons 97 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 105 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 115 )
, IAL
    ( Cons 115 )
, IAL
    ( Cons 97 )
, IAL
    ( Cons 112 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 44 )
, IAL
    ( Cons 110 )
, IAL
    ( Cons 119 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 100 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 110 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 107 )
, IAL
    ( Cons 97 )
, IAL
    ( Cons 84 )
, IControl
    ( CStatic "printsln" Call )
, IAL
    ( Cons 1 )
, IAL ( Binary Sub )
, IAL Dup
, IAL ( SIO OutputDec )
, IAL
    ( Cons 0 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 97 )
, IAL
    ( Cons 119 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 104 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 110 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 114 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 98 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 102 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 115 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 116 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 98 )
, IAL
    ( Cons 32 )
, IControl
    ( CStatic "printsln" Call )
, IAL
    ( Cons 0 )
, IControl
    ( CStatic "printsln" Call )
, IAL Dup
, IControl
    ( CStatic "LOOP:1" ( Branch EZ ) )
, IControl
    ( CStatic "LOOP" Jump )
, IControl
    ( Mark "LOOP:1" )
, End
, IControl
    ( Mark "prints" )
, IAL Dup
, IControl
    ( CStatic "prints_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, IControl
    ( CStatic "prints" Jump )
, IControl
    ( Mark "prints_end" )
, IAL Discard
, IControl Return
, IControl
    ( Mark "printsln" )
, IControl
    ( CStatic "prints" Call )
, IAL
    ( Cons 10 )
, IAL ( SIO OutputChar )
, IControl Return
]