[ IAL
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Mark
        ( MArtificial "C" )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, IAL ( SIO OutputDec )
, IAL
    ( SPure
        ( Cons 10 )
    )
, IAL ( SIO OutputChar )
, IAL
    ( SPure
        ( Cons 1 )
    )
, IAL
    ( SPure ( Binary Add ) )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, IAL
    ( SPure
        ( Cons 11 )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled ( Branch EZ )
        ( LArtificial "E" )
    )
, ICF
    ( Labeled Jump
        ( LArtificial "C" )
    )
, ICF
    ( Mark
        ( MArtificial "E" )
    )
, IAL ( SPure Discard )
, End
, ICF
    ( Mark
        ( MArtificial "add" )
    )
, IAL
    ( SPure ( Binary Add ) )
, ICF Return
, ICF
    ( Mark
        ( MArtificial "write" )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ILS Load
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ICF
    ( Labeled ( Branch EZ )
        ( LArtificial "write_end" )
    )
, IAL ( SIO OutputChar )
, IAL
    ( SPure
        ( Cons 1 )
    )
, IAL
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled Jump
        ( LArtificial "write" )
    )
, ICF
    ( Mark
        ( MArtificial "write_end" )
    )
, IAL ( SPure Discard )
, IAL ( SPure Discard )
, ICF Return
, ICF
    ( Mark
        ( MArtificial "read" )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ILS ( MIO InputChar )
, ILS Load
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, IAL
    ( SPure
        ( Cons 10 )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled ( Branch EZ )
        ( LArtificial "read_end" )
    )
, IAL ( SPure Discard )
, IAL
    ( SPure
        ( Cons 1 )
    )
, IAL
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled Jump
        ( LArtificial "read" )
    )
, ICF
    ( Mark
        ( MArtificial "read_end" )
    )
, IAL ( SPure Discard )
, IAL
    ( SPure
        ( Cons 1 )
    )
, IAL
    ( SPure ( Binary Add ) )
, IAL
    ( SPure
        ( Cons 0 )
    )
, ILS Store
, ICF Return
, ICF
    ( Mark
        ( MArtificial "newline" )
    )
, IAL
    ( SPure
        ( Cons 10 )
    )
, IAL
    ( SPure
        ( Cons 13 )
    )
, IAL ( SIO OutputChar )
, IAL ( SIO OutputChar )
, ICF Return
]