module HelVM.HelCam.BrainFuck.EvaluatorTestData where

--------------------------------------------------------------------------------

value256 :: String
value256 = "                                                              \
\Calculate the value 256 and test if it's zero                            \
\ If the interpreter errors on overflow this is where it'll happen        \
\ ++++++++[>++++++++<-]>[<++++>-]                                         \
\ +<[>-<                                                                  \
\     Not zero so multiply by 256 again to get 65536                      \
\     [>++++<-]>[<++++++++>-]<[>++++++++<-]                               \
\     +>[>                                                                \
\         # Print \"32\"                                                  \
\         ++++++++++[>+++++<-]>+.-.[-]<                                   \
\     <[-]<->] <[>>                                                       \
\         # Print \"16\"                                                  \
\         +++++++[>+++++++<-]>.+++++.[-]<                                 \
\ <<-]] >[>                                                               \
\     # Print \"8\"                                                       \
\     ++++++++[>+++++++<-]>.[-]<                                          \
\ <-]<                                                                    \
\ # Print \" bit cells\n\"                                                \
\ +++++++++++[>+++>+++++++++>+++++++++>+<<<<-]>-.>-.+++++++.+++++++++++.<.\
\ >>.++.+++++++..<-.>>-                                                   \
\ Clean up used cells.                                                    \
\ [[-]<]"

--------------------------------------------------------------------------------

helloWorldWithComments :: String
helloWorldWithComments = "                                                       \
\ 1 +++++ +++               Set Cell #0 to 8                                     \
\ 2 [                                                                            \
\ 3     >++++               Add 4 to Cell #1; this will always set Cell #1 to 4  \
\ 4     [                   as the cell will be cleared by the loop              \
\ 5         >++             Add 4*2 to Cell #2                                   \
\ 6         >+++            Add 4*3 to Cell #3                                   \
\ 7         >+++            Add 4*3 to Cell #4                                   \
\ 8         >+              Add 4 to Cell #5                                     \
\ 9         <<<<-           Decrement the loop counter in Cell #1                \
\10     ]                   Loop till Cell #1 is zero                            \
\11     >+                  Add 1 to Cell #2                                     \
\12     >+                  Add 1 to Cell #3                                     \
\13     >-                  Subtract 1 from Cell #4                              \
\14     >>+                 Add 1 to Cell #6                                     \
\15     [<]                 Move back to the first zero cell you find; this will \
\16                         be Cell #1 which was cleared by the previous loop    \
\17     <-                  Decrement the loop Counter in Cell #0                \
\18 ]                       Loop till Cell #0 is zero                            \
\19                                                                              \
\20 The result of this is:                                                       \
\21 Cell No :   0   1   2   3   4   5   6                                        \
\22 Contents:   0   0  72 104  88  32   8                                        \
\23 Pointer :   ^                                                                \
\24                                                                              \
\25 >>.                     Cell #2 has value 72 which is 'H'                    \
\26 >---.                   Subtract 3 from Cell #3 to get 101 which is 'e'      \
\27 +++++ ++..+++.          Likewise for 'llo' from Cell #3                      \
\28 >>.                     Cell #5 is 32 for the space                          \
\29 <-.                     Subtract 1 from Cell #4 for 87 to give a 'W'         \
\30 <.                      Cell #3 was set to 'o' from the end of 'Hello'       \
\31 +++.----- -.----- ---.  Cell #3 for 'rl' and 'd'                             \
\32 >>+.                    Add 1 to Cell #5 gives us an exclamation point       \
\33 >++.                    And finally a newline from Cell #6                   \
\"

--------------------------------------------------------------------------------

helloWorld :: String
helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

fascistHelloWorld :: String
fascistHelloWorld = ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+."

padHelloWorld :: String
padHelloWorld = "--<-<<+[+[<+>--->->->-<<<]>]<<--.<++++++.<<-..<<.<+.>>.>>.<<<.+++.>>.>>-.<<<+."

theShortestHelloWorld :: String
theShortestHelloWorld = "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."

--------------------------------------------------------------------------------

helloWorldExpected :: String
helloWorldExpected = "Hello World!\n"

hello_WorldExpected :: String
hello_WorldExpected = "Hello, World!"
