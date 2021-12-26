{ factorial program -- from original False distribution }

[$1=$[\%1\]?~[$1-f;!*]?]f:  { factorial() function in False }

"calculate the factorial of [1..8]: "
B^B'0-$$0>~\8>|$
"
result: "
~[\f;!.]?
[%"illegal input!"]?"
"
