{ factorial program in false! }

[$1=~[$1-f;!*]?]f:          { fac() in false }

"calculate the factorial of [1..8]: "
ß^ß'0-$$0>~\8>|$
"result: "
~[\f;!.]?
["illegal input!"]?"
"
