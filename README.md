# Interpreter for Microcaml

see https://github.com/umd-cmsc330/cmsc330spring22/tree/main/project4a for the language grammar

build: `dune build`
usage:
`./_build/default/microcaml.exe -e [filename]` to run the interpreter
`./_build/default/microcaml.exe -j [filename]` to transpile to js (unoptimized)

`gcd.ml` should evaluate to `128`
`fib.ml` should evaluate to `1597`
`binomial.ml` should evaluate to `2035800`
`oct.ml` should evaluate to `17141`
`leetcode223.ml` should evaluate to `45`
