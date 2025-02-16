# Interpreter for Microcaml

see https://github.com/umd-cmsc330/cmsc330spring22/tree/main/project4a for the language grammar

- build: `dune build`
- usage:
  - `./_build/default/microcaml.exe -e [filename]` to run the interpreter
  - `./_build/default/microcaml.exe -j [filename]` to transpile to js (unoptimized)

- `gcd.ml` should evaluate to `128`
- `fib.ml` should evaluate to `1597`
- `binomial.ml` should evaluate to `2035800`
- `oct.ml` should evaluate to `17141`
- `leetcode223.ml` should evaluate to `45`

## Example output
`fib.ml` is converted the following javascript code:
```js
(function(){ "use strict";
function $assert(cond, msg) { if( !cond ) throw new Error(msg); }
function $add(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return a + b;
}
function $sub(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return a - b;
}
function $mul(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return a * b;
}
function $div(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return Math.floor(a/b);
}
function $concat(a, b) {
  $assert( typeof a == "string" && typeof b == "string" );
  return a + b;
}
function $gt(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return a > b;
}
function $lt(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return a < b;
}
function $ge(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return a >= b;
}
function $le(a, b) {
  $assert( typeof a == "number" && typeof b == "number" );
  return a <= b;
}
function $eq(a, b) {
  $assert( typeof a == typeof b && typeof a != "function" );
  return a == b;
}
function $neq(a, b) {
  $assert( typeof a == typeof b && typeof a != "function" );
  return a != b;
}
let $t0;
let $t1;
function $t6($t5) {
  let $t4;
  let $t7;
  let $t22, $t21;
  $t22 = $t5;
  $t21 = 0;
  $t7 = $eq($t22, $t21);
  $assert( typeof $t7 == "boolean" );
  if($t7) {
    $t4 = 0;
  }
  else {
    let $t8;
    let $t20, $t19;
    $t20 = $t5;
    $t19 = 1;
    $t8 = $eq($t20, $t19);
    $assert( typeof $t8 == "boolean" );
    if($t8) {
      $t4 = 1;
    }
    else {
      let $t10, $t9;
      let $t16, $t15;
      $t16 = $t1;
      let $t18, $t17;
      $t18 = $t5;
      $t17 = 1;
      $t15 = $sub($t18, $t17);
      $assert( typeof $t16 == "function" );
      $t10 = $t16($t15);
      let $t12, $t11;
      $t12 = $t1;
      let $t14, $t13;
      $t14 = $t5;
      $t13 = 2;
      $t11 = $sub($t14, $t13);
      $assert( typeof $t12 == "function" );
      $t9 = $t12($t11);
      $t4 = $add($t10, $t9);
    }
  }
  return $t4;
}
$t1 = $t6;
let $t3, $t2;
$t3 = $t1;
$t2 = 17;
$assert( typeof $t3 == "function" );
$t0 = $t3($t2);
return $t0; })();
```
