let area =
  fun ax1 -> fun ay1 -> fun ax2 -> fun ay2 ->
  fun bx1 -> fun by1 -> fun bx2 -> fun by2 ->
    let min = fun x -> fun y -> if x < y then x else y in
    let max = fun x -> fun y -> if x > y then x else y in
    let areaOfA = (ay2 - ay1) * (ax2 - ax1) in
    let areaOfB = (by2 - by1) * (bx2 - bx1) in
    let left = (max ax1) bx1 in
    let right = (min ax2) bx2 in
    let xOverlap = right - left in
    let top = (min ay2) by2 in
    let bottom = (max ay1) by1 in
    let yOverlap = top - bottom in
    let areaOfOverlap =
      if xOverlap > 0 && yOverlap > 0 then xOverlap * yOverlap else 0
    in
    (areaOfA + areaOfB) - areaOfOverlap
in (((((((area (-3)) 0) 3) 4) 0) (-1)) 9) 2