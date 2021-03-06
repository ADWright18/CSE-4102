(* Program accepts date, month, and year and outputs the day of the week

ex.
- zeller(31, 12, 1999)
val it = 5 : int
Day 5 is Friday

*)

local
 val floor = Real.floor
 val real = Real.fromInt
 val zc =
 fn (d, m, y, c) =>
 (floor (2.61 * real (m) - 0.2)
 + d + y + y div 4 + c div 4 - 2 * c) mod 7
in
 val zeller =
 fn (d, 1, y) => zc (d, 11, (y-1) mod 100, (y-1) div 100)
 | (d, 2, y) => zc (d, 12, (y-1) mod 100, (y-1) div 100)
 | (d, m, y) => zc (d, m - 2, y mod 100, y div 100)
end;
