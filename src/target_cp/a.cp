function f (x, z) =
  y := 123;
  k := 10 * x + z;
  if (y == k)
    then return true
    else return k == z
  end
end
function g (y, w) =
  if (y % 32 != 16) then return 0 else () end;
  y := (y shr 5) * w;
  y := y & 3;
  if (y == 3)
    then return 0
    else return y
  end
end
function h (a, b) =
  if b == 0 then return false else a := a + 1 end;
  if a % b != 0 then return false else a := a + g(b, 1) end;
  if a + b > 150 then return false else a := a + 1 end;
  if (a - b) shl 1 > 200 then return false else () end;
  return a >= 80 and b <= 20
end

readint v;
readint x;
readint y;
z := x + y;
w := 0;
if v > 100 or x > 20 or y < 10
  then reject
  else ()
end;
if (f(x, z))
  then (w := 3)
  else ()
end;
if (g(v, w) == 0)
  then reject
  else z := v + z
end;
if (h(z, y))
  then accept
  else reject
end
