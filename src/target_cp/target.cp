x := 1;
y := 2;
z := 3;
readint w;
if (w < 5)
  then
    x := 2;
    z := x shl 2
  else
    if (not (w < 10))
      then
        x := 2
      else
        x := -5;
        z := w + x
      end
  end;
if (z - y == 0)
  then
    accept
  else
    reject
  end
