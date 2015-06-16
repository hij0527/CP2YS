function fun (x, y) =
  return -x -y
end
x := 1;
y := 2;
z := 3;
readint w;
if (w < 5)
  then
    x := 2;
    z := fun(x, w)
  else
    if (w > 10)
      then
        x := 2
      else
        x := fun(4, 1);
        z := w + x
      end
  end;
if (z - y == 0)
  then
    accept
  else
    reject
  end
