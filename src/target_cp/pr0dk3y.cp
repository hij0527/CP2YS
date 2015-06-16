(* hello world *)
function k20_1_E10 (a1) = 
  return ((a1 shr 8) & 255) == (a1 & 255)
  end
function k20_2_DA2 (a1) =
  return (0 == (((a1 shr 12) ^ (a1 shr 8)) & (15)))
      or (0 == (((a1 shr 8) ^ ((a1 shr 4) & 255)) & (15)))
      or (0 == ((((a1 shr 4) & 255) ^ (a1 & 255)) & (15)))
  end
readint x;
if (k20_1_E10(x) or k20_2_DA2(x)) then reject else accept end
