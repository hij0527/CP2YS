(* hello world *)
(*
let byte_2030B0 := [0, 1, 3, 2, 6, 7, 5, 4, 12, 13, 15, 14, 10, 11] in
let byte_1FC0 := [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8] in
*)
function k20_1_E10 (a1) = 
  return ((a1 shr 8) & 255) == (a1 & 255)
  end
function k20_2_DA2 (a1) =
  return (not (((a1 shr 12) ^ (a1 shr 8)) & (15)))
      or (not (((a1 shr 8) ^ ((a1 shr 4) & 255)) & (15)))
      or (not ((((a1 shr 4) & 255) ^ (a1 & 255)) & (15)))
  end
readint x;
if (k20_1_E10(x) or k20_2_DA2(x)) then reject else accept end
