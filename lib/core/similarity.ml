let cosine_similarity v1 v2 =
  let dot = ref 0. in
  let norm1 = ref 0. in
  let norm2 = ref 0. in
  for i = 0 to Array.length v1 - 1 do
    dot := !dot +. v1.(i) *. v2.(i);
    norm1 := !norm1 +. v1.(i) *. v1.(i);
    norm2 := !norm2 +. v2.(i) *. v2.(i)
  done;
  if !norm1 = 0. || !norm2 = 0. then 0.
  else !dot /. (sqrt !norm1 *. sqrt !norm2)

let pearson_correlation xs ys =
  let n = float_of_int (List.length xs) in
  let mean l = List.fold_left ( +. ) 0. l /. n in
  let mx = mean xs and my = mean ys in
  let cov = List.fold_left2 (fun acc x y -> acc +. (x -. mx) *. (y -. my)) 0. xs ys in
  let sx = sqrt (List.fold_left (fun acc x -> acc +. (x -. mx) ** 2.) 0. xs) in
  let sy = sqrt (List.fold_left (fun acc y -> acc +. (y -. my) ** 2.) 0. ys) in
  if sx = 0. || sy = 0. then 0. else cov /. (sx *. sy)

let knn vectors idx k =
  let v = vectors.(idx) in
  let sims =
    Array.mapi (fun j wv -> if j = idx then (-1.0, j) else (cosine_similarity v wv, j)) vectors
  in
  Array.sort (fun (a,_) (b,_) -> compare b a) sims;
  Array.sub sims 0 k