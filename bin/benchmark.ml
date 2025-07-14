open Printf

let parse_wordsim353 filename =
  let ic = open_in filename in
  let _ = input_line ic in
  let rec loop acc =
    match input_line ic with
    | line ->
      let cols = String.split_on_char ',' line in
      (match cols with
      | w1 :: w2 :: score :: _ ->
        (try
           loop ((w1, w2, float_of_string score) :: acc)
         with Failure _ ->
           Printf.eprintf "Skipping line (bad score): %s\n%!" line;
           loop acc)
      | _ -> loop acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in loop []

let evaluate_wordsim353 embeddings_file wordsim_file =
  let words, embeddings = Wordvec.SkipGram.load ~filename:embeddings_file in
  let pairs = parse_wordsim353 wordsim_file in
  let cosines, scores =
    List.fold_left (fun (coss, scs) (w1, w2, score) ->
      match 
        (
          Array.fold_left (fun acc (i, w) -> if w = w1 then Some i else acc) None (Array.mapi (fun i w -> (i, w)) words),
          Array.fold_left (fun acc (i, w) -> if w = w2 then Some i else acc) None (Array.mapi (fun i w -> (i, w)) words)
        ) with
      | Some idx1, Some idx2 ->
        let cos = Wordvec.Core.Similarity.cosine_similarity embeddings.(idx1) embeddings.(idx2) in
        (cos :: coss, score :: scs)
      | _ -> (coss, scs)
    ) ([], []) pairs
  in
  let cosines = List.rev cosines in
  let scores = List.rev scores in
  let corr = Wordvec.Core.Similarity.pearson_correlation cosines scores in
  printf "Pairs evaluated: %d\n" (List.length cosines);
  printf "Pearson correlation between cosine similarity and WordSim-353: %.4f\n" corr

let () =
  evaluate_wordsim353 "data/artifacts/embeddings.csv" "data/eval/wordsim353crowd.csv"