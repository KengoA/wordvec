let () =
  let words, vectors =
    Wordvec.SkipGram.load ~filename:"data/artifacts/embeddings.csv"
  in
  let predefined_words =
    [
      "basketball";
      "olive";
      "computer";
      "apple";
      "football";
      "car";
      "dog";
      "cat";
      "tree";
      "house";
      "river";
      "mountain";
      "ocean";
      "book";
      "phone";
      "music";
      "movie";
      "art";
      "science";
      "language";
      "travel";
      "food";
      "health";
      "education";
      "history";
      "technology";
      "nature";
      "culture";
      "sports";
      "fashion";
      "photography";
      "gaming";
      "writing";
      "politics";
      "economy";
      "psychology";
      "philosophy";
      "environment";
      "astronomy";
      "biology";
      "chemistry";
      "physics";
      "mathematics";
      "geography";
      "sociology";
      "anthropology";
      "architecture";
      "engineering";
      "medicine";
      "law";
      "business";
      "finance";
      "marketing";
      "journalism";
    ]
  in
  Printf.printf
    "Evaluating top 5 most similar words for predefined set (%d words)...\n"
    (List.length predefined_words) ;
  List.iter
    (fun word ->
      match
        Array.fold_left
          (fun acc (i, w) -> if w = word then Some i else acc)
          None
          (Array.mapi (fun i w -> (i, w)) words)
      with
      | Some idx ->
        let top5 = Wordvec.Core.Similarity.knn vectors idx 5 in
        Printf.printf "%s:\t" word ;
        Array.iter
          (fun (_, j) -> if j >= 0 then Printf.printf "%s\t" words.(j))
          top5 ;
        print_endline ""
      | None -> Printf.printf "%s:\tNOT_FOUND\n" word)
    predefined_words
