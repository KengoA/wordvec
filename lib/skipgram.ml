open Preprocess.Text_utils

module type SkipGramSig = sig
  type t

  val create : vocab_size:int -> embed_dim:int -> t

  val train :
    t ->
    (int * int) Seq.t ->
    neg_table:int array ->
    neg_samples:int ->
    epochs:int ->
    unit

  val get_embedding : t -> int -> float array

  val save :
    filename:string ->
    model:t ->
    embed_dim:int ->
    sorted_vocab:(string * 'a) list ->
    vocab_tbl:(string, int) Hashtbl.t ->
    unit

  val load : filename:string -> string array * float array array
end

module SkipGram : SkipGramSig = struct
  type t = {
    w_in : float array array;
    w_out : float array array;
    embed_dim : int;
  }

  let create ~vocab_size ~embed_dim =
    {
      w_in =
        Array.init vocab_size (fun _ ->
            Array.init embed_dim (fun _ -> Random.float 1.0));
      w_out =
        Array.init vocab_size (fun _ ->
            Array.init embed_dim (fun _ -> Random.float 1.0));
      embed_dim;
    }

  let sigmoid x = 1.0 /. (1.0 +. exp (-.x))

  let sgd_step model input_idx context_idx neg_table neg_samples lr =
    let input_vec = model.w_in.(input_idx) in
    let update output_idx label =
      let output_vec = model.w_out.(output_idx) in
      let dot = ref 0.0 in
      for i = 0 to model.embed_dim - 1 do
        dot := !dot +. (input_vec.(i) *. output_vec.(i))
      done ;
      let pred = sigmoid !dot in
      let error = pred -. label in
      for i = 0 to model.embed_dim - 1 do
        let grad_in = error *. output_vec.(i) in
        let grad_out = error *. input_vec.(i) in
        model.w_in.(input_idx).(i) <-
          model.w_in.(input_idx).(i) -. (lr *. grad_in) ;
        model.w_out.(output_idx).(i) <-
          model.w_out.(output_idx).(i) -. (lr *. grad_out)
      done
    in
    update context_idx 1.0 ;
    let negs = Text_utils.sample_negative neg_table neg_samples context_idx in
    List.iter (fun neg_idx -> update neg_idx 0.0) negs

  let train model pairs ~neg_table ~neg_samples ~epochs =
    let lr = 0.01 in
    for epoch = 1 to epochs do
      Seq.iter
        (fun (input_idx, context_idx) ->
          sgd_step model input_idx context_idx neg_table neg_samples lr)
        pairs ;
      Printf.printf "[Epoch %d/%d] finished\n" epoch epochs ;
      flush stdout
    done

  let get_embedding model idx = Array.copy model.w_in.(idx)

  let save ~filename ~model ~embed_dim ~sorted_vocab ~vocab_tbl =
    let oc = open_out filename in
    Printf.fprintf oc "word" ;
    for d = 0 to embed_dim - 1 do
      Printf.fprintf oc ",dim%d" d
    done ;
    Printf.fprintf oc "\n" ;
    List.iter
      (fun (word, _) ->
        let idx = Hashtbl.find vocab_tbl word in
        let emb = get_embedding model idx in
        Printf.fprintf oc "%s" word ;
        Array.iter (fun x -> Printf.fprintf oc ",%f" x) emb ;
        Printf.fprintf oc "\n")
      sorted_vocab ;
    close_out oc

  let load ~filename =
    let ic = open_in filename in
    let header = input_line ic in
    let dims = List.length (String.split_on_char ',' header) - 1 in
    let words = ref [] in
    let vectors = ref [] in
    (try
       while true do
         let line = input_line ic in
         match String.split_on_char ',' line with
         | word :: floats when List.length floats = dims ->
           let vec =
             Array.init dims (fun i -> float_of_string (List.nth floats i))
           in
           words := word :: !words ;
           vectors := vec :: !vectors
         | _ -> ()
       done
     with End_of_file -> close_in ic) ;
    (Array.of_list (List.rev !words), Array.of_list (List.rev !vectors))
end
