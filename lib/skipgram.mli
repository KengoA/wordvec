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

module SkipGram : SkipGramSig
