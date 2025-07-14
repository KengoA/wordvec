val tokenize : string -> string list
val build_vocab : string list -> (string, int) Hashtbl.t * int

val generate_pairs :
  string list -> int -> (string, int) Hashtbl.t -> (int * int) Seq.t

val build_negative_sampling_table :
  string list -> (string, int) Hashtbl.t -> int -> int array

val sample_negative : int array -> int -> int -> int list

val build_vocab_with_freq :
  string list -> (string, int) Hashtbl.t * int * (string * int) list

val save_vocab_freq :
  filename:string -> sorted_vocab:(string * int) list -> unit

val preprocess_file : input_txt:string -> output_txt:string -> unit

module Text_utils : sig
  val tokenize : string -> string list
  val build_vocab : string list -> (string, int) Hashtbl.t * int

  val generate_pairs :
    string list -> int -> (string, int) Hashtbl.t -> (int * int) Seq.t

  val build_negative_sampling_table :
    string list -> (string, int) Hashtbl.t -> int -> int array

  val sample_negative : int array -> int -> int -> int list

  val build_vocab_with_freq :
    string list -> (string, int) Hashtbl.t * int * (string * int) list

  val save_vocab_freq :
    filename:string -> sorted_vocab:(string * int) list -> unit

  val preprocess_file : input_txt:string -> output_txt:string -> unit
end
