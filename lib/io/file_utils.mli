val split_file :
  filename:string -> chunk_size_mb:int -> out_dir:string -> string list

val read_chunks :
  filename:string -> chunk_size_mb:int -> f:(string -> unit) -> string list

val cleanup_chunks : string list -> unit
val get_file_extension : string -> string
val is_xml_file : string -> bool
val is_txt_file : string -> bool
val prepare_text_file : input_file:string -> string
