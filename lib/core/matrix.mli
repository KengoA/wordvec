module type MatrixSig = sig
  type t

  val zeros : int -> int -> t
  val random : int -> int -> t
  val shape : t -> int * int
  val get : t -> int -> int -> float
  val set : t -> int -> int -> float -> unit
  val dot : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val map : (float -> float) -> t -> t
  val copy : t -> t
end
