module type VectorSig = sig
  type t

  val zeros : int -> t
  val random : int -> t
  val get : t -> int -> float
  val set : t -> int -> float -> unit
  val add : t -> t -> t
  val sub : t -> t -> t
  val dot : t -> t -> float
  val map : (float -> float) -> t -> t
  val copy : t -> t
end

module Vector : VectorSig
