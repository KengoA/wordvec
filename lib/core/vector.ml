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

module Vector : VectorSig = struct
  type t = float array
  let zeros n = Array.make n 0.0
  let random n = Array.init n (fun _ -> Random.float 1.0 -. 0.5)
  let get v i = v.(i)
  let set v i x = v.(i) <- x
  let add a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))
  let sub a b = Array.init (Array.length a) (fun i -> a.(i) -. b.(i))
  let dot a b =
    let sum = ref 0.0 in
    for i = 0 to Array.length a - 1 do
      sum := !sum +. a.(i) *. b.(i)
    done;
    !sum
  let map f v = Array.map f v
  let copy v = Array.copy v
end
