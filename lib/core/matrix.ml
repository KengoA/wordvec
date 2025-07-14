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

module Matrix : MatrixSig = struct
  type t = float array array

  let zeros rows cols = Array.make_matrix rows cols 0.0

  let random rows cols =
    Array.init rows (fun _ -> Array.init cols (fun _ -> Random.float 1.0))

  let shape m = (Array.length m, Array.length m.(0))
  let get m i j = m.(i).(j)
  let set m i j v = m.(i).(j) <- v

  let dot a b =
    let rows, cols = (Array.length a, Array.length b.(0)) in
    let res = Array.make_matrix rows cols 0.0 in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        for k = 0 to Array.length b - 1 do
          res.(i).(j) <- res.(i).(j) +. (a.(i).(k) *. b.(k).(j))
        done
      done
    done ;
    res

  let add a b =
    Array.mapi (fun i row -> Array.mapi (fun j v -> v +. b.(i).(j)) row) a

  let sub a b =
    Array.mapi (fun i row -> Array.mapi (fun j v -> v -. b.(i).(j)) row) a

  let map f m = Array.map (Array.map f) m
  let copy m = Array.map Array.copy m
end
