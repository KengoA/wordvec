let parallel_map f lst n_workers =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  let results = Array.make len None in
  let next = Atomic.make 0 in
  let worker () =
    let rec loop () =
      let i = Atomic.fetch_and_add next 1 in
      if i < len then (
        results.(i) <- Some (f arr.(i)) ;
        loop ())
    in
    loop ()
  in
  let domains = Array.init n_workers (fun _ -> Domain.spawn worker) in
  Array.iter Domain.join domains ;
  Array.to_list (Array.map Option.get results)
