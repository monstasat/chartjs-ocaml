open Js_of_ocaml

(** Indexable type represents a single value or a list of values *)
type 'a t

let of_single (x : 'a) : 'a t Js.t = Obj.magic x

let of_js_array (x : 'a Js.js_array Js.t) : 'a t Js.t =
  Js.Unsafe.coerce x

let of_array (x : 'a array) : 'a t Js.t =
  of_js_array @@ Js.array x

let of_list (x : 'a list) : 'a t Js.t =
  of_array @@ Array.of_list x

let cast_single (t : 'a t Js.t) : 'a Js.opt =
  if Js.instanceof t Js.array_empty
  then Js.null
  else Js.some (Obj.magic t)

let cast_array (t : 'a t Js.t) : 'a Js.js_array Js.t Js.opt =
  if Js.instanceof t Js.array_empty
  then Js.some (Js.Unsafe.coerce t)
  else Js.null

let cast (t : 'a t Js.t) : [`Single of 'a | `Array of 'a Js.js_array Js.t] =
  Js.Opt.case (cast_array t)
    (fun () -> `Single (Obj.magic t))
    (fun x -> `Array x)
