module type M = sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end

type 'a t = 'a Js.js_array Js.t

let t_to_js _ l = Obj.magic l
let t_of_js _ o = Obj.magic o

let length (t : 'a t) : int = t##.length

let get (t : 'a t) (i : int) : 'a option =
  Js.Optdef.to_option @@ Js.array_get t i

let get_exn (t : 'a t) (i : int) : 'a =
  match get t i with
  | None -> raise Not_found
  | Some x -> x

let set (t : 'a t) (i : int) (v : 'a) : unit =
  Js.array_set t i v

let make (len : int) (v : 'a) : 'a t =
  Js.array @@ Array.make len v

let init (len : int) (f : int -> 'a) : 'a t =
  Js.array @@ Array.init len f

let empty () : 'a t =
  new%js Js.array_empty

let concat (a : 'a t) (b : 'a t) =
  a##concat b

let pop (t : 'a t) : 'a option =
  Js.Optdef.to_option t##pop

let push (t : 'a t) (v : 'a) : int =
  t##push v

let push_2 (t : 'a t) (v1 : 'a) (v2 : 'a) : int =
  t##push_2 v1 v2

let push_3 (t : 'a t) (v1 : 'a) (v2 : 'a) (v3 : 'a) : int =
  t##push_3 v1 v2 v3

let push_4 (t : 'a t) (v1 : 'a) (v2 : 'a) (v3 : 'a) (v4 : 'a) : int =
  t##push_4 v1 v2 v3 v4

let reverse (t : 'a t) : 'a t =
  t##reverse

let shift (t : 'a t) : 'a option =
  Js.Optdef.to_option t##shift

let slice ?(till : int option) (t : 'a t) (from : int) : 'a t =
  match till with
  | None -> t##slice_end from
  | Some till -> t##slice from till

let sort (f : 'a -> 'a -> int) (t : 'a t) : 'a t =
  let f a b = f a b |> float_of_int in
  t##sort (Js.wrap_callback f)

let splice (t : 'a t) (from : int) (del_count : int) : 'a t =
  t##splice from del_count

let splice_1 (t : 'a t) (from : int) (del_count : int) (v : 'a) : 'a t =
  t##splice_1 from del_count v

let splice_2 (t : 'a t) (from : int) (del_count : int)
      (v1 : 'a) (v2 : 'a) : 'a t =
  t##splice_2 from del_count v1 v2

let splice_3 (t : 'a t) (from : int) (del_count : int)
      (v1 : 'a) (v2 : 'a) (v3 : 'a) : 'a t =
  t##splice_3 from del_count v1 v2 v3

let splice_4 (t : 'a t) (from : int) (del_count : int)
      (v1 : 'a) (v2 : 'a) (v3 : 'a) (v4 : 'a) : 'a t =
  t##splice_4 from del_count v1 v2 v3 v4

let unshift (t : 'a t) (v : 'a) : int =
  t##unshift v

let unshift_2 (t : 'a t) (v1 : 'a) (v2 : 'a) : int =
  t##unshift_2 v1 v2

let unshift_3 (t : 'a t) (v1 : 'a) (v2 : 'a) (v3 : 'a) : int =
  t##unshift_3 v1 v2 v3

let unshift_4 (t : 'a t) (v1 : 'a) (v2 : 'a) (v3 : 'a) (v4 : 'a) : int =
  t##unshift_4 v1 v2 v3 v4

let some (f : 'a -> int -> 'a t -> bool) (t : 'a t) : bool =
  let f v n a = f v n a |> Js.bool in
  Js.to_bool @@ t##some (Js.wrap_callback f)

let every (f : 'a -> int -> 'a t -> bool) (t : 'a t) : bool =
  let f v n a = f v n a |> Js.bool in
  Js.to_bool @@ t##every (Js.wrap_callback f)

let for_each (f : 'a -> int -> 'a t -> unit) (t : 'a t) : unit =
  t##forEach (Js.wrap_callback f)

let map' (f : 'a -> int -> 'a t -> 'a) (t : 'a t) : 'a t =
  t##map (Js.wrap_callback f)

let map (f : 'a -> 'b) (t : 'a t) : 'b t =
  Js.array_map f t

let mapi (f : int -> 'a -> 'b) (t : 'a t) : 'a t =
  Js.array_mapi f t

let filter (f : 'a -> int -> 'a t -> bool) (t : 'a t) : 'a t =
  let f v n a = f v n a |> Js.bool in
  t##filter (Js.wrap_callback f)

let reduce : 'b. ('b -> 'a -> int -> 'a t -> 'b) -> 'b -> 'a t -> 'b =
  fun f acc t ->
  t##reduce_init (Js.wrap_callback f) acc

let reduce' (f : 'a -> 'a -> int -> 'a t -> 'a) (t : 'a t) : 'a =
  t##reduce (Js.wrap_callback f)

let reduce_right : 'b. ('b -> 'a -> int -> 'a t -> 'b) -> 'b -> 'a t -> 'b =
  fun f acc t ->
  t##reduceRight_init (Js.wrap_callback f) acc

let reduce_right' (f : 'a -> 'a -> int -> 'a t -> 'a) (t : 'a t) : 'a =
  t##reduceRight (Js.wrap_callback f)

module Infix = struct
  let ( .%[] ) (t : 'a t) (i : int) = get_exn t i
  let ( .%[]<- ) (t : 'a t) (i : int) (v : 'a) = set t i v
end

include Infix

module Make(M : M) = struct

  type item = M.t
  type t = Ojs.t Js.js_array Js.t

  let t_to_js l = Obj.magic l
  let t_of_js o = Obj.magic o

  let length (t : t) : int = t##.length

  let get (t : t) (i : int) : item option =
    match Js.Optdef.to_option @@ Js.array_get t i with
    | None -> None
    | Some x -> Some (M.t_of_js x)

  let get_exn (t : t) (i : int) : item =
    match get t i with
    | None -> raise Not_found
    | Some x -> x

  let set (t : t) (i : int) (v : item) : unit =
    Js.array_set t i (M.t_to_js v)

  let make (len : int) (v : item) : t =
    Js.array @@ Array.make len (M.t_to_js v)

  let init (len : int) (f : int -> item) : t =
    let f i = M.t_to_js @@ f i in
    Js.array @@ Array.init len f

  let empty () : t =
    new%js Js.array_empty

  let concat (a : t) (b : t) =
    a##concat b

  let pop (t : t) : item option =
    match Js.Optdef.to_option t##pop with
    | None -> None
    | Some x -> Some (M.t_of_js x)

  let push (t : t) (v : item) : int =
    t##push (M.t_to_js v)

  let push_2 (t : t) (v1 : item) (v2 : item) : int =
    t##push_2 (M.t_to_js v1) (M.t_to_js v2)

  let push_3 (t : t) (v1 : item) (v2 : item) (v3 : item) : int =
    t##push_3 (M.t_to_js v1) (M.t_to_js v2) (M.t_to_js v3)

  let push_4 (t : t) (v1 : item) (v2 : item) (v3 : item) (v4 : item) : int =
    t##push_4 (M.t_to_js v1) (M.t_to_js v2) (M.t_to_js v3) (M.t_to_js v4)

  let reverse (t : t) : t =
    t##reverse

  let shift (t : t) : item option =
    match Js.Optdef.to_option t##shift with
    | None -> None
    | Some x -> Some (M.t_of_js x)

  let slice ?(till : int option) (t : t) (from : int) : t =
    match till with
    | None -> t##slice_end from
    | Some till -> t##slice from till

  let sort (f : item -> item -> int) (t : t) : t =
    let f (a : Ojs.t) (b : Ojs.t) =
      f (M.t_of_js a) (M.t_of_js b) |> float_of_int in
    t##sort (Js.wrap_callback f)

  let splice (t : t) (from : int) (del_count : int) : t =
    t##splice from del_count

  let splice_1 (t : t) (from : int) (del_count : int) (v : item) : t =
    t##splice_1 from del_count (M.t_to_js v)

  let splice_2 (t : t) (from : int) (del_count : int)
        (v1 : item) (v2 : item): t =
    t##splice_2 from del_count (M.t_to_js v1) (M.t_to_js v2)

  let splice_3 (t : t) (from : int) (del_count : int)
        (v1 : item) (v2 : item) (v3 : item) : t =
    t##splice_3 from del_count (M.t_to_js v1) (M.t_to_js v2) (M.t_to_js v3)

  let splice_4 (t : t) (from : int) (del_count : int)
        (v1 : item) (v2 : item) (v3 : item) (v4 : item): t =
    t##splice_4 from del_count
      (M.t_to_js v1) (M.t_to_js v2) (M.t_to_js v3) (M.t_to_js v4)

  let unshift (t : t) (v : item) : int =
    t##unshift (M.t_to_js v)

  let unshift_2 (t : t) (v1 : item) (v2 : item) : int =
    t##unshift_2 (M.t_to_js v1) (M.t_to_js v2)

  let unshift_3 (t : t) (v1 : item) (v2 : item) (v3 : item) : int =
    t##unshift_3 (M.t_to_js v1) (M.t_to_js v2) (M.t_to_js v3)

  let unshift_4 (t : t) (v1 : item) (v2 : item) (v3 : item) (v4 : item) : int =
    t##unshift_4 (M.t_to_js v1) (M.t_to_js v2) (M.t_to_js v3) (M.t_to_js v4)

  let some (f : item -> int -> t -> bool) (t : t) : bool =
    let f (v : Ojs.t) n a = f (M.t_of_js v) n a |> Js.bool in
    Js.to_bool @@ t##some (Js.wrap_callback f)

  let every (f : item -> int -> t -> bool) (t : t) : bool =
    let f (v : Ojs.t) n a = f (M.t_of_js v) n a |> Js.bool in
    Js.to_bool @@ t##every (Js.wrap_callback f)

  let for_each (f : item -> int -> t -> unit) (t : t) : unit =
    let f (v : Ojs.t) n a = f (M.t_of_js v) n a in
    t##forEach (Js.wrap_callback f)

  let map (f : item -> int -> t -> item) (t : t) : t =
    let f (v : Ojs.t) n a = f (M.t_of_js v) n a |> M.t_to_js in
    t##map (Js.wrap_callback f)

  let mapi (f : int -> 'a -> 'b) (t : t) : t =
    Js.array_mapi f t

  let filter (f : item -> int -> t -> bool) (t : t) : t =
    let f (v : Ojs.t) n a = f (M.t_of_js v) n a |> Js.bool in
    t##filter (Js.wrap_callback f)

  let reduce : 'b. ('b -> item -> int -> t -> 'b) -> 'b -> t -> 'b =
    fun f acc t ->
    let f acc (v : Ojs.t) n a = f acc (M.t_of_js v) n a in
    t##reduce_init (Js.wrap_callback f) acc

  let reduce' (f : item -> item -> int -> t -> 'a) (t : t) : 'a =
    let f (acc : Ojs.t) (v : Ojs.t) n a = f (M.t_of_js acc) (M.t_of_js v) n a in
    t##reduce (Js.wrap_callback f)

  let reduce_right : 'b. ('b -> item -> int -> t -> 'b) -> 'b -> t -> 'b =
    fun f acc t ->
    let f acc (v : Ojs.t) n a = f acc (M.t_of_js v) n a in
    t##reduceRight_init (Js.wrap_callback f) acc

  let reduce_right' (f : item -> item -> int -> t -> 'a) (t : t) : 'a =
    let f (acc : Ojs.t) (v : Ojs.t) n a = f (M.t_of_js acc) (M.t_of_js v) n a in
    t##reduceRight (Js.wrap_callback f)

  module Infix = struct
    let ( .%[] ) (t : t) (i : int) = get_exn t i
    let ( .%[]<- ) (t : t) (i : int) (v : item) = set t i v
  end

  include Infix

end

module String =
  Make(struct
      type t = string
      let t_of_js (o : Ojs.t) : t = Ojs.string_of_js o
      let t_to_js (t : t) : Ojs.t = Ojs.string_to_js t
    end)
module Int =
  Make(struct
      type t = int
      let t_of_js (o : Ojs.t) : t = Ojs.int_of_js o
      let t_to_js (t : t) : Ojs.t = Ojs.int_to_js t
    end)
module Float =
  Make(struct
      type t = float
      let t_of_js (o : Ojs.t) : t = Ojs.float_of_js o
      let t_to_js (t : t) : Ojs.t = Ojs.float_to_js t
    end)
module Bool =
  Make(struct
      type t = bool
      let t_of_js (o : Ojs.t) : t = Ojs.bool_of_js o
      let t_to_js (t : t) : Ojs.t = Ojs.bool_to_js t
    end)
module Color = Make(Chartjs_types.Color)
