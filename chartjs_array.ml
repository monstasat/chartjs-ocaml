module Any : sig
  type 'a t = 'a Js.js_array Js.t
  val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a option
  val get_exn : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val make : int -> 'a -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  val empty : unit -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val pop : 'a t -> 'a option
  val push : 'a t -> 'a -> int
  val push_2 : 'a t -> 'a -> 'a -> int
  val push_3 : 'a t -> 'a -> 'a -> 'a -> int
  val push_4 : 'a t -> 'a -> 'a -> 'a -> 'a -> int
  val reverse : 'a t -> 'a t
  val shift : 'a t -> 'a option
  val slice : ?till:int -> 'a t -> int -> 'a t
  val sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val splice : 'a t -> int -> int -> 'a t
  val splice_1 : 'a t -> int -> int -> 'a -> 'a t
  val splice_2 : 'a t -> int -> int -> 'a -> 'a -> 'a t
  val splice_3 : 'a t -> int -> int -> 'a -> 'a -> 'a -> 'a t
  val splice_4 : 'a t -> int -> int -> 'a -> 'a -> 'a -> 'a -> 'a t
  val unshift : 'a t -> 'a -> int
  val unshift_2 : 'a t -> 'a -> 'a -> int
  val unshift_3 : 'a t -> 'a -> 'a -> 'a -> int
  val unshift_4 : 'a t -> 'a -> 'a -> 'a -> 'a -> int
  val some : ('a -> int -> 'a t -> bool) -> 'a t -> bool
  val every : ('a -> int -> 'a t -> bool) -> 'a t -> bool
  val for_each : ('a -> int -> 'a t -> unit) -> 'a t -> unit
  val map : ('a -> int -> 'a t -> 'a) -> 'a t -> 'a t
  val map' : ('a -> 'b) -> 'a t -> 'b t
  val mapi' : (int -> 'a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> int -> 'a t -> bool) -> 'a t -> 'a t
  val reduce : ('b -> 'a -> int -> 'a t -> 'b) -> 'b -> 'a t -> 'b
  val reduce' : ('a -> 'a -> int -> 'a t -> 'a) -> 'a t -> 'a
  val reduce_right : ('b -> 'a -> int -> 'a t -> 'b) -> 'b -> 'a t -> 'b
  val reduce_right' : ('a -> 'a -> int -> 'a t -> 'a) -> 'a t -> 'a
  val to_array : 'a t -> 'a array
  val of_array : 'a array -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  module Infix :
  sig
    val ( .%[] ) : 'a t -> int -> 'a
    val ( .%[]<- ) : 'a t -> int -> 'a -> unit
  end
  val ( .%[] ) : 'a t -> int -> 'a
  val ( .%[]<- ) : 'a t -> int -> 'a -> unit
end = struct
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

  let map (f : 'a -> int -> 'a t -> 'a) (t : 'a t) : 'a t =
    t##map (Js.wrap_callback f)

  let map' (f : 'a -> 'b) (t : 'a t) : 'b t =
    Js.array_map f t

  let mapi' (f : int -> 'a -> 'b) (t : 'a t) : 'b t =
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

  let to_array (t : 'a t) : 'a array =
    let n = length t in
    Array.init n (fun i -> get_exn t i)

  let of_array (arr : 'a array) : 'a t =
    let a = Ojs.array_to_js (fun (x : 'a) -> Obj.magic x) arr in
    t_of_js (fun x -> x) a

  let to_list (t : 'a t) : 'a list =
    Array.to_list @@ to_array t

  let of_list (lst : 'a list) : 'a t =
    of_array @@ Array.of_list lst

  module Infix = struct
    let ( .%[] ) (t : 'a t) (i : int) = get_exn t i
    let ( .%[]<- ) (t : 'a t) (i : int) (v : 'a) = set t i v
  end

  include Infix
end

module type Typed_array = sig
  type item
  type t = Ojs.t Any.t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val length : t -> int
  val get : t -> int -> item option
  val get_exn : t -> int -> item
  val set : t -> int -> item -> unit
  val make : int -> item -> t
  val init : int -> (int -> item) -> t
  val empty : unit -> t
  val concat : t -> t -> t
  val pop : t -> item option
  val push : t -> item -> int
  val push_2 : t -> item -> item -> int
  val push_3 : t -> item -> item -> item -> int
  val push_4 : t -> item -> item -> item -> item -> int
  val reverse : t -> t
  val shift : t -> item option
  val slice : ?till:int -> t -> int -> t
  val sort : (item -> item -> int) -> t -> t
  val splice : t -> int -> int -> t
  val splice_1 : t -> int -> int -> item -> t
  val splice_2 : t -> int -> int -> item -> item -> t
  val splice_3 : t -> int -> int -> item -> item -> item -> t
  val splice_4 : t -> int -> int -> item -> item -> item -> item -> t
  val unshift : t -> item -> int
  val unshift_2 : t -> item -> item -> int
  val unshift_3 : t -> item -> item -> item -> int
  val unshift_4 : t -> item -> item -> item -> item -> int
  val some : (item -> int -> t -> bool) -> t -> bool
  val every : (item -> int -> t -> bool) -> t -> bool
  val for_each : (item -> int -> t -> unit) -> t -> unit
  val map : (item -> int -> t -> item) -> t -> t
  val filter : (item -> int -> t -> bool) -> t -> t
  val reduce : ('b -> item -> int -> t -> 'b) -> 'b -> t -> 'b
  val reduce' : (item -> item -> int -> t -> item) -> t -> item
  val reduce_right : ('b -> item -> int -> t -> 'b) -> 'b -> t -> 'b
  val reduce_right' : (item -> item -> int -> t -> item) -> t -> item
  val to_array : t -> item array
  val of_array : item array -> t
  val to_list : t -> item list
  val of_list : item list -> t
  module Infix :
  sig
    val ( .%[] ) : t -> int -> item
    val ( .%[]<- ) : t -> int -> item -> unit
  end
  val ( .%[] ) : t -> int -> item
  val ( .%[]<- ) : t -> int -> item -> unit
end

module Make(M : Chartjs_types.Jsable)
       : Typed_array with type item := M.t = struct

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

  let concat (a : t) (b : t) : t =
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

  let filter (f : item -> int -> t -> bool) (t : t) : t =
    let f (v : Ojs.t) n a = f (M.t_of_js v) n a |> Js.bool in
    t##filter (Js.wrap_callback f)

  let reduce : 'b. ('b -> item -> int -> t -> 'b) -> 'b -> t -> 'b =
    fun f acc t ->
    let f acc (v : Ojs.t) n a = f acc (M.t_of_js v) n a in
    t##reduce_init (Js.wrap_callback f) acc

  let reduce' (f : item -> item -> int -> t -> item) (t : t) : item =
    let f (acc : Ojs.t) (v : Ojs.t) n a =
      M.t_to_js @@ f (M.t_of_js acc) (M.t_of_js v) n a in
    M.t_of_js @@ t##reduce (Js.wrap_callback f)

  let reduce_right : 'b. ('b -> item -> int -> t -> 'b) -> 'b -> t -> 'b =
    fun f acc t ->
    let f acc (v : Ojs.t) n a = f acc (M.t_of_js v) n a in
    t##reduceRight_init (Js.wrap_callback f) acc

  let reduce_right' (f : item -> item -> int -> t -> item) (t : t) : item =
    let f (acc : Ojs.t) (v : Ojs.t) n a =
      M.t_to_js @@ f (M.t_of_js acc) (M.t_of_js v) n a in
    M.t_of_js (t##reduceRight (Js.wrap_callback f))

  let to_array (t : t) : item array =
    let n = length t in
    Array.init n (fun i -> get_exn t i)

  let of_array (arr : item array) : t =
    let a = Ojs.array_to_js M.t_to_js arr in
    t_of_js a

  let to_list (t : t) : item list =
    Array.to_list @@ to_array t

  let of_list (lst : item list) : t =
    of_array @@ Array.of_list lst

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
