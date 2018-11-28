type 'a js_array
let js_array_to_js (_ : 'a -> Ojs.t) (t : 'a js_array) : Ojs.t = Obj.magic t
let js_array_of_js (_ : Ojs.t -> 'a) (j : Ojs.t) : 'a js_array = Obj.magic j

module Make(M : Types.Jsable) = struct
  type t = M.t js_array
  let t_to_js = js_array_to_js M.t_to_js
  let t_of_js = js_array_of_js M.t_of_js

  let item_to_js = M.t_to_js
  let item_of_js = M.t_of_js

  type 'a callback = 'a -> M.t -> int -> t -> 'a
  let callback_to_js (f : Ojs.t -> 'a) (x : 'a callback) : Ojs.t =
    Ojs.fun_to_js 4
      (fun acc ->
        (fun item ->
          (fun index ->
            (fun arr ->
              x (f acc) (M.t_of_js item)
                (Ojs.int_of_js index) (t_of_js arr)))))

  val length : t -> int [@@js.get]

  val concat : t -> t -> t [@@js.call]
  val pop : t -> M.t option [@@js.call]
  val push : t -> (M.t list [@js.variadic]) -> int [@@js.call]
  val shift : t -> M.t option [@@js.call]
  val unshift : t -> (M.t list [@js.variadic]) -> int [@@js.call]
  val reverse : t -> t [@@js.call]
  val some : t -> (M.t -> int -> t -> bool) -> bool [@@js.call]
  val every : t -> (M.t -> int -> t -> bool) -> bool [@@js.call]

  let reduce (t : t) (callback : 'a callback) (acc : 'b) : 'b =
    [|(callback_to_js Obj.magic callback); (Obj.magic acc)|]
    |> Ojs.call (t_to_js t) "reduce"
    |> Obj.magic

  let reduce' ?(init : M.t option) (t : t) (callback : M.t callback) : M.t =
    let cb = callback_to_js M.t_of_js callback in
    let arr = match init with
      | None -> [|cb|]
      | Some init -> [|cb; M.t_to_js init|] in
    Obj.magic @@ Ojs.call (t_to_js t) "reduce" arr

  let make (n : int) : t =
    t_of_js @@ Ojs.array_make n

  let get (t : t) (i : int) : M.t option =
    Ojs.option_of_js M.t_of_js @@ Ojs.array_get (t_to_js t) i

  let get_exn (t : t) (i : int) : M.t = match get t i with
    | None -> raise Not_found
    | Some x -> x

  let set (t : t) (i : int) (v : M.t) : unit =
    Ojs.array_set (t_to_js t) i (M.t_to_js v)

  let of_list (l : M.t list) : t = t_of_js @@ [%js.of: M.t list] l
  let to_list (t : t) : M.t list = [%js.to: M.t list] @@ t_to_js t

  let of_array (l : M.t array) : t = t_of_js @@ [%js.of: M.t array] l
  let to_array (t : t) : M.t array = [%js.to: M.t array] @@ t_to_js t


  let ( .%[] ) (t : t) (i : int) = get_exn t i
  let ( .%[]<- ) (t : t) (i : int) (v : M.t) = set t i v

end

module type Typed = sig
  type item
  val item_to_js : item -> Ojs.t
  val item_of_js : Ojs.t -> item
  include module type of Make(struct
                             type t = item
                             let t_to_js = item_to_js
                             let t_of_js = item_of_js
                           end)
end

type 'a t = 'a js_array

let t_to_js = js_array_to_js
let t_of_js = js_array_of_js

module String = Make(Types.String)
module Int = Make(Types.Int)
module Int32 = Make(Types.Int32)
module Int64 = Make(Types.Int64)
module Float = Make(Types.Float)
module Bool = Make(Types.Bool)
module Color = Make(Types.Color)
