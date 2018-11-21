module Any : sig
  type 'a t
  val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a option
  val get_exn : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val make : int -> 'a -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  val empty : unit -> 'a t
  val concat : 'a t -> 'a t -> 'a Js.js_array Js.t
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
  val of_array : Ojs.t array -> Ojs.t t
  val to_list : 'a t -> 'a list
  val of_list : Ojs.t list -> Ojs.t t
  module Infix :
  sig
    val ( .%[] ) : 'a t -> int -> 'a
    val ( .%[]<- ) : 'a t -> int -> 'a -> unit
  end
  val ( .%[] ) : 'a t -> int -> 'a
  val ( .%[]<- ) : 'a t -> int -> 'a -> unit
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

module Make : functor (M : Chartjs_types.Jsable) ->
              Typed_array with type item := M.t

module String : Typed_array with type item := string
module Int : Typed_array with type item := int
module Float : Typed_array with type item := float
module Color : Typed_array with type item := Chartjs_types.Color.t
