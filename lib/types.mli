(** Chart instance *)
type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

(** ChartJS chart (or dataset) type *)
type typ =
  [ `Line [@js "line"]
  | `Bar [@js "bar"]
  | `Horizontal_bar [@js "horizontalBar"]
  | `Radar [@js "radar"]
  | `Doughnut [@js "doughnut"]
  | `Pie [@js "pie"]
  | `Polar_area [@js "polarArea"]
  | `Bubble [@js "bubble"]
  | `Scatter [@js "scatter"]
  | `Custom of string [@js.default]
  ] [@js.enum]
val typ_to_js : typ -> Ojs.t
val typ_of_js : Ojs.t -> typ

(** Type 'or_false' is the OCaml option type mapped
    to 'false' when the value is None *)
[@@@js.stop]
type 'a or_false = 'a option
val or_false_to_js : ('a -> Ojs.t) -> 'a or_false -> Ojs.t
val or_false_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a or_false
[@@@js.start]
[@@@js.implem
 type 'a or_false = 'a option
 let or_false_to_js f = function
   | None -> Ojs.bool_to_js false
   | Some x -> f x
 let or_false_of_js f js =
   match Ojs.obj_type js with
   | "[object Boolean]" ->
      if Ojs.bool_of_js js then assert false else None
   | _ -> Some (f js)
]

(** 'text' type represents multiline string which is converted
    to an array of strings if '\n' char is present *)
[@@@js.stop]
type text = string
val text_to_js : text -> Ojs.t
val text_of_js : Ojs.t -> text
[@@@js.start]
[@@@js.implem
 type text = string
 let text_to_js (s : text) : Ojs.t =
   match String.split_on_char '\n' s with
   | [s] -> Ojs.string_to_js s
   | l -> Ojs.list_to_js Ojs.string_to_js l
 let text_of_js (js : Ojs.t) : text =
   match Ojs.obj_type js with
   | "[object Array]" ->
      let l = Ojs.list_of_js Ojs.string_of_js js in
      String.concat "\n" l
   | "[object String]" ->
      Ojs.string_of_js js
   | _ -> assert false
]

type point_style =
  [ `Circle [@js "circle"]
  | `Cross [@js "cross"]
  | `Cross_rot [@js "crossRot"]
  | `Dash [@js "dash"]
  | `Line [@js "line"]
  | `Rect [@js "rect"]
  | `Rect_rounded [@js "rectRounded"]
  | `Rect_rot [@js "rectRot"]
  | `Star [@js "star"]
  | `Triangle [@js "triangle"]
  ] [@js.enum]
val point_style_to_js : point_style -> Ojs.t
val point_style_of_js : Ojs.t -> point_style

type easing =
  [ `Linear [@js "linear"]
  | `Ease_in_quad [@js "easeInQuad"]
  | `Ease_out_quad [@js "easeOutQuad"]
  | `Ease_in_out_quad [@js "easeInOutQuad"]
  | `Ease_in_cubic [@js "easeInCubic"]
  | `Ease_out_cubic [@js "easeOutCubic"]
  | `Ease_in_out_cubic [@js "easeInOutCubic"]
  | `Ease_in_quart [@js "easeInQuart"]
  | `Ease_out_quart [@js "easeOutQuart"]
  | `Ease_in_out_quart [@js "easeInOutQuart"]
  | `Ease_in_quint [@js "easeInQuint"]
  | `Ease_out_quint [@js "easeOutQuint"]
  | `Ease_in_out_quint [@js "easeInOutQuint"]
  | `Ease_in_sine [@js "easeInSine"]
  | `Ease_out_sine [@js "easeOutSine"]
  | `Ease_in_out_sine [@js "easeInOutSine"]
  | `Ease_in_expo [@js "easeInExpo"]
  | `Ease_out_expo [@js "easeOutExpo"]
  | `Ease_in_out_expo [@js "easeInOutExpo"]
  | `Ease_in_circ [@js "easeInCirc"]
  | `Ease_out_circ [@js "easeOutCirc"]
  | `Ease_in_out_circ [@js "easeInOutCirc"]
  | `Ease_in_elastic [@js "easeInElastic"]
  | `Ease_out_elastic [@js "easeOutElastic"]
  | `Ease_in_out_elastic [@js "easeInOutElastic"]
  | `Ease_in_back [@js "easeInBack"]
  | `Ease_out_back [@js "easeOutBack"]
  | `Ease_in_out_back [@js "easeInOutBack"]
  | `Ease_in_bounce [@js "easeInBounce"]
  | `Ease_out_bounce [@js "easeOutBounce"]
  | `Ease_in_out_bounce [@js "easeInOutBounce"]
  | `Unknown of string [@js.default]
  ] [@js.enum] [@@deriving show]
val easing_to_js : easing -> Ojs.t
val easing_of_js : Ojs.t -> easing

type border_dash = int list
val border_dash_to_js : border_dash -> Ojs.t
val border_dash_of_js : Ojs.t -> border_dash

type border_dash_offset = float
val border_dash_offset_to_js : border_dash_offset -> Ojs.t
val border_dash_offset_of_js : Ojs.t -> border_dash_offset

type line_cap =
  [ `Butt [@js "butt"]
  | `Round [@js "round"]
  | `Square [@js "square"]
  ] [@js.enum]
val line_cap_to_js : line_cap -> Ojs.t
val line_cap_of_js : Ojs.t -> line_cap

type line_join =
  [ `Round [@js "round"]
  | `Bevel [@js "bevel"]
  | `Miter [@js "miter"]
  ] [@js.enum]
val line_join_to_js : line_join -> Ojs.t
val line_join_of_js : Ojs.t -> line_join

type line_height = float
val line_height_to_js : line_height -> Ojs.t
val line_height_of_js : Ojs.t -> line_height

type interaction_mode =
  [ `Point [@js "point"]
  | `Nearest [@js "nearest"]
  | `Index [@js "index"]
  | `Dataset [@js "dataset"]
  | `X [@js "x"]
  | `Y [@js "y"]
  ] [@js.enum]
val interaction_mode_to_js : interaction_mode -> Ojs.t
val interaction_mode_of_js : Ojs.t -> interaction_mode


[@@@js.stop]
module type Jsable = sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
[@@@js.start]
[@@@js.implem
module type Jsable = sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
]
module Int : sig
  type t = int
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module Float : sig
  type t = float
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module String : sig
  type t = string
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module Int32 : sig
  [@@@js.stop]
  type t = int32
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  [@@@js.start]
  [@@@js.implem
   type t = int32
   let t_to_js i = Float.t_to_js @@ Int32.to_float i
   let t_of_js j = Int32.of_float @@ Float.t_of_js j
  ]
end
module Int64 : sig
  [@@@js.stop]
  type t = int64
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  [@@@js.start]
  [@@@js.implem
   type t = int64
   let t_to_js i = Float.t_to_js @@ Int64.to_float i
   let t_of_js j = Int64.of_float @@ Float.t_of_js j
  ]
end
module Bool : sig
  type t = bool
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module Color : sig
  type t = string
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module Font_family : sig
  type t = string
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module Font_style : sig
  type t = string
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module Time : sig
  [@@@js.stop]
  type t = Ptime.t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  [@@@js.start]
  [@@@js.implem
   type t = Ptime.t
   let t_to_js x = Ojs.float_to_js @@ Ptime.to_float_s x *. 1000.
   let t_of_js x =
     let s = Ojs.float_of_js x /. 1000. in
     match Ptime.of_float_s s with
     | None -> failwith "bad time value"
     | Some x -> x
  ]
end
module Padding : sig
  type t =
    [ `Num of int
    | `Obj of obj
    ] [@js.union]
  and obj =
    { top : int option
    ; right : int option
    ; bottom : int option
    ; left : int option
    }

  val make : ?top:int ->
             ?right:int ->
             ?bottom:int ->
             ?left:int ->
             unit ->
             t
    [@@js.custom
     let make ?top ?right ?bottom ?left () : t =
       `Obj { top; right; bottom; left }
    ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
    [@@js.custom
     let t_of_js (js : Ojs.t) : t =
       match Ojs.obj_type js with
       | "[object Number]" -> `Num (Ojs.int_of_js js)
       | "[object Object]" ->
          let (x : obj) =
            { left = Ojs.(option_of_js int_of_js @@ get js "left")
            ; right = Ojs.(option_of_js int_of_js @@ get js "right")
            ; top = Ojs.(option_of_js int_of_js @@ get js "top")
            ; bottom = Ojs.(option_of_js int_of_js @@ get js "bottom")
            } in
          `Obj x
       | _ -> assert false
    ]
end
