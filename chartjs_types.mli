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

module Time : sig
  [@@@js.stop]
  type t = Ptime.t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  [@@@js.start]
  [@@@js.implem
   type t = Ptime.t
   let t_to_js x = Ojs.float_to_js @@ Ptime.to_float_s x *. 1000.
   let t_of_js x = match Ptime.of_float_s @@ Ojs.float_of_js x with
     | None -> failwith "bad time value"
     | Some x -> x
  ]
end

module Color : sig
  type t = string
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

end

module Font : sig
  type family = string
  val family_to_js : family -> Ojs.t
  val family_of_js : Ojs.t -> family

  type style = string
  val style_to_js : style -> Ojs.t
  val style_of_js : Ojs.t -> style

end

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

[@@@js.stop]
type 'a indexable =
  [ `Single of 'a
  | `List of 'a list
  ]
val indexable_to_js : ('a -> Ojs.t) -> 'a indexable -> Ojs.t
val indexable_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a indexable
[@@@js.start]
[@@@js.implem
 type 'a indexable =
   [ `Single of 'a
   | `List of 'a list
   ]
 let indexable_to_js (f : 'a -> Ojs.t) = function
   | `Single x -> f x
   | `List x -> Ojs.list_to_js f x
 let indexable_of_js (f : Ojs.t -> 'a) (js : Ojs.t) =
   match Ojs.obj_type js with
   | "[object Array]" -> `List (Ojs.list_of_js f js)
   | _ -> `Single (f js)
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

module Update_config : sig
  type t

  val duration : t -> int
  val set_duration : t -> int -> unit

  val lazy_ : t -> bool
  val set_lazy_ : t -> bool -> unit

  val easing : t -> easing
  val set_easing : t -> easing -> unit

  val make : ?duration:int ->
             ?lazy_:bool ->
             ?easing:easing ->
             unit ->
             t [@@js.builder]

end

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

type text = string [@@deriving show]
val text_to_js : text -> Ojs.t
  [@@js.custom
   let text_to_js (s : text) : Ojs.t =
     match String.split_on_char '\n' s with
     | [s] -> Ojs.string_to_js s
     | l -> Ojs.list_to_js Ojs.string_to_js l
  ]
val text_of_js : Ojs.t -> text
  [@@js.custom
   let text_of_js (js : Ojs.t) : text =
     match Ojs.obj_type js with
     | "[object Array]" ->
        let l = Ojs.list_of_js Ojs.string_of_js js in
        String.concat "\n" l
     | "[object String]" ->
        Ojs.string_of_js js
     | _ -> assert false
  ]

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
