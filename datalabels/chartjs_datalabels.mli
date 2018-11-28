open Chartjs.Types
open Chartjs.Option_types

module Option_context : sig
  type t = Option_context.t

  val active : t -> bool
end

module Font : sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val family : t -> Font_family.t
  val size : t -> int
  val style : t -> Font_style.t
  val weight : t -> string
  val line_height : t -> line_height

  val make : ?family:Font_family.t ->
             ?size:int ->
             ?style:Font_style.t ->
             ?weight:string ->
             ?line_height:line_height ->
             unit ->
             t [@@js.builder]
end

type padding =
  [ `Num of int
  | `Obj of padding_obj
  ] [@js.union]
and padding_obj =
  { top : int option
  ; right : int option
  ; bottom : int option
  ; left : int option
  }
val padding_of_js : Ojs.t -> padding
  [@@js.custom
   let padding_of_js (js : Ojs.t) : padding =
     match Ojs.obj_type js with
     | "[object Number]" -> `Num (Ojs.int_of_js js)
     | "[object Object]" ->
        let (x : padding_obj) =
          { left = Ojs.(option_of_js int_of_js @@ get js "left")
          ; right = Ojs.(option_of_js int_of_js @@ get js "right")
          ; top = Ojs.(option_of_js int_of_js @@ get js "top")
          ; bottom = Ojs.(option_of_js int_of_js @@ get js "bottom")
          } in
        `Obj x
     | _ -> assert false
  ]

type listeners

type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

type align =
  [ `Start [@js "start"]
  | `End [@js "end"]
  | `Center [@js "center"]
  | `Right [@js "right"]
  | `Bottom [@js "bottom"]
  | `Left [@js "left"]
  | `Top [@js "top"]
  | `Angle of int [@js.default]
  ] [@js.enum]

type anchor =
  [ `Center [@js "center"]
  | `Start [@js "start"]
  | `End [@js "end"]
  ] [@js.enum]

type text_align =
  [ `Start [@js "start"]
  | `Center [@js "center"]
  | `End [@js "end"]
  | `Left [@js "left"]
  | `Right [@js "right"]
  ] [@js.enum]

(* TODO add 'listeners' option *)

(* TODO think what to do with the first parameter (value) type *)
type formatter = Ojs.t -> Option_context.t -> string

val align : t -> align scriptable
val set_align : t -> align scriptable -> unit

val anchor : t -> anchor scriptable
val set_anchor : t -> anchor scriptable -> unit

val background_color : t -> Color.t scriptable
val set_background_color : t -> Color.t scriptable -> unit

val border_color : t -> Color.t scriptable
val set_border_color : t -> Color.t scriptable -> unit

val border_radius : t -> int scriptable
val set_border_radius : t -> int scriptable -> unit

val border_width : t -> float scriptable
val set_border_width : t -> float scriptable -> unit

val clip : t -> bool scriptable
val set_clip : t -> bool scriptable -> unit

val color : t -> Color.t scriptable
val set_color : t -> Color.t scriptable -> unit

val display : t -> bool scriptable
val set_display : t -> bool scriptable -> unit

val font : t -> Font.t scriptable
val set_font : t -> Font.t scriptable -> unit

val offset : t -> int scriptable
val set_offset : t -> int scriptable -> unit

val opacity : t -> float scriptable
val set_opacity : t -> float scriptable -> unit

val padding : t -> padding scriptable
val set_padding : t -> padding scriptable -> unit

val rotation : t -> int scriptable
val set_rotation : t -> int scriptable -> unit

val text_align : t -> text_align scriptable
val set_text_align : t -> text_align scriptable -> unit

val formatter : t -> formatter
val set_formatter : t -> formatter -> unit

val make : ?align:align scriptable ->
           ?anchor:anchor scriptable ->
           ?background_color:Color.t scriptable ->
           ?border_color:Color.t scriptable ->
           ?border_radius:int scriptable ->
           ?border_width:float scriptable ->
           ?clip:bool scriptable ->
           ?color:Color.t scriptable ->
           ?display:bool scriptable ->
           ?font:Font.t scriptable ->
           ?offset:int scriptable ->
           ?opacity:float scriptable ->
           ?padding:padding scriptable ->
           ?rotation:int scriptable ->
           ?text_align:text_align scriptable ->
           ?formatter:formatter ->
           unit ->
           t [@@js.builder]

module Per_dataset : sig
  val datalabels : Chartjs.Data.Dataset.t -> t
  val set_datalabels : Chartjs.Data.Dataset.t -> t -> unit
end

module Per_chart : sig
  val datalabels : Chartjs.Options.Plugins.t -> t
  val set_datalabels : Chartjs.Options.Plugins.t -> t or_false -> unit
end
