open Chartjs.Option_types

module Option_context : sig
  type t = Option_context.t

  val active : t -> bool
end

module Font : sig
  open Chartjs.Types

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

type listeners

type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

module Align : sig

  type t =
    [ `Start [@js "start"]
    | `End [@js "end"]
    | `Center [@js "center"]
    | `Right [@js "right"]
    | `Bottom [@js "bottom"]
    | `Left [@js "left"]
    | `Top [@js "top"]
    | `Angle of int [@js.default]
    ] [@js.enum]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

end

module Anchor : sig

  type t =
    [ `Center [@js "center"]
    | `Start [@js "start"]
    | `End [@js "end"]
    ] [@js.enum]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

end

module Text_align : sig

  type t =
    [ `Start [@js "start"]
    | `Center [@js "center"]
    | `End [@js "end"]
    | `Left [@js "left"]
    | `Right [@js "right"]
    ] [@js.enum]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

end

(* TODO add 'listeners' option *)

(* TODO think what to do with the first parameter (value) type *)
type formatter = Ojs.t -> Option_context.t -> string

val align : t -> Align.t scriptable
val set_align : t -> Align.t scriptable -> unit

val anchor : t -> Anchor.t scriptable
val set_anchor : t -> Anchor.t scriptable -> unit

val background_color : t -> Chartjs.Types.Color.t scriptable
val set_background_color : t -> Chartjs.Types.Color.t scriptable -> unit

val border_color : t -> Chartjs.Types.Color.t scriptable
val set_border_color : t -> Chartjs.Types.Color.t scriptable -> unit

val border_radius : t -> int scriptable
val set_border_radius : t -> int scriptable -> unit

val border_width : t -> float scriptable
val set_border_width : t -> float scriptable -> unit

val clip : t -> bool scriptable
val set_clip : t -> bool scriptable -> unit

val color : t -> Chartjs.Types.Color.t scriptable
val set_color : t -> Chartjs.Types.Color.t scriptable -> unit

val display : t -> bool scriptable
val set_display : t -> bool scriptable -> unit

val font : t -> Font.t scriptable
val set_font : t -> Font.t scriptable -> unit

val offset : t -> int scriptable
val set_offset : t -> int scriptable -> unit

val opacity : t -> float scriptable
val set_opacity : t -> float scriptable -> unit

val padding : t -> Chartjs.Types.Padding.t scriptable
val set_padding : t -> Chartjs.Types.Padding.t scriptable -> unit

val rotation : t -> int scriptable
val set_rotation : t -> int scriptable -> unit

val text_align : t -> Text_align.t scriptable
val set_text_align : t -> Text_align.t scriptable -> unit

val formatter : t -> formatter
val set_formatter : t -> formatter -> unit

val make : ?align:Align.t scriptable ->
           ?anchor:Anchor.t scriptable ->
           ?background_color:Chartjs.Types.Color.t scriptable ->
           ?border_color:Chartjs.Types.Color.t scriptable ->
           ?border_radius:int scriptable ->
           ?border_width:float scriptable ->
           ?clip:bool scriptable ->
           ?color:Chartjs.Types.Color.t scriptable ->
           ?display:bool scriptable ->
           ?font:Font.t scriptable ->
           ?offset:int scriptable ->
           ?opacity:float scriptable ->
           ?padding:Chartjs.Types.Padding.t scriptable ->
           ?rotation:int scriptable ->
           ?text_align:Text_align.t scriptable ->
           ?formatter:formatter ->
           unit ->
           t [@@js.builder]

module Per_dataset : sig
  val get : Chartjs.Data.Dataset.t -> t [@@js.get "datalabels"]
  val set : Chartjs.Data.Dataset.t -> t -> unit [@@js.set "datalabels"]
end

module Per_chart : sig
  val get : Chartjs.Options.Plugins.t -> t [@@js.get "datalabels"]
  val set : Chartjs.Options.Plugins.t ->
            t Chartjs.Types.or_false ->
            unit [@@js.set "datalabels"]
end
