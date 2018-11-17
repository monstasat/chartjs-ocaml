module Axes : sig
  include module type of Axes
end
module Options : sig
  include module type of Chartjs_options
end
module Pie : sig
  include module type of Chart.Pie
end

type canvas = Dom_html.canvasElement Js.t
type context = Dom_html.canvasRenderingContext2D Js.t
type node =
  [ `Canvas of canvas
  | `Context of context
  | `Id of string ]
type api_config = Chart.api_config
type config = Chart.config
type t = Chart.t

val make_config : ?data:Ojs.t -> ?options:Options.t -> ?type_:string -> unit -> config
val make_api_config :
  ?duration:int -> ?lazy_:bool -> ?easing:Chartjs_types.easing -> unit -> api_config

val id : t -> int
val inner_radius : t -> int
val height : t -> int
val width : t -> int
val offset_x : t -> int
val offset_y : t -> int
val border_width : t -> int
val animating : t -> bool
val aspect_ratio : t -> float
val canvas : t -> canvas
val ctx : t -> context
val options : t -> Options.t
val set_options : t -> Options.t -> unit
val destroy : t -> unit
val update : t -> api_config option -> unit
val reset : t -> unit
val render : t -> api_config option -> unit
val stop : t -> t
val resize : t -> t
val clear : t -> t
val to_base64_image : t -> string
val generate_legend : t -> string
val new_chart : node -> config -> t
