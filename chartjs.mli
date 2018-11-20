module Array = Chartjs_array
module Scales : sig
  include module type of Axes
end
module Data : sig
  include module type of Chart.Data
end
module Options : sig
  include module type of Chartjs_options
end
module Doughnut : sig
  include module type of Pie
end
module Pie : sig
  include module type of Pie
end
val ( .%[] ) : 'a Chartjs_array.t -> int -> 'a
val ( .%[]<- ) : 'a Chartjs_array.t -> int -> 'a -> unit

type node =
  [ `Canvas of Dom_html.canvasElement Js.t
  | `Context of Dom_html.canvasRenderingContext2D Js.t
  | `Id of string
  ]
type t = Chart.t

val make : ?options:Options.t ->
           ?data:Data.t ->
           Chartjs_types.typ ->
           node ->
           t

type config = Chart.API.config
val make_config :
  ?duration:int ->
  ?lazy_:bool -> ?easing:Chartjs_types.easing -> unit -> config

val id : t -> int
val inner_radius : t -> int
val height : t -> int
val width : t -> int
val offset_x : t -> int
val offset_y : t -> int
val border_width : t -> int
val animating : t -> bool
val aspect_ratio : t -> float
val canvas : t -> Dom_html.canvasElement Js.t
val ctx : t -> Dom_html.canvasRenderingContext2D Js.t
val data : t -> Data.t
val set_data : t -> Data.t -> unit
val options : t -> Options.t
val set_options : t -> Options.t -> unit
val destroy : t -> unit
val update : t -> config option -> unit
val reset : t -> unit
val render : t -> config option -> unit
val stop : t -> t
val resize : t -> t
val clear : t -> t
val to_base64_image : t -> string
val generate_legend : t -> string
