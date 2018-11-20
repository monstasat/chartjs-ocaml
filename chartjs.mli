module type Jsable = sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end
module Array = Chartjs_array
module Data_types : sig
  module Int : Jsable with type t = int
  module Float : Jsable with type t = float
  module Int32 : Jsable with type t = int32
  module Int64 : Jsable with type t = int64
  module String : Jsable with type t = string
  module Time : Jsable with type t = Ptime.t
end
module Data : sig
  include module type of Chart.Data
end
module Scales : sig
  include module type of Axes
end
module Options : sig
  include module type of Chartjs_options
end
(** A line chart is a way of plotting data points on a line.
    Often, it is used to show trend data, or the comparison
    of two data sets. *)
module Line : sig
  module Options : sig
    include module type of Line.Options
  end
  module Dataset : sig
    type ('a, 'b) point = { x : 'a; y : 'b; }
    module type DS = sig
      type dot
      type data = dot list
      include module type of Line.Dataset
      val make :
        ?data:data ->
        ?type_:Chartjs_types.typ ->
        ?label:string ->
        ?x_axis_id:string ->
        ?y_axis_id:string ->
        ?background_color:Chartjs_types.Color.t ->
        ?border_color:Chartjs_types.Color.t ->
        ?border_width:int ->
        ?border_dash:Chartjs_types.border_dash ->
        ?border_dash_offset:Chartjs_types.border_dash_offset ->
        ?border_cap_style:Chartjs_types.line_cap ->
        ?border_join_style:Chartjs_types.line_join ->
        ?cubic_interpolation_mode:cubic_interpolation_mode ->
        ?fill:fill ->
        ?line_tension:float ->
        ?show_line:bool ->
        ?span_gaps:bool ->
        ?stepped_line:stepped_line ->
        ?point_background_color:Chartjs_types.Color.t
          Chartjs_types.indexable ->
        ?point_border_color:Chartjs_types.Color.t Chartjs_types.indexable ->
        ?point_border_width:int Chartjs_types.indexable ->
        ?point_radius:int Chartjs_types.indexable ->
        ?point_rotation:int Chartjs_types.indexable ->
        ?point_hit_radius:int Chartjs_types.indexable ->
        ?point_hover_background_color:Chartjs_types.Color.t
          Chartjs_types.indexable ->
        ?point_hover_border_color:Chartjs_types.Color.t
          Chartjs_types.indexable ->
        ?point_hover_border_width:int Chartjs_types.indexable ->
        ?point_hover_radius:int Chartjs_types.indexable -> unit -> t
    end
    module Make_point :
    functor (X : Jsable)(Y : Jsable) -> Jsable with type t = (X.t, Y.t) point

    module Make : functor (M : Jsable) -> DS with type dot := M.t
    module Raw = Line.Dataset
    module Int : DS with type dot := int
    module Int32 : DS with type dot := int32
    module Int64 : DS with type dot := int64
    module Float : DS with type dot := float
    module String : DS with type dot := string
    module Time : DS with type dot := Ptime.t
  end
end
(** A bar chart provides a way of showing data values represented
    as vertical bars. It is sometimes used to show trend data, and
    the comparison of multiple data sets side by side. *)
module Bar : sig
  open Chartjs_types
  module Options : sig
    include module type of Bar.Options
  end
  module Dataset : sig
    type ('a, 'b) point = { x : 'a; y : 'b; }
    module type DS = sig
      type dot
      type data = dot list
      include module type of Bar.Dataset
      val make :
        ?data:data ->
        ?type_:typ ->
        ?label:string ->
        ?x_axis_id:string ->
        ?y_axis_id:string ->
        ?stack:string ->
        ?border_skipped:border_skipped ->
        ?background_color:Color.t indexable ->
        ?border_color:Color.t indexable ->
        ?border_width:int indexable ->
        ?hover_background_color:Color.t indexable ->
        ?hover_border_color:Color.t indexable ->
        ?hover_border_width:int indexable ->
        unit ->
        t
    end
    module Make_point :
    functor (X : Jsable)(Y : Jsable) -> Jsable with type t = (X.t, Y.t) point
    module Make : functor (M : Jsable) -> DS with type dot := M.t
    module Raw = Bar.Dataset
    module Int : DS with type dot := int
    module Int32 : DS with type dot := int32
    module Int64 : DS with type dot := int64
    module Float : DS with type dot := float
    module String : DS with type dot := string
    module Time : DS with type dot := Ptime.t
  end
end
(** A radar chart is a way of showing multiple data points and the
    variation between them. They are often useful for comparing the
    points of two or more different data sets. *)
module Radar : sig
  (* TODO implement *)
end
(** Pie and doughnut charts are probably the most commonly used charts.
    They are divided into segments, the arc of each segment shows
    the proportional value of each piece of data. They are excellent at
    showing the relational proportions between data. *)
module Pie : sig
  open Chartjs_types
  module Options : sig
    include module type of Pie.Options
  end
  module Dataset : sig
    module type DS = sig
      type dot
      include module type of Pie.Dataset
      module Array : sig
        include Chartjs_array.Typed_array with type item := dot
      end

      val data : t -> Array.t
      val set_data : t -> dot list -> unit
      val make :
        ?data:dot list ->
        ?type_:typ ->
        ?background_color:Color.t list ->
        ?border_color:Color.t list ->
        ?border_width:int list ->
        ?hover_background_color:Color.t list ->
        ?hover_border_color:Color.t list ->
        ?hover_border_width:int list ->
        unit ->
        t
    end
    module Make : functor (M : Jsable) -> DS with type dot := M.t
    module Raw = Pie.Dataset
    module Int : DS with type dot := int
    module Int32 : DS with type dot := int32
    module Int64 : DS with type dot := int64
    module Float : DS with type dot := float
  end
end
(** Polar area charts are similar to pie charts, but each segment has the
    same angle - the radius of the segment differs depending on the value.
    This type of chart is often useful when we want to show a comparison
    data similar to a pie chart, but also show a scale of values for context. *)
module Polar_area : sig
  (* TODO implement *)
end
(** A bubble chart is used to display three dimensions of data at the same time.
    The location of the bubble is determined by the first two dimensions and the
    corresponding horizontal and vertical axes. The third dimension is
    represented by the size of the individual bubbles. *)
module Bubble : sig
  (* TODO implement *)
end
(** Scatter charts are based on basic line charts with the x axis changed to a
    linear axis. To use a scatter chart, data must be passed as objects
    containing X and Y properties. *)
module Scatter : sig
  (* TODO implement *)
end

type canvas = Dom_html.canvasElement Js.t
type context = Dom_html.canvasRenderingContext2D Js.t
type node =
  [ `Canvas of canvas
  | `Context of context
  | `Id of string
  ]
type config
type t = Chart.t

val ( .%[] ) : 'a Chartjs_array.t -> int -> 'a
val ( .%[]<- ) : 'a Chartjs_array.t -> int -> 'a -> unit
val make_config :
  ?duration:int ->
  ?lazy_:bool ->
  ?easing:Chartjs_types.easing ->
  unit ->
  config

val make :
  ?options:Options.t ->
  ?data:Data.t ->
  Chartjs_types.typ ->
  node ->
  t

(* Getters *)
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

(* Getters, setters *)
val data : t -> Data.t
val set_data : t -> Data.t -> unit

val options : t -> Chartjs_options.t
val set_options : t -> Chartjs_options.t -> unit

(* API *)
val destroy : t -> unit
val update : t -> config option -> unit
val reset : t -> unit
val render : t -> config option -> unit
val stop : t -> t
val resize : t -> t
val clear : t -> t
val to_base64_image : t -> string
val generate_legend : t -> string
