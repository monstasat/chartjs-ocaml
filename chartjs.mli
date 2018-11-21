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
  open Chartjs_types
  module Options : sig
    include module type of Line.Options
  end
  module Dataset : sig
    type ('a, 'b) point = { x : 'a; y : 'b; }

    include module type of Line.Dataset
    module type DS = sig
      type item

      include module type of Line.Dataset
      module A : Chartjs_array.Typed_array with type item := item

      val data : t -> A.t
      val set_data : t -> item list -> unit
      val make :
        ?data:item list ->
        ?type_:typ ->
        ?label:string ->
        ?x_axis_id:string ->
        ?y_axis_id:string ->
        ?background_color:Color.t ->
        ?border_color:Color.t ->
        ?border_width:int ->
        ?border_dash:border_dash ->
        ?border_dash_offset:border_dash_offset ->
        ?border_cap_style:line_cap ->
        ?border_join_style:line_join ->
        ?cubic_interpolation_mode:cubic_interpolation_mode ->
        ?fill:fill ->
        ?line_tension:float ->
        ?show_line:bool ->
        ?span_gaps:bool ->
        ?stepped_line:stepped_line ->
        ?point_background_color:Color.t indexable ->
        ?point_border_color:Color.t indexable ->
        ?point_border_width:int indexable ->
        ?point_radius:int indexable ->
        ?point_rotation:int indexable ->
        ?point_hit_radius:int indexable ->
        ?point_hover_background_color:Color.t indexable ->
        ?point_hover_border_color:Color.t indexable ->
        ?point_hover_border_width:int indexable ->
        ?point_hover_radius:int indexable ->
        unit ->
        t
    end
    module Make_point :
    functor (X : Jsable)(Y : Jsable) -> Jsable with type t = (X.t, Y.t) point
    module Make : functor (M : Jsable) -> DS with type item := M.t

    module Int : DS with type item := int
    module Int32 : DS with type item := int32
    module Int64 : DS with type item := int64
    module Float : DS with type item := float
    module String : DS with type item := string
    module Time : DS with type item := Ptime.t
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

    include module type of Bar.Dataset
    module type DS = sig
      type item
      include module type of Bar.Dataset
      module A : Chartjs_array.Typed_array with type item := item

      val data : t -> A.t
      val set_data : t -> item list -> unit
      val make :
        ?data:item list ->
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
    module Make : functor (M : Jsable) -> DS with type item := M.t

    module Int : DS with type item := int
    module Int32 : DS with type item := int32
    module Int64 : DS with type item := int64
    module Float : DS with type item := float
    module String : DS with type item := string
    module Time : DS with type item := Ptime.t
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
    include module type of Pie.Dataset

    module type DS = sig
      type item
      include module type of Pie.Dataset
      module A : Chartjs_array.Typed_array with type item := item

      val data : t -> A.t
      val set_data : t -> item list -> unit
      val make :
        ?data:item list ->
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
    module Make : functor (M : Jsable) -> DS with type item := M.t

    module Int : DS with type item := int
    module Int32 : DS with type item := int32
    module Int64 : DS with type item := int64
    module Float : DS with type item := float
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
