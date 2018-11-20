open Chartjs_types

module Options : sig
  type t = Chartjs_options.t

  module Scales : sig

    module Grid_lines : sig
      type t = Chartjs_scales.Grid_lines.t

      (** If true, the bars for a particular data point fall between
        the grid lines. The grid line will move to the left by one
        half of the tick interval. If false, the grid line will go
        right down the middle of the bars. *)
      val offset_grid_lines : t -> bool
      val set_offset_grid_lines : t -> bool -> unit

    end

    module Cartesian : sig
      type t = Chartjs_scales.Cartesian.t

      (** Stacked bar charts can be used to show how one data series
          is made up of a number of smaller pieces. *)
      val stacked : t -> bool
      val set_stacked : t -> t -> bool -> unit

    end

  end

  (** Percent (0-1) of the available width each bar should be within
      the category width. 1.0 will take the whole category width and
      put the bars right next to each other. *)
  val bar_percentage : t -> float
  val set_bar_percentage : t -> float -> unit

  (** Percent (0-1) of the available width each category should be within
      the sample width. *)
  val category_percentage : t -> float
  val set_category_percentage : t -> float -> unit

  (** Manually set width of each bar in pixels.
      If set to 'flex', it computes "optimal" sample widths that globally
      arrange bars side by side. If not set (default), bars are equally
      sized based on the smallest interval. *)
  val bar_thickness : t -> float
  val set_bar_thickness : t -> float -> unit

  (** Set this to ensure that bars are not sized thicker than this. *)
  val max_bar_thickness : t -> float
  val set_max_bar_thickness : t -> float -> unit

end

module Dataset : sig
  type border_skipped =
    [ `Bottom [@js "bottom"]
    | `Left [@js "left"]
    | `Top [@js "top"]
    | `Right [@js "right"]
    ] [@js.enum]
  type t

  (** The label for the dataset which appears in the legend and tooltips. *)
  val label : t -> string
  val set_label : t -> string -> unit

  (** The ID of the x axis to plot this dataset on.
      If not specified, this defaults to the ID of the first found x axis. *)
  val x_axis_id : t -> string
  val set_x_axis_id : t -> string -> unit

  (** The ID of the y axis to plot this dataset on.
      If not specified, this defaults to the ID of the first found y axis. *)
  val y_axis_id : t -> string
  val set_y_axis_id : t -> string -> unit

  (** The ID of the group to which this dataset belongs to
      (when stacked, each group will be a separate stack). *)
  val stack : t -> string
  val set_stack : t -> string -> unit

  (** The fill color of the bar. *)
  val background_color : t -> Color.t
  val set_background_color : t -> Color.t -> unit

  (** The color of the bar border. *)
  val border_color : t -> Color.t
  val set_border_color : t -> Color.t -> unit

  (** The stroke width of the bar in pixels. *)
  val border_width : t -> int
  val set_border_width : t -> int -> unit

  (** Which edge to skip drawing the border for. *)
  val border_skipped : t -> border_skipped
  val set_border_skipped : t -> border_skipped -> unit

  (** The fill colour of the bars when hovered. *)
  val hover_background_color : t -> Color.t
  val set_hover_background_color : t -> Color.t -> unit

  (** The stroke colour of the bars when hovered. *)
  val hover_border_color : t -> Color.t
  val set_hover_border_color : t -> Color.t -> unit

  (** The stroke width of the bars when hovered. *)
  val hover_border_width : t -> int
  val set_hover_border_width : t -> int -> unit

end

module Data : sig

end
