open Chartjs_types

type scale
val scale_to_js : scale -> Ojs.t
val scale_of_js : Ojs.t -> scale

module Scale_label : sig
  type padding =
    [ `Obj of padding_obj
    | `Int of int
    ] [@js.union]
  and padding_obj =
    { top : int option
    ; bottom : int option
    }
  val padding_of_js : Ojs.t -> padding
    [@@js.custom
     let padding_of_js (js : Ojs.t) : padding =
       match Ojs.obj_type js with
       | "[object Number]" -> `Int (Ojs.int_of_js js)
       | "[object Object]" ->
          let x =
            { top = Ojs.(option_of_js int_of_js @@ get js "top")
            ; bottom = Ojs.(option_of_js int_of_js @@ get js "bottom")
            } in
          `Obj x
       | _ -> assert false
    ]
  type t

  (** If true, display the axis title. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** The text for the title. (i.e. "# of People" or "Response Choices"). *)
  val label_string : t -> string
  val set_label_string : t -> string -> unit

  (** Height of an individual line of text. *)
  val line_height : t -> line_height
  val set_line_height : t -> line_height -> unit

  (** Font color for scale title. *)
  val font_color : t -> Color.t
  val set_font_color : t -> Color.t -> unit

  (** Font family for the scale title, follows CSS font-family options. *)
  val font_family : t -> Font.family
  val set_font_family : t -> Font.family -> unit

  (** Font size for scale title. *)
  val font_size : t -> int
  val set_font_size : t -> int -> unit

  (** Font style for the scale title, follows CSS font-style options
      (i.e. normal, italic, oblique, initial, inherit) *)
  val font_style : t -> Font.style
  val set_font_style : t -> Font.style -> unit

  (** Padding to apply around scale labels.
      Only top and bottom are implemented. *)
  val padding : t -> padding
  val set_padding : t -> padding -> unit

  val make : ?display:bool ->
             ?label_string:string ->
             ?line_height:line_height ->
             ?font_color:Color.t ->
             ?font_family:Font.family ->
             ?font_size:int ->
             ?font_style:Font.style ->
             ?padding:padding ->
             unit ->
             t [@@js.builder]

end

module Grid_lines : sig
  type color =
    [ `Single of Color.t
    | `List of Color.t list
    ] [@js.union]
  val color_of_js : Ojs.t -> color
    [@@js.custom
     let color_of_js (js : Ojs.t) : color =
       match Ojs.obj_type js with
       | "[object Array]" ->
          `List (Ojs.list_of_js Color.t_of_js js)
       | _ -> `Single (Color.t_of_js js)
    ]
  type line_width =
    [ `Single of int
    | `List of int list
    ] [@js.union]
  val line_width_of_js : Ojs.t -> line_width
    [@@js.custom
     let line_width_of_js (js : Ojs.t) : line_width =
       match Ojs.obj_type js with
       | "[object Array]" ->
          `List (Ojs.list_of_js Ojs.int_of_js js)
       | "[object Number]" ->
          `Single (Ojs.int_of_js js)
       | _ -> assert false
    ]
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  (** If false, do not display grid lines for this axis. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** If true, gridlines are circular (on radar chart only). *)
  val circular : t -> bool
  val set_circular : t -> bool -> unit

  (** The color of the grid lines. If specified as an array,
      the first color applies to the first grid line, the second
      to the second grid line and so on. *)
  val color : t -> color
  val set_color : t -> color -> unit

  (** Length and spacing of dashes on grid lines. *)
  val border_dash : t -> border_dash
  val set_border_dash : t -> border_dash -> unit

  (** Offset for line dashes. *)
  val border_dash_offset : t -> border_dash_offset
  val set_border_dash_offset : t -> border_dash_offset -> unit

  (** Stroke width of grid lines. *)
  val line_width : t -> line_width
  val set_line_width : t -> line_width -> unit

  (** If true, draw border at the edge between the axis and the chart area. *)
  val draw_border : t -> bool
  val set_draw_border : t -> bool -> unit

  (** If true, draw lines on the chart area inside the axis lines.
      This is useful when there are multiple axes and you need to
      control which grid lines are drawn. *)
  val draw_on_chart_area : t -> bool
  val set_draw_on_chart_area : t -> bool -> unit

  (** If true, draw lines beside the ticks in the axis area beside the chart. *)
  val draw_ticks : t -> bool
  val set_draw_ticks : t -> bool -> unit

  (** Length in pixels that the grid lines will draw into the axis area. *)
  val tick_mark_length : t -> int
  val set_tick_mark_length : t -> int -> unit

  (** Stroke width of the grid line for the first index (index 0). *)
  val zero_line_width : t -> int
  val set_zero_line_width : t -> int -> unit

  (** Stroke color of the grid line for the first index (index 0). *)
  val zero_line_color : t -> Color.t
  val set_zero_line_color : t -> Color.t -> unit

  (** Length and spacing of dashes of the grid line
      for the first index (index 0). *)
  val zero_line_border_dash : t -> border_dash
  val set_zero_line_border_dash : t -> border_dash -> unit

  (** Offset for line dashes of the grid line for the first index (index 0). *)
  val zero_line_border_dash_offset : t -> border_dash_offset
  val set_zero_line_border_dash_offset : t -> border_dash_offset -> unit

  (** If true, grid lines will be shifted to be between labels.
      This is set to true for a category scale in a bar chart by default. *)
  val offset_grid_lines : t -> bool
  val set_offset_grid_lines : t -> bool -> unit

  val make : ?display:bool ->
             ?circular:bool ->
             ?color:color ->
             ?border_dash:border_dash ->
             ?border_dash_offset:border_dash_offset ->
             ?line_width:line_width ->
             ?draw_border:bool ->
             ?draw_on_chart_area:bool ->
             ?draw_ticks:bool ->
             ?tick_mark_length:int ->
             ?zero_line_width:int ->
             ?zero_line_color:Color.t ->
             ?zero_line_border_dash:border_dash ->
             ?zero_line_border_dash_offset:border_dash_offset ->
             ?offset_grid_lines:bool ->
             unit ->
             t [@@js.builder]

end

(** The tick configuration is nested under the scale configuration
    in the ticks key. It defines options for the tick marks that are
    generated by the axis.*)
module Ticks : sig

  type callback = value:Ojs.t -> index:int -> values:Ojs.t -> string option

  (** The minorTick configuration is nested under the ticks configuration
      in the minor key. It defines options for the minor tick marks that are
      generated by the axis. Omitted options are inherited from ticks
      configuration. *)
  module Minor_major : sig
    type t

    (** Returns the string representation of the tick value
        as it should be displayed on the chart. *)
    val callback : t -> callback
    val set_callback : t -> callback -> unit

    (** Font color for tick labels. *)
    val font_color : t -> Color.t
    val set_font_color : t -> Color.t -> unit

    (** Font family for the tick labels, follows CSS font-family options. *)
    val font_family : t -> Font.family
    val set_font_family : t -> Font.family -> unit

    (** Font size for the tick labels. *)
    val font_size : t -> int
    val set_font_size : t -> int -> unit

    (** Font style for the tick labels, follows CSS font-style options
        (i.e. normal, italic, oblique, initial, inherit). *)
    val font_style : t -> Font.style
    val set_font_style : t -> Font.style -> unit

  end

  type t

  (** Returns the string representation of the tick value as
      it should be displayed on the chart. *)
  val callback : t -> callback
  val set_callback : t -> callback -> unit

  (** If true, automatically calculates how many labels that
      can be shown and hides labels accordingly. Turn it off to show all
      labels no matter what. *)
  val auto_skip : t -> bool
  val set_auto_skip : t -> bool -> unit

  (** Padding between the ticks on the horizontal axis when autoSkip is
      enabled. Note: Only applicable to horizontal scales. *)
  val auto_skip_padding : t -> int
  val set_auto_skip_padding : t -> int -> unit

  (** Distance in pixels to offset the label from the centre point of the
      tick (in the y direction for the x axis, and the x direction for the
      y axis). Note: this can cause labels at the edges to be cropped by the
      edge of the canvas. *)
  val label_offset : t -> int
  val set_label_offset : t -> int -> unit

  (** Maximum rotation for tick labels when rotating to condense labels.
      Note: Rotation doesn't occur until necessary. Note: Only applicable
      to horizontal scales. *)
  val max_rotation : t -> int
  val set_max_rotation : t -> int -> unit

  (** Minimum rotation for tick labels.
      Note: Only applicable to horizontal scales. *)
  val min_rotation : t -> int
  val set_min_rotation : t -> int -> unit

  (** Flips tick labels around axis, displaying the labels inside the chart
      instead of outside. Note: Only applicable to vertical scales. *)
  val mirror : t -> bool
  val set_mirror : t -> bool -> unit

  (** Padding between the tick label and the axis. When set on a vertical axis,
      this applies in the horizontal (X) direction. When set on a horizontal
      axis, this applies in the vertical (Y) direction. *)
  val padding : t -> int
  val set_padding : t -> int -> unit

  val minor : t -> Minor_major.t
  val set_minor : t -> Minor_major.t -> unit

  val major : t -> Minor_major.t
  val set_major : t -> Minor_major.t -> unit

  val make : ?callback:callback ->
             ?display:bool ->
             ?font_color:Color.t ->
             ?font_family:Font.family ->
             ?font_size:int ->
             ?font_style:Font.style ->
             ?reverse:bool ->
             ?minor:Minor_major.t ->
             ?major:Minor_major.t ->
             unit ->
             t

end

module Cartesian : sig
  type position
  type t = scale

  module Common : sig

    (** Type of scale being employed.
      Custom scales can be created and registered with a string key.
      This allows changing the type of an axis for a chart. *)
    val type_ : t -> string
    val set_type_ : t -> string -> unit

    (** Position of the axis in the chart.
      Possible values are: 'top', 'left', 'bottom', 'right' *)
    val position : t -> position
    val set_position : t -> position -> unit

    (** If true, extra space is added to the both edges and the axis
      is scaled to fit into the chart area. This is set to true for a
      category scale in a bar chart by default. *)
    val offset : t -> bool
    val set_offset : t -> bool -> unit

    (** The ID is used to link datasets and scale axes together. *)
    val id : t -> string
    val set_id : t -> string -> unit

    (** Grid line configuration. *)
    val grid_lines : t -> Grid_lines.t
    val set_grid_lines : t -> Grid_lines.t -> unit

    (** Scale title configuration. *)
    val scale_label : t -> Scale_label.t
    val set_scale_label : t -> Scale_label.t -> unit

    (** Tick configuration. *)
    val ticks : t -> Ticks.t
    val set_ticks : t -> Ticks.t -> unit

    val make :
      ?type_:string ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t ->
      unit ->
      scale [@@js.builder]

  end

  module Category : sig

    module Ticks : sig
      type t = Ticks.t

      (** An array of labels to display. *)
      val labels : t -> string list
      val set_labels : t -> string list -> unit

      (** The minimum item to display. *)
      val min : t -> string
      val set_min : t -> string -> unit

      (** The maximum item to display. *)
      val max : t -> string
      val set_max : t -> string -> unit

      val make :
        (* Common*)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font.family ->
        ?font_size:int ->
        ?font_style:Font.style ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Category-specific*)
        ?labels:string list ->
        ?min:string ->
        ?max:string ->
        unit ->
        t [@@js.builder]

    end

  end

  module Linear : sig

    module Ticks : sig
      type t = Ticks.t

      (** If true, scale will include 0 if it is not already included. *)
      val begin_at_zero : t -> bool
      val set_begin_at_zero : t -> bool -> unit

      (** User defined minimum number for the scale,
        overrides minimum value from data. *)
      val min : t -> float
      val set_min : t -> float -> unit

      (** User defined maximum number for the scale,
        overrides maximum value from data. *)
      val max : t -> float
      val set_max : t -> float -> unit

      (** Maximum number of ticks and gridlines to show. *)
      val max_ticks_limit : t -> int
      val set_max_ticks_limit : t -> int -> unit

      (** If defined and stepSize is not specified,
        the step size will be rounded to this many decimal places. *)
      val precision : t -> int
      val set_precision : t -> int -> unit

      (** User defined fixed step size for the scale. *)
      val step_size : t -> float
      val set_step_size : t -> float -> unit

      (** Adjustment used when calculating the maximum data value. *)
      val suggested_max : t -> float
      val set_suggested_max : t -> float -> unit

      (** Adjustment used when calculating the minimum data value. *)
      val suggested_min : t -> float
      val set_suggested_min : t -> float -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font.family ->
        ?font_size:int ->
        ?font_style:Font.style ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Linear-specific *)
        ?begin_at_zero:bool ->
        ?min:float ->
        ?max:float ->
        ?max_ticks_limit:int ->
        ?precision:int ->
        ?step_size:float ->
        ?suggested_max:float ->
        ?suggested_min:float ->
        unit ->
        t [@@js.builder]

    end

  end

  module Logarithmic : sig

    module Ticks : sig
      type t = Ticks.t

      (** User defined minimum number for the scale,
        overrides minimum value from data. *)
      val min : t -> float
      val set_min : t -> float -> unit

      (** User defined maximum number for the scale,
        overrides maximum value from data. *)
      val max : t -> float
      val set_max : t -> float -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font.family ->
        ?font_size:int ->
        ?font_style:Font.style ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Logarithmic-specific *)
        ?min:float ->
        ?max:float ->
        unit ->
        t [@@js.builder]

    end

  end

  module Time : sig

    module Ticks : sig
      type source =
        [ `Auto [@js "auto"]
        | `Data [@js "data"]
        | `Labels [@js "labels"]
        ] [@js.enum]
      type t = Ticks.t

      (** How ticks are generated. *)
      val source : t -> source
      val set_source : t -> source -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font.family ->
        ?font_size:int ->
        ?font_style:Font.style ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Time-specific *)
        ?source:source ->
        unit ->
        t [@@js.builder]

    end

    module Time : sig

      (** The following display formats are used to configure
        how different time units are formed into strings for
        the axis tick marks. *)
      module Display_formats : sig
        type t

        val millisecond : t -> string
        val set_millisecond : t -> string -> unit

        val second : t -> string
        val set_second : t -> string -> unit

        val minute : t -> string
        val set_minute : t -> string -> unit

        val hour : t -> string
        val set_hour : t -> string -> unit

        val day : t -> string
        val set_day : t -> string -> unit

        val week : t -> string
        val set_week : t -> string -> unit

        val month : t -> string
        val set_month : t -> string -> unit

        val quarter : t -> string
        val set_quarter : t -> string -> unit

        val year : t -> string
        val set_year : t -> string -> unit

        val make : ?millisecond:string ->
                   ?second:string ->
                   ?minute:string ->
                   ?hour:string ->
                   ?day:string ->
                   ?week:string ->
                   ?month:string ->
                   ?quarter:string ->
                   ?year:string ->
                   unit ->
                   t [@@js.builder]

      end

      (* TODO implement types *)
      type parser
      type time_unit =
        [ `Millisecond [@js "millisecond"]
        | `Second [@js "second"]
        | `Minute [@js "minute"]
        | `Hour [@js "hour"]
        | `Day [@js "day"]
        | `Week [@js "week"]
        | `Month [@js "month"]
        | `Quarter [@js "quarter"]
        | `Year [@js "year"]
        ] [@js.enum]
      type t

      (** Sets how different time units are displayed. *)
      val display_formats : t -> Display_formats.t
      val set_display_formats : t -> Display_formats.t -> unit

      (** If true and the unit is set to 'week', then the first day
        of the week will be Monday. Otherwise, it will be Sunday. *)
      val iso_weekday : t -> bool
      val set_iso_weekday : t -> bool -> unit

      (** If defined, this will override the data maximum *)
      val max : t -> Time.t
      val set_max : t -> Time.t -> unit

      (** If defined, this will override the data minimum *)
      val min : t -> Time.t
      val set_min : t -> Time.t -> unit

      (** Custom parser for dates. *)
      val parser : t -> parser
      val set_parser : t -> parser -> unit

      (** If defined, dates will be rounded to the start of this unit. *)
      val round : t -> time_unit or_false
      val set_round : t -> time_unit or_false -> unit

      (** The moment js format string to use for the tooltip. *)
      val tooltip_format : t -> string
      val set_tooltip_format : t -> string -> unit

      (** If defined, will force the unit to be a certain type. *)
      val unit : t -> time_unit
      val set_unit : t -> time_unit -> unit

      (** The number of units between grid lines. *)
      val step_size : t -> int
      val set_step_size : t -> int -> unit

      (** The minimum display format to be used for a time unit. *)
      val min_unit : t -> time_unit
      val set_min_unit : t -> time_unit -> unit

      val make : ?display_formats:Display_formats.t ->
                 ?iso_weekday:bool ->
                 ?max:Time.t ->
                 ?min:Time.t ->
                 ?parser:parser ->
                 ?round:time_unit or_false ->
                 ?tooltip_format:string ->
                 ?unit:time_unit ->
                 ?step_size:int ->
                 ?min_unit:time_unit ->
                 unit ->
                 t [@@js.builder]

    end

    type distribution =
      [ `Linear [@js "linear"]
      | `Series [@js "series"]
      ] [@js.enum]
    type bounds =
      [ `Data [@js "data"]
      | `Ticks [@js "ticks"]
      ] [@js.enum]

    val distribution : t -> distribution
    val set_distribution : t -> distribution -> unit

    val bounds : t -> bounds
    val set_bounds : t -> bounds -> unit

    val make :
      (* Common *)
      ?type_:string ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t ->
      (* Time-specific *)
      ?distribution:distribution ->
      ?bounds:bounds ->
      unit ->
      scale [@@js.builder]

  end

end

module Radial : sig

  type t = scale

  module Common : sig

  end

  module Linear : sig

    module Angle_lines : sig
      type t

      (** If true, angle lines are shown. *)
      val display : t -> bool
      val set_display : t -> bool -> unit

      (** Color of angled lines. *)
      val color : t -> Color.t
      val set_color : t -> Color.t -> unit

      (** Width of angled lines. *)
      val line_width : t -> int
      val set_line_width : t -> int -> unit

      val make : ?display:bool ->
                 ?color:Color.t ->
                 ?line_width:int ->
                 unit ->
                 t [@@js.builder]
    end

    module Point_labels : sig
      type t
      (* TODO implement 'callback' property *)

      (** Font color for point labels. *)
      val font_color : t -> Color.t
      val set_font_color : t -> Color.t -> unit

      (** Font family to use when rendering labels. *)
      val font_family : t -> Font.family
      val set_font_family : t -> Font.family -> unit

      (** Font size in pixels. *)
      val font_size : t -> int
      val set_font_size : t -> int -> unit

      (** Font style to use when rendering point labels. *)
      val font_style : t -> Font.style
      val set_font_style : t -> Font.style -> unit

      val make : ?font_color:Color.t ->
                 ?font_family:Font.family ->
                 ?font_size:int ->
                 ?font_style:Font.style ->
                 unit ->
                 t [@@js.builder]

    end

    module Ticks : sig
      type t = Ticks.t

      (** Color of label backdrops. *)
      val backdrop_color : t -> Color.t
      val set_backdrop_color : t -> Color.t -> unit

      (** Horizontal padding of label backdrop. *)
      val backdrop_padding_x : t -> int
      val set_backdrop_padding_x : t -> int -> unit

      (** Vertical padding of label backdrop. *)
      val backdrop_padding_y : t -> int
      val set_backdrop_padding_y : t -> int -> unit

      (** If true, scale will include 0 if it is not already included. *)
      val begin_at_zero : t -> bool
      val set_begin_at_zero : t -> bool -> unit

      (** User defined minimum number for the scale,
          overrides minimum value from data. *)
      val min : t -> float
      val set_min : t -> float -> unit

      (** User defined maximum number for the scale,
          overrides maximum value from data. *)
      val max : t -> float
      val set_max : t -> float -> unit

      (** Maximum number of ticks and gridlines to show. *)
      val max_ticks_limit : t -> int
      val set_max_ticks_limit : t -> int -> unit

      (** If defined and 'stepSize' is not specified, the step size will be
          rounded to this many decimal places. *)
      val precision : t -> int
      val set_precision : t -> int -> unit

      (** User defined fixed step size for the scale. *)
      val step_size : t -> float
      val set_step_size : t -> float -> unit

      (** Adjustment used when calculating the maximum data value. *)
      val suggested_max : t -> float
      val set_suggested_max : t -> float -> unit

      (** Adjustment used when calculating the minimum data value. *)
      val suggested_min : t -> float
      val set_suggested_min : t -> float -> unit

      (** If true, draw a background behind the tick labels. *)
      val show_label_backdrop : t -> bool
      val set_show_label_backdrop : t -> bool -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font.family ->
        ?font_size:int ->
        ?font_style:Font.style ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Linear-specific *)
        ?backdrop_color:Color.t ->
        ?backdrop_padding_x:int ->
        ?backdrop_padding_y:int ->
        ?begin_at_zero:bool ->
        ?min:float ->
        ?max:float ->
        ?max_ticks_limit:int ->
        ?precision:int ->
        ?step_size:float ->
        ?suggested_max:float ->
        ?suggested_min:float ->
        ?show_label_backdrop:bool ->
        unit ->
        t [@@js.builder]

    end

    (** Angle line configuration. *)
    val angle_lines : t -> Angle_lines.t
    val set_angle_lines : t -> Angle_lines.t -> unit

    (** Grid line configuration. *)
    val grid_lines : t -> Grid_lines.t
    val set_grid_lines : t -> Grid_lines.t -> unit

    (** Point label configuration. *)
    val point_labels : t -> Point_labels.t
    val set_point_labels : t -> Point_labels.t -> unit

    (** Tick configuration. *)
    val ticks : t -> Ticks.t
    val set_ticks : t -> Ticks.t -> unit

    val make : ?angle_lines:Angle_lines.t ->
               ?grid_lines:Grid_lines.t ->
               ?point_labels:Point_labels.t ->
               ?ticks:Ticks.t ->
               unit ->
               scale [@@js.builder]

  end

end

type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

val make : ?x_axes:scale list ->
           ?y_axes:scale list ->
           unit ->
           t [@@js.builder]
