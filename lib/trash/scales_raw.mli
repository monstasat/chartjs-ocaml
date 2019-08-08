open Types
open Option_types

type t

module Cartesian : sig
  type position =
    [ `Left [@js "left"]
    | `Right [@js "right"]
    | `Top [@js "top"]
    | `Bottom [@js "bottom"]
    ] [@js.enum]

  module Common_ticks : sig
    type t = Ticks.t

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

  end

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
      ?type_:typ ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t ->
      unit ->
      t [@@js.builder]

  end

  module Category : sig

    module Ticks : sig
      type t = Ticks.t

      (** An array of labels to display. *)
      val labels : t -> string list
      val set_labels : t -> string list -> unit

      (** The minimum item to display. *)
      val min : t -> string option
      val set_min : t -> string option -> unit

      (** The maximum item to display. *)
      val max : t -> string option
      val set_max : t -> string option -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font_family.t ->
        ?font_size:int ->
        ?font_style:Font_style.t ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Cartesian-specific *)
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
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
      val min : t -> float option
      val set_min : t -> float option -> unit

      (** User defined maximum number for the scale,
        overrides maximum value from data. *)
      val max : t -> float option
      val set_max : t -> float option -> unit

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
      val suggested_max : t -> float option
      val set_suggested_max : t -> float option -> unit

      (** Adjustment used when calculating the minimum data value. *)
      val suggested_min : t -> float option
      val set_suggested_min : t -> float option -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font_family.t ->
        ?font_size:int ->
        ?font_style:Font_style.t ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Cartesian-specific *)
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
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
      val min : t -> float option
      val set_min : t -> float option -> unit

      (** User defined maximum number for the scale,
        overrides maximum value from data. *)
      val max : t -> float option
      val set_max : t -> float option -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font_family.t ->
        ?font_size:int ->
        ?font_style:Font_style.t ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Cartesian-specific *)
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
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
        ?font_family:Font_family.t ->
        ?font_size:int ->
        ?font_style:Font_style.t ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        (* Cartesian-specific *)
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
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
      val max : t -> Time.t option
      val set_max : t -> Time.t option -> unit

      (** If defined, this will override the data minimum *)
      val min : t -> Time.t option
      val set_min : t -> Time.t option -> unit

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
      ?type_:typ ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t ->
      (* Time-specific *)
      ?time:Time.t ->
      ?distribution:distribution ->
      ?bounds:bounds ->
      unit ->
      t [@@js.builder]

  end

end

module Radial : sig

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
      val font_family : t -> Font_family.t
      val set_font_family : t -> Font_family.t -> unit

      (** Font size in pixels. *)
      val font_size : t -> int
      val set_font_size : t -> int -> unit

      (** Font style to use when rendering point labels. *)
      val font_style : t -> Font_style.t
      val set_font_style : t -> Font_style.t -> unit

      val make : ?font_color:Color.t ->
                 ?font_family:Font_family.t ->
                 ?font_size:int ->
                 ?font_style:Font_style.t ->
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
      val min : t -> float option
      val set_min : t -> float option -> unit

      (** User defined maximum number for the scale,
          overrides maximum value from data. *)
      val max : t -> float option
      val set_max : t -> float option -> unit

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
      val suggested_max : t -> float option
      val set_suggested_max : t -> float option -> unit

      (** Adjustment used when calculating the minimum data value. *)
      val suggested_min : t -> float option
      val set_suggested_min : t -> float option -> unit

      (** If true, draw a background behind the tick labels. *)
      val show_label_backdrop : t -> bool
      val set_show_label_backdrop : t -> bool -> unit

      val make :
        (* Common *)
        ?display:bool ->
        ?font_color:Color.t ->
        ?font_family:Font_family.t ->
        ?font_size:int ->
        ?font_style:Font_style.t ->
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
               t [@@js.builder]

  end

end

type scales

val make : ?x_axes:t list ->
           ?y_axes:t list ->
           unit ->
           scales [@@js.builder]
