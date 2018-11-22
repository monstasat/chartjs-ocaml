module Scale_label : sig
  type padding = [ `Int of int | `Obj of padding_obj ]
  and padding_obj =
    {
      top : int option;
      bottom : int option;
    }
  val padding_of_js : Ojs.t -> padding
  type t
  val display : t -> bool
  val set_display : t -> bool -> unit
  val label_string : t -> string
  val set_label_string : t -> string -> unit
  val line_height : t -> Chartjs_types.line_height
  val set_line_height : t -> Chartjs_types.line_height -> unit
  val font_color : t -> Chartjs_types.Color.t
  val set_font_color : t -> Chartjs_types.Color.t -> unit
  val font_family : t -> Chartjs_types.Font_family.t
  val set_font_family : t -> Chartjs_types.Font_family.t -> unit
  val font_size : t -> int
  val set_font_size : t -> int -> unit
  val font_style : t -> Chartjs_types.Font_style.t
  val set_font_style : t -> Chartjs_types.Font_style.t -> unit
  val padding : t -> padding
  val set_padding : t -> padding -> unit
  val make :
    ?display:bool ->
    ?label_string:string ->
    ?line_height:Chartjs_types.line_height ->
    ?font_color:Chartjs_types.Color.t ->
    ?font_family:Chartjs_types.Font_family.t ->
    ?font_size:int ->
    ?font_style:Chartjs_types.Font_style.t -> ?padding:padding -> unit -> t
end

module Grid_lines : sig
  type color =
    [ `List of Chartjs_types.Color.t list
    | `Single of Chartjs_types.Color.t ]
  val color_of_js : Ojs.t -> color
  type line_width = [ `List of int list | `Single of int ]
  val line_width_of_js : Ojs.t -> line_width
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val display : t -> bool
  val set_display : t -> bool -> unit
  val circular : t -> bool
  val set_circular : t -> bool -> unit
  val color : t -> color
  val set_color : t -> color -> unit
  val border_dash : t -> Chartjs_types.border_dash
  val set_border_dash : t -> Chartjs_types.border_dash -> unit
  val border_dash_offset : t -> Chartjs_types.border_dash_offset
  val set_border_dash_offset : t -> Chartjs_types.border_dash_offset -> unit
  val line_width : t -> line_width
  val set_line_width : t -> line_width -> unit
  val draw_border : t -> bool
  val set_draw_border : t -> bool -> unit
  val draw_on_chart_area : t -> bool
  val set_draw_on_chart_area : t -> bool -> unit
  val draw_ticks : t -> bool
  val set_draw_ticks : t -> bool -> unit
  val tick_mark_length : t -> int
  val set_tick_mark_length : t -> int -> unit
  val zero_line_width : t -> int
  val set_zero_line_width : t -> int -> unit
  val zero_line_color : t -> Chartjs_types.Color.t
  val set_zero_line_color : t -> Chartjs_types.Color.t -> unit
  val zero_line_border_dash : t -> Chartjs_types.border_dash
  val set_zero_line_border_dash : t -> Chartjs_types.border_dash -> unit
  val zero_line_border_dash_offset : t -> Chartjs_types.border_dash_offset
  val set_zero_line_border_dash_offset :
    t -> Chartjs_types.border_dash_offset -> unit
  val offset_grid_lines : t -> bool
  val set_offset_grid_lines : t -> bool -> unit
  val make :
    ?display:bool ->
    ?circular:bool ->
    ?color:color ->
    ?border_dash:Chartjs_types.border_dash ->
    ?border_dash_offset:Chartjs_types.border_dash_offset ->
    ?line_width:line_width ->
    ?draw_border:bool ->
    ?draw_on_chart_area:bool ->
    ?draw_ticks:bool ->
    ?tick_mark_length:int ->
    ?zero_line_width:int ->
    ?zero_line_color:Chartjs_types.Color.t ->
    ?zero_line_border_dash:Chartjs_types.border_dash ->
    ?zero_line_border_dash_offset:Chartjs_types.border_dash_offset ->
    ?offset_grid_lines:bool -> unit -> t
end

module Ticks : sig
  type callback = value:Ojs.t -> index:int -> values:Ojs.t -> string option
  module Minor_major : sig
    type t
    val callback : t -> callback
    val set_callback : t -> callback -> unit
    val font_color : t -> Chartjs_types.Color.t
    val set_font_color : t -> Chartjs_types.Color.t -> unit
    val font_family : t -> Chartjs_types.Font_family.t
    val set_font_family : t -> Chartjs_types.Font_family.t -> unit
    val font_size : t -> int
    val set_font_size : t -> int -> unit
    val font_style : t -> Chartjs_types.Font_style.t
    val set_font_style : t -> Chartjs_types.Font_style.t -> unit
  end
  type t
  val callback : t -> callback
  val set_callback : t -> callback -> unit
  val display : t -> bool
  val set_display : t -> bool -> unit
  val font_color : t -> Chartjs_types.Color.t
  val set_font_color : t -> Chartjs_types.Color.t -> unit
  val font_family : t -> Chartjs_types.Font_family.t
  val set_font_family : t -> Chartjs_types.Font_family.t -> unit
  val font_size : t -> int
  val set_font_size : t -> int -> unit
  val font_style : t -> Chartjs_types.Font_style.t
  val set_font_style : t -> Chartjs_types.Font_style.t -> unit
  val reverse : t -> bool
  val set_reverse : t -> bool -> unit
  val minor : t -> Minor_major.t
  val set_minor : t -> Minor_major.t -> unit
  val major : t -> Minor_major.t
  val set_major : t -> Minor_major.t -> unit
  val make :
    ?callback:callback ->
    ?display:bool ->
    ?font_color:Chartjs_types.Color.t ->
    ?font_family:Chartjs_types.Font_family.t ->
    ?font_size:int ->
    ?font_style:Chartjs_types.Font_style.t ->
    ?reverse:bool ->
    ?minor:Minor_major.t ->
    ?major:Minor_major.t -> unit -> t
end

type typ =
  [ `Category
    | `Linear
    | `Logarithmic
    | `Time
    | `Custom of string
    ]
type t

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

type scales

val scales_to_js : scales -> Ojs.t
val scales_of_js : Ojs.t -> scales

val make :
  ?x_axes:t list ->
  ?y_axes:t list ->
  unit ->
  scales

module Cartesian : sig
  type position =
    [ `Left
    | `Right
    | `Top
    | `Bottom
    ]

  module Ticks : sig
    type t = Ticks.t
    include module type of Ticks with type t := t
    val auto_skip : t -> bool
    val set_auto_skip : t -> bool -> unit
    val auto_skip_padding : t -> int
    val set_auto_skip_padding : t -> int -> unit
    val label_offset : t -> int
    val set_label_offset : t -> int -> unit
    val max_rotation : t -> int
    val set_max_rotation : t -> int -> unit
    val min_rotation : t -> int
    val set_min_rotation : t -> int -> unit
    val mirror : t -> bool
    val set_mirror : t -> bool -> unit
    val padding : t -> int
    val set_padding : t -> int -> unit
  end

  module Category : sig
    module Ticks : sig
      type t = Ticks.t
      include module type of Ticks with type t := t
      val labels : Ticks.t -> string list
      val set_labels : Ticks.t -> string list -> unit
      val min : Ticks.t -> string option
      val set_min : Ticks.t -> string option -> unit
      val max : Ticks.t -> string option
      val set_max : Ticks.t -> string option -> unit
      val make :
        ?display:bool ->
        ?font_color:Chartjs_types.Color.t ->
        ?font_family:Chartjs_types.Font_family.t ->
        ?font_size:int ->
        ?font_style:Chartjs_types.Font_style.t ->
        ?reverse:bool ->
        ?minor:Ticks.Minor_major.t ->
        ?major:Ticks.Minor_major.t ->
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
        ?labels:string list ->
        ?min:string ->
        ?max:string -> unit -> t
    end
    val type_ : t -> string
    val set_type_ : t -> string -> unit
    val position : t -> position
    val set_position : t -> position -> unit
    val offset : t -> bool
    val set_offset : t -> bool -> unit
    val id : t -> string
    val set_id : t -> string -> unit
    val grid_lines : t -> Grid_lines.t
    val set_grid_lines :
      t -> Grid_lines.t -> unit
    val scale_label : t -> Scale_label.t
    val set_scale_label :
      t -> Scale_label.t -> unit
    val ticks : t -> Ticks.t
    val set_ticks : t -> Ticks.t -> unit
    val make :
      ?type_:typ ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t -> unit -> t
  end

  module Linear : sig
    module Ticks : sig
      type t = Ticks.t
      include module type of Ticks with type t := t
      val auto_skip : t -> bool
      val set_auto_skip : t -> bool -> unit
      val auto_skip_padding : t -> int
      val set_auto_skip_padding : t -> int -> unit
      val label_offset : t -> int
      val set_label_offset : t -> int -> unit
      val max_rotation : t -> int
      val set_max_rotation : t -> int -> unit
      val min_rotation : t -> int
      val set_min_rotation : t -> int -> unit
      val mirror : t -> bool
      val set_mirror : t -> bool -> unit
      val padding : t -> int
      val set_padding : t -> int -> unit
      val begin_at_zero : t -> bool
      val set_begin_at_zero : t -> bool -> unit
      val min : t -> float option
      val set_min : t -> float option -> unit
      val max : t -> float option
      val set_max : t -> float option -> unit
      val max_ticks_limit : t -> int
      val set_max_ticks_limit : t -> int -> unit
      val precision : t -> int
      val set_precision : t -> int -> unit
      val step_size : t -> float
      val set_step_size : t -> float -> unit
      val suggested_max : t -> float option
      val set_suggested_max : t -> float option -> unit
      val suggested_min : t -> float option
      val set_suggested_min : t -> float option -> unit
      val make :
        ?display:bool ->
        ?font_color:Chartjs_types.Color.t ->
        ?font_family:Chartjs_types.Font_family.t ->
        ?font_size:int ->
        ?font_style:Chartjs_types.Font_style.t ->
        ?reverse:bool ->
        ?minor:Minor_major.t ->
        ?major:Minor_major.t ->
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
        ?begin_at_zero:bool ->
        ?min:float ->
        ?max:float ->
        ?max_ticks_limit:int ->
        ?precision:int ->
        ?step_size:float ->
        ?suggested_max:float ->
        ?suggested_min:float -> unit -> t
    end
    val type_ : t -> string
    val set_type_ : t -> string -> unit
    val position : t -> position
    val set_position : t -> position -> unit
    val offset : t -> bool
    val set_offset : t -> bool -> unit
    val id : t -> string
    val set_id : t -> string -> unit
    val grid_lines : t -> Grid_lines.t
    val set_grid_lines : t -> Grid_lines.t -> unit
    val scale_label : t -> Scale_label.t
    val set_scale_label : t -> Scale_label.t -> unit
    val ticks : t -> Ticks.t
    val set_ticks : t -> Ticks.t -> unit
    val make :
      ?type_:typ ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t -> unit -> t
  end
  module Logarithmic : sig
    module Ticks : sig
      type t = Ticks.t
      include module type of Ticks with type t := t
      val auto_skip : t -> bool
      val set_auto_skip : t -> bool -> unit
      val auto_skip_padding : t -> int
      val set_auto_skip_padding : t -> int -> unit
      val label_offset : t -> int
      val set_label_offset : t -> int -> unit
      val max_rotation : t -> int
      val set_max_rotation : t -> int -> unit
      val min_rotation : t -> int
      val set_min_rotation : t -> int -> unit
      val mirror : t -> bool
      val set_mirror : t -> bool -> unit
      val padding : t -> int
      val set_padding : t -> int -> unit
      val min : t -> float option
      val set_min : t -> float option -> unit
      val max : t -> float option
      val set_max : t -> float option -> unit
      val make :
        ?display:bool ->
        ?font_color:Chartjs_types.Color.t ->
        ?font_family:Chartjs_types.Font_family.t ->
        ?font_size:int ->
        ?font_style:Chartjs_types.Font_style.t ->
        ?reverse:bool ->
        ?minor:Minor_major.t ->
        ?major:Minor_major.t ->
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
        ?min:float -> ?max:float -> unit -> t
    end
    val type_ : t -> string
    val set_type_ : t -> string -> unit
    val position : t -> position
    val set_position : t -> position -> unit
    val offset : t -> bool
    val set_offset : t -> bool -> unit
    val id : t -> string
    val set_id : t -> string -> unit
    val grid_lines : t -> Grid_lines.t
    val set_grid_lines : t -> Grid_lines.t -> unit
    val scale_label : t -> Scale_label.t
    val set_scale_label : t -> Scale_label.t -> unit
    val ticks : t -> Ticks.t
    val set_ticks : t -> Ticks.t -> unit
    val make :
      ?type_:typ ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t -> unit -> t
  end
  module Time : sig
    module Ticks : sig
      type t = Ticks.t
      include module type of Ticks with type t := t
      type source = [ `Auto | `Data | `Labels ]
      val source : t -> source
      val set_source : t -> source -> unit
      val make :
        ?display:bool ->
        ?font_color:Chartjs_types.Color.t ->
        ?font_family:Chartjs_types.Font_family.t ->
        ?font_size:int ->
        ?font_style:Chartjs_types.Font_style.t ->
        ?reverse:bool ->
        ?minor:Minor_major.t ->
        ?major:Minor_major.t ->
        ?auto_skip:bool ->
        ?auto_skip_padding:int ->
        ?label_offset:int ->
        ?max_rotation:int ->
        ?min_rotation:int ->
        ?mirror:bool ->
        ?padding:int ->
        ?source:source -> unit -> t
    end
    module Time : sig
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
        val make :
          ?millisecond:string ->
          ?second:string ->
          ?minute:string ->
          ?hour:string ->
          ?day:string ->
          ?week:string ->
          ?month:string ->
          ?quarter:string -> ?year:string -> unit -> t
      end
      type parser
      type time_unit =
        [ `Day
        | `Hour
        | `Millisecond
        | `Minute
        | `Month
        | `Quarter
        | `Second
        | `Week
        | `Year ]
      type t
      val display_formats : t -> Display_formats.t
      val set_display_formats : t -> Display_formats.t -> unit
      val iso_weekday : t -> bool
      val set_iso_weekday : t -> bool -> unit
      val max : t -> Ptime.t option
      val set_max : t -> Ptime.t option -> unit
      val min : t -> Ptime.t option
      val set_min : t -> Ptime.t option -> unit
      val parser : t -> parser
      val set_parser : t -> parser -> unit
      val round : t -> time_unit Chartjs_types.or_false
      val set_round : t -> time_unit Chartjs_types.or_false -> unit
      val tooltip_format : t -> string
      val set_tooltip_format : t -> string -> unit
      val unit : t -> time_unit
      val set_unit : t -> time_unit -> unit
      val step_size : t -> int
      val set_step_size : t -> int -> unit
      val min_unit : t -> time_unit
      val set_min_unit : t -> time_unit -> unit
      val make :
        ?display_formats:Display_formats.t ->
        ?iso_weekday:bool ->
        ?max:Chartjs_types.Time.t ->
        ?min:Chartjs_types.Time.t ->
        ?parser:parser ->
        ?round:time_unit Chartjs_types.or_false ->
        ?tooltip_format:string ->
        ?unit:time_unit ->
        ?step_size:int -> ?min_unit:time_unit -> unit -> t
    end

    type distribution = [ `Linear | `Series ]
    type bounds = [ `Data | `Ticks ]

    val type_ : t -> string
    val set_type_ : t -> string -> unit
    val position : t -> position
    val set_position : t -> position -> unit
    val offset : t -> bool
    val set_offset : t -> bool -> unit
    val id : t -> string
    val set_id : t -> string -> unit
    val grid_lines : t -> Grid_lines.t
    val set_grid_lines : t -> Grid_lines.t -> unit
    val scale_label : t -> Scale_label.t
    val set_scale_label : t -> Scale_label.t -> unit
    val ticks : t -> Ticks.t
    val set_ticks : t -> Ticks.t -> unit
    val distribution : t -> distribution
    val set_distribution : t -> distribution -> unit
    val bounds : t -> bounds
    val set_bounds : t -> bounds -> unit
    val make :
      ?type_:typ ->
      ?position:position ->
      ?offset:bool ->
      ?id:string ->
      ?grid_lines:Grid_lines.t ->
      ?scale_label:Scale_label.t ->
      ?ticks:Ticks.t ->
      ?time:Time.t ->
      ?distribution:distribution ->
      ?bounds:bounds -> unit -> t
  end
end
module Radial : sig
  module Linear : sig
    module Ticks : sig
      type t = Ticks.t
      include module type of Ticks with type t := t
      val backdrop_color : t -> Chartjs_types.Color.t
      val set_backdrop_color : t -> Chartjs_types.Color.t -> unit
      val backdrop_padding_x : t -> int
      val set_backdrop_padding_x : t -> int -> unit
      val backdrop_padding_y : t -> int
      val set_backdrop_padding_y : t -> int -> unit
      val begin_at_zero : t -> bool
      val set_begin_at_zero : t -> bool -> unit
      val min : t -> float option
      val set_min : t -> float option -> unit
      val max : Ticks.t -> float option
      val set_max : t -> float option -> unit
      val max_ticks_limit : t -> int
      val set_max_ticks_limit : t -> int -> unit
      val precision : t -> int
      val set_precision : t -> int -> unit
      val step_size : t -> float
      val set_step_size : t -> float -> unit
      val suggested_max : t -> float option
      val set_suggested_max : t -> float option -> unit
      val suggested_min : t -> float option
      val set_suggested_min : t -> float option -> unit
      val show_label_backdrop : t -> bool
      val set_show_label_backdrop : t -> bool -> unit
      val make :
        ?display:bool ->
        ?font_color:Chartjs_types.Color.t ->
        ?font_family:Chartjs_types.Font_family.t ->
        ?font_size:int ->
        ?font_style:Chartjs_types.Font_style.t ->
        ?reverse:bool ->
        ?minor:Minor_major.t ->
        ?major:Minor_major.t ->
        ?backdrop_color:Chartjs_types.Color.t ->
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
        ?show_label_backdrop:bool -> unit -> t
    end
    module Angle_lines : sig
      type t
      val display : t -> bool
      val set_display : t -> bool -> unit
      val color : t -> Chartjs_types.Color.t
      val set_color : t -> Chartjs_types.Color.t -> unit
      val line_width : t -> int
      val set_line_width : t -> int -> unit
      val make :
        ?display:bool ->
        ?color:Chartjs_types.Color.t -> ?line_width:int -> unit -> t
    end
    module Point_labels : sig
      type t
      val font_color : t -> Chartjs_types.Color.t
      val set_font_color : t -> Chartjs_types.Color.t -> unit
      val font_family : t -> Chartjs_types.Font_family.t
      val set_font_family : t -> Chartjs_types.Font_family.t -> unit
      val font_size : t -> int
      val set_font_size : t -> int -> unit
      val font_style : t -> Chartjs_types.Font_style.t
      val set_font_style : t -> Chartjs_types.Font_style.t -> unit
      val make :
        ?font_color:Chartjs_types.Color.t ->
        ?font_family:Chartjs_types.Font_family.t ->
        ?font_size:int ->
        ?font_style:Chartjs_types.Font_style.t -> unit -> t
    end
    val angle_lines : t -> Angle_lines.t
    val set_angle_lines : t -> Angle_lines.t -> unit
    val grid_lines : t -> Grid_lines.t
    val set_grid_lines :
      t -> Grid_lines.t -> unit
    val point_labels : t -> Point_labels.t
    val set_point_labels : t -> Point_labels.t -> unit
    val ticks : t -> Ticks.t
    val set_ticks : t -> Ticks.t -> unit
    val make :
      ?angle_lines:Angle_lines.t ->
      ?grid_lines:Grid_lines.t ->
      ?point_labels:Point_labels.t ->
      ?ticks:Ticks.t -> unit -> t
  end
end
