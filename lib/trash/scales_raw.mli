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
