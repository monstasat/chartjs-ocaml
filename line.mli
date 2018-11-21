open Chartjs_types

module Options : sig
  type t = Chartjs_options.t

  (** If false, the lines between points are not drawn. *)
  val show_lines : t -> bool
  val set_show_lines : t -> bool -> unit

  (** If false, NaN data causes a break in the line. *)
  val span_gaps : t -> bool
  val set_span_gaps : t -> bool -> unit

end

module Dataset : sig

  type fill =
    [ `Abs of int
    | `Rel of int
    | `Start
    | `End
    | `Origin
    | `Off
    ] [@js.union]

  val fill_to_js : fill -> Ojs.t
    [@@js.custom
     let fill_to_js : fill -> Ojs.t = function
       | `Abs i -> Ojs.int_to_js i
       | `Rel i ->
          let s = Printf.sprintf "%+d" i in
          Ojs.string_to_js s
       | `Start -> Ojs.string_to_js "start"
       | `End -> Ojs.string_to_js "end"
       | `Origin -> Ojs.string_to_js "origin"
       | `Off -> Ojs.bool_to_js false
    ]

  val fill_of_js : Ojs.t -> fill
    [@@js.custom
     let fill_of_js (js : Ojs.t) : fill =
       match Ojs.obj_type js with
       | "[object Number]" -> `Abs (Ojs.int_of_js js)
       | "[object Boolean]" ->
          begin match Ojs.bool_of_js js with
          | true -> `Origin
          | false -> `Off
          end
       | "[object String]" ->
          begin match Ojs.string_of_js js with
          | "start" -> `Start
          | "end" -> `End
          | "origin" -> `Origin
          | s -> `Rel (int_of_string s)
          end
       | _ -> assert false
    ]

  type cubic_interpolation_mode =
    [ `Default [@js "default"]
    | `Monotone [@js "monotone"]
    ] [@js.enum]

  type stepped_line =
    [ `Bool of bool
    | `Before [@js "before"]
    | `After [@js "after"]
    ] [@js.union]

  type int_point_prop =
    [ `Single of int
    | `List of int list
    ] [@js.union]

  type color_point_prop =
    [ `Single of Color.t
    | `List of Color.t list
    ] [@js.union]

  type t = Chart.Dataset.t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val stepped_line_of_js : Ojs.t -> stepped_line
    [@@js.custom
     let stepped_line_of_js (js : Ojs.t) : stepped_line =
       match Ojs.obj_type js with
       | "[object Boolean]" -> `Bool (Ojs.bool_of_js js)
       | "[object String]" ->
          begin match Ojs.string_of_js js with
          | "before" -> `Before
          | "after" -> `After
          | _ -> assert false
          end
       | _ -> assert false
    ]

  val type_ : t -> typ
  val set_type : t -> typ -> unit

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

  (** The fill color under the line. *)
  val background_color : t -> Color.t
  val set_background_color : t -> Color.t -> unit

  (** The color of the line. *)
  val border_color : t -> Color.t
  val set_border_color : t -> Color.t -> unit

  (** The width of the line in pixels. *)
  val border_width : t -> int
  val set_border_width : t -> int -> unit

  (** Length and spacing of dashes. *)
  val border_dash : t -> border_dash
  val set_border_dash : t -> border_dash -> unit

  (** Offset for line dashes. *)
  val border_dash_offset : t -> border_dash_offset
  val set_border_dash_offset : t -> border_dash_offset -> unit

  (** Cap style of the line. *)
  val border_cap_style : t -> line_cap
  val set_border_cap_style : t -> line_cap -> unit

  (** Line joint style. *)
  val border_join_style : t -> line_join
  val set_border_join_style : t -> line_join -> unit

  (** Algorithm used to interpolate a smooth
      curve from the discrete data points. *)
  val cubic_interpolation_mode : t -> cubic_interpolation_mode
  val set_cubic_interpolation_mode : t -> cubic_interpolation_mode -> unit

  (** How to fill the area under the line. *)
  val fill : t -> fill
  val set_fill : t -> fill -> unit

  (** Bezier curve tension of the line. Set to 0 to draw straightlines.
      This option is ignored if monotone cubic interpolation is used. *)
  val line_tension : t -> float
  val set_line_tension : t -> float -> unit

  (** If false, the line is not drawn for this dataset. *)
  val show_line : t -> bool
  val set_show_line : t -> bool -> unit

  (** If true, lines will be drawn between points with no or null data.
      If false, points with NaN data will create a break in the line *)
  val span_gaps : t -> bool
  val set_span_gaps : t -> bool -> unit

  (** If the line is shown as a stepped line. *)
  val stepped_line : t -> stepped_line
  val set_stepped_line : t -> stepped_line -> unit

  (** Point properties *)

  (** The fill color for points. *)
  val point_background_color : t -> Color.t indexable
  val set_point_background_color : t -> Color.t indexable -> unit

  (** The border color for points. *)
  val point_border_color : t -> Color.t indexable
  val set_point_border_color : t -> Color.t indexable -> unit

  (** The width of the point border in pixels. *)
  val point_border_width : t -> int indexable
  val set_point_border_width : t -> int indexable -> unit

  (** The radius of the point shape. If set to 0, the point is not rendered. *)
  val point_radius : t -> int indexable
  val set_point_radius : t -> int indexable -> unit

  (** The rotation of the point in degrees. *)
  val point_rotation : t -> int indexable
  val set_point_rotation : t -> int indexable -> unit

  (** The pixel size of the non-displayed point that reacts to mouse events. *)
  val point_hit_radius : t -> int indexable
  val set_point_hit_radius : t -> int indexable -> unit

  val point_hover_background_color : t -> Color.t indexable
  val set_point_hover_background_color : t -> Color.t indexable -> unit

  val point_hover_border_color : t -> Color.t indexable
  val set_point_hover_border_color : t -> Color.t indexable -> unit

  (** Border width of point when hovered. *)
  val point_hover_border_width : t -> int indexable
  val set_point_hover_border_width : t -> int indexable -> unit

  (** The radius of the point when hovered. *)
  val point_hover_radius : t -> int indexable
  val set_point_hover_radius : t -> int indexable -> unit

  module Indexable : sig

    (** The fill color for points. *)
    val point_background_color : t -> Chartjs_array.Color.t

    (** The border color for points. *)
    val point_border_color : t -> Chartjs_array.Color.t

    (** The width of the point border in pixels. *)
    val point_border_width : t -> Chartjs_array.Int.t

    (** The radius of the point shape. If set to 0, the point is not rendered. *)
    val point_radius : t -> Chartjs_array.Int.t

    (** The rotation of the point in degrees. *)
    val point_rotation : t -> Chartjs_array.Int.t

    (** The pixel size of the non-displayed point that reacts to mouse events. *)
    val point_hit_radius : t -> Chartjs_array.Int.t

    (** Point background color when hovered. *)
    val point_hover_background_color : t -> Chartjs_array.Color.t

    (** Point border color when hovered. *)
    val point_hover_border_color : t -> Chartjs_array.Color.t

    (** Border width of point when hovered. *)
    val point_hover_border_width : t -> Chartjs_array.Int.t

    (** The radius of the point when hovered. *)
    val point_hover_radius : t -> Chartjs_array.Int.t

  end

  val make : ?type_:typ ->
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
             (* Point properties *)
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
             ?data:Ojs.t ->
             unit ->
             t [@@js.builder]

end
