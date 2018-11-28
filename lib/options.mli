open Types

module Animation : sig
  type animation =
    { chart : Types.t
    ; current_step : float option
    ; num_steps : float
    ; easing : easing option
    ; render : (Types.t -> animation -> unit) option
    ; on_animation_progress : callback option
    ; on_animation_complete : callback option
    }
  and callback = animation -> unit

  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  (** The number of milliseconds an animation takes. *)
  val duration : t -> int
  val set_duration : t -> int -> unit

  (** Easing function to use. *)
  val easing : t -> easing
  val set_easing : t -> easing -> unit

  (** Callback called on each step of an animation. *)
  val on_progress : t -> callback
  val set_on_progress : t -> callback -> unit

  (** Callback called at the end of an animation. *)
  val on_complete : t -> callback
  val set_on_complete : t -> callback -> unit

  val make : ?duration:int ->
             ?easing:easing ->
             ?on_progress:callback ->
             ?on_complete:callback ->
             unit ->
             t [@@js.builder]

end

module Layout : sig
  type t

  (** The padding to add inside the chart. *)
  val padding : t -> Padding.t
  val set_padding : t -> Padding.t -> unit

  val make : ?padding:Padding.t -> unit -> t [@@js.builder]

end

module Legend : sig

  module Item : sig
    type t

    (** Label that will be displayed. *)
    val text : t -> string option
    val set_text : t -> string -> unit

    (** Fill style of the legend box *)
    val fill_style : t -> Color.t option
    val set_fill_style : t -> Color.t -> unit

    (** If true, this item represents a hidden dataset.
        Label will be rendered with a strike-through effect *)
    val hidden : t -> bool
    val set_hidden : t -> bool -> unit

    (** For box border. *)
    val line_cap : t -> line_cap option
    val set_line_cap : t -> line_cap -> unit

    (** For box border. *)
    val line_dash : t -> border_dash option
    val set_line_dash : t -> border_dash -> unit

    (** For box border. *)
    val line_dash_offset : t -> border_dash_offset
    val set_line_dash_offset : t -> border_dash_offset -> unit

    (** For box border. *)
    val line_join : t -> line_join option
    val set_line_join : t -> line_join -> unit

    (** Width of box border. *)
    val line_width : t -> int option
    val set_line_width : t -> int -> unit

    (** Stroke style of the legend box *)
    val stroke_style : t -> Color.t option
    val set_stroke_style : t -> Color.t -> unit

    (** Point style of the legend box (only used if usePointStyle is true) *)
    val point_style : t -> point_style option
    val set_point_style : t -> point_style -> unit

    val dataset_index : t -> int option
    val set_dataset_index : t -> int -> unit

    val make : ?fill_style:Color.t ->
               ?hidden:bool ->
               ?line_cap:line_cap ->
               ?line_dash:border_dash ->
               ?line_dash_offset:border_dash_offset ->
               ?line_join:line_join ->
               ?line_width:int ->
               ?stroke_style:Color.t ->
               ?point_style:point_style ->
               ?dataset_index:int ->
               ?text:string ->
               unit ->
               t [@@js.builder]

  end

  module Labels : sig

    type generate = Types.t -> Item.t list

    type filter = item:Item.t -> Data.t -> bool

    type t

    (** Width of coloured box. *)
    val box_width : t -> int
    val set_box_width : t -> int -> unit

    (** Font size of text. *)
    val font_size : t -> int
    val set_font_size : t -> int -> unit

    (** Font style of text. *)
    val font_style : t -> Font_style.t
    val set_font_style : t -> Font_style.t -> unit

    (** Color of text. *)
    val font_color : t -> Color.t
    val set_font_color : t -> Color.t -> unit

    (** Font family of legend text. *)
    val font_family : t -> Font_family.t
    val set_font_family : t -> Font_family.t -> unit

    (** Padding between rows of colored boxes. *)
    val padding : t -> int
    val set_padding : t -> int -> unit

    (** Generates legend items for each thing in the legend.
        Default implementation returns the text + styling for the color box. *)
    val generate_labels : t -> generate
    val set_generate_labels : t -> generate -> unit

    (** Filters legend items out of the legend. Receives 2 parameters,
        a Legend Item and the chart data. *)
    val filter : t -> filter
    val set_filter : t -> filter -> unit

    (** Label style will match corresponding point style
        (size is based on fontSize, boxWidth is not used in this case). *)
    val use_point_style : t -> bool
    val set_use_point_style : t -> bool -> unit

    val make : ?box_width:int ->
               ?font_size:int ->
               ?font_style:Font_style.t ->
               ?font_color:Color.t ->
               ?font_family:Font_family.t ->
               ?padding:int ->
               ?generate_labels:generate ->
               ?filter:filter ->
               ?use_point_style:bool ->
               unit ->
               t [@@js.builder]

  end

  type position =
    [ `Left [@js "left"]
    | `Right [@js "right"]
    | `Top [@js "top"]
    | `Bottom [@js "bottom"]
    ] [@js.enum]

  type handler = event:Ojs.t -> item:Item.t -> unit

  type t

  (** Is the legend shown. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** Position of the legend. *)
  val position : t -> position
  val set_position : t -> position -> unit

  (** Marks that this box should take the full width of the canvas
      (pushing down other boxes). This is unlikely to need to be changed
      in day-to-day use. *)
  val full_width : t -> bool
  val set_full_width : t -> bool -> unit

  (** A callback that is called when a click event is registered on a label item *)
  val on_click : t -> handler
  val set_on_click : t -> handler -> unit

  (** A callback that is called when a 'mousemove' event is
      registered on top of a label item *)
  val on_hover : t -> handler
  val set_on_hover : t -> handler -> unit

  (** Legend will show datasets in reverse order. *)
  val reverse : t -> bool
  val set_reverse : t -> bool -> unit

  (** Legend label configuration. *)
  val labels : t -> Labels.t
  val set_labels : t -> Labels.t -> unit

  val make : ?display:bool ->
             ?position:position ->
             ?full_width:bool ->
             ?on_click:handler ->
             ?on_hover:handler ->
             ?reverse:bool ->
             ?labels:Labels.t ->
             unit ->
             t [@@js.builder]

end

module Title : sig

  type position =
    [ `Left [@js "left"]
    | `Right [@js "right"]
    | `Top [@js "top"]
    | `Bottom [@js "bottom"]
    ] [@js.enum]

  type t

  (** Is the title shown. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** Position of title. *)
  val position : t -> position
  val set_position : t -> position -> unit

  (** Font size. *)
  val font_size : t -> int
  val set_font_size : t -> int -> unit

  (** Font family for the title text. *)
  val font_family : t -> Font_family.t
  val set_font_family : t -> Font_family.t -> unit

  (** Font color. *)
  val font_color : t -> Color.t
  val set_font_color : t -> Color.t -> unit

  (** Font style. *)
  val font_style : t -> Font_style.t
  val set_font_style : t -> Font_style.t -> unit

  (** Number of pixels to add above and below the title text. *)
  val padding : t -> int
  val set_padding : t -> int -> unit

  (** Height of an individual line of text. *)
  val line_height : t -> line_height
  val set_line_height : t -> line_height -> unit

  (** Title text to display. If specified as an array,
      text is rendered on multiple lines. *)
  val text : t -> string
  val set_text : t -> string -> unit

  val make : ?display:bool ->
             ?position:position ->
             ?font_size:int ->
             ?font_family:Font_family.t ->
             ?font_color:Color.t ->
             ?font_style:Font_style.t ->
             ?padding:int ->
             ?line_height:line_height ->
             ?text:text ->
             unit ->
             t [@@js.builder]

end

module Tooltips : sig

  type color =
    { border_color : Color.t
    ; background_color : Color.t
    }

  module Item : sig
    (* FIXME x_label and y_label must be of some defined type *)
    type t =
      { (** X Value of the tooltip as a string. *)
        x_label : Ojs.t
      (** Y value of the tooltip as a string. *)
      ; y_label : Ojs.t
      (** Index of the dataset the item comes from. *)
      ; dataset_index : int
      (** Index of this data item in the dataset. *)
      ; index : int
      (** X position of matching point. *)
      ; x : float
      (** Y position of matching point. *)
      ; y : float
      }

  end

  module Model : sig

    type t =
      { data_points : Item.t list option
      ; x_padding : int
      ; y_padding : int
      ; x_align : string option
      ; y_align : string option
      ; x : float
      ; y : float
      ; width : float
      ; height : float
      ; caret_x : int
      ; caret_y : int
      ; body : body option
      ; before_body : string list option
      ; after_body : string list option
      ; body_font_color : Color.t
      ; body_font_family : Font_family.t [@js "_bodyFontFamily"]
      ; body_font_style : Font_style.t [@js "_bodyFontStyle"]
      ; body_align : string [@js "_bodyAlign"]
      ; body_font_size : int
      ; body_spacing : int
      ; title : string list option
      ; title_font_color : Color.t
      ; title_font_family : Font_family.t [@js "_titleFontFamily"]
      ; title_font_style : Font_style.t [@js "_titleFontStyle"]
      ; title_font_size : int
      ; title_align : string [@js "_titleAlign"]
      ; title_spacing : int
      ; title_margin_bottom : int
      ; footer : string list option
      ; footer_font_color : Color.t
      ; footer_font_family : Font_family.t [@js "_footerFontFamily"]
      ; footer_font_style : Font_style.t [@js "_footerFontStyle"]
      ; footer_font_size : int
      ; footer_align : string [@js "_footerAlign"]
      ; footer_spacing : int
      ; footer_margin_top : int
      ; caret_size : int
      ; corner_radius : int
      ; background_color : Color.t
      ; label_colors : color list option
      ; opacity : int
      ; legend_color_background : Color.t
      ; display_color : bool
      }
    and body =
      { before : string list option
      ; lines : string list option
      ; after : string list option
      }

  end

  module Callbacks : sig
    type t

    type items_cb = Item.t list -> Data.t -> text
    type item_cb = Item.t -> Data.t -> text
    type label_color_cb = Item.t -> Types.t -> color
    type label_text_color_cb = Item.t -> Types.t -> Color.t

    (** Returns the text to render before the title. *)
    val before_title : t -> items_cb
    val set_before_title : t -> items_cb -> unit

    (** Returns text to render as the title of the tooltip. *)
    val title : t -> items_cb
    val set_title : t -> items_cb -> unit

    (** Returns text to render after the title. *)
    val after_title : t -> items_cb
    val set_after_title : t -> items_cb -> unit

    (** Returns text to render before the body section. *)
    val before_body : t -> items_cb
    val set_before_body : t -> items_cb -> unit

    (** Returns text to render before an individual label.
        This will be called for each item in the tooltip. *)
    val before_label : t -> item_cb
    val set_before_label : t -> item_cb

    (** Returns text to render for an individual item in the tooltip. *)
    val label : t -> item_cb
    val set_label : t -> item_cb -> unit

    (** Returns the colors to render for the tooltip item. *)
    val label_color : t -> label_color_cb
    val set_label_color : t -> label_color_cb -> unit

    (** Returns the colors for the text of the label for the tooltip item. *)
    val label_text_color : t -> label_text_color_cb
    val set_label_text_color : t -> label_text_color_cb -> unit

    (** Returns text to render after an individual label. *)
    val after_label : t -> item_cb
    val set_after_label : t -> item_cb

    (** Returns text to render after the body section *)
    val after_body : t -> items_cb
    val set_after_body : t -> items_cb -> unit

    (** Returns text to render before the footer section. *)
    val before_footer : t -> items_cb
    val set_before_footer : t -> items_cb -> unit

    (** Returns text to render as the footer of the tooltip. *)
    val footer : t -> items_cb
    val set_footer : t -> items_cb -> unit

    (** Text to render after the footer section. *)
    val after_footer : t -> items_cb
    val set_after_footer : t -> items_cb -> unit

    val make : ?before_title:items_cb ->
               ?title:items_cb ->
               ?after_title:items_cb ->
               ?before_body:items_cb ->
               ?before_label:item_cb ->
               ?label:item_cb ->
               ?label_color:label_color_cb ->
               ?label_text_color:label_text_color_cb ->
               ?after_label:item_cb ->
               ?after_body:items_cb ->
               ?before_footer:items_cb ->
               ?footer:items_cb ->
               ?after_footer:items_cb ->
               unit ->
               t [@@js.builder]

  end

  type custom = Model.t -> unit

  type sort = Item.t -> Item.t -> Data.t -> int

  type filter = Item.t -> Data.t -> bool

  type position =
    [ `Average [@js "average"]
    | `Nearest [@js "nearest"]
    ] [@js.enum]

  type t

  (** Are on-canvas tooltips enabled. *)
  val enabled : t -> bool
  val set_enabled : t -> bool -> unit

  (** Custom tooltip callback. *)
  val custom : t -> custom
  val set_custom : t -> custom -> unit

  (** Sets which elements appear in the tooltip. *)
  val mode : t -> interaction_mode
  val set_mode : t -> interaction_mode -> unit

  (** If true, the tooltip mode applies only when the mouse position
      intersects with an element. If false, the mode will be applied
      at all times. *)
  val intersect : t -> bool
  val set_intersect : t -> bool -> unit

  (** The mode for positioning the tooltip. *)
  val position : t -> position
  val set_position : t -> position -> unit

  (** Callbacks *)
  val callbacks : t -> Callbacks.t
  val set_callbacks : t -> Callbacks.t -> unit

  (** Sort tooltip items. *)
  val item_sort : t -> sort
  val set_item_sort : t -> sort -> unit

  (** Filter tooltip items. *)
  val filter : t -> filter
  val set_filter : t -> filter -> unit

  (** Background color of the tooltip. *)
  val background_color : t -> Color.t
  val set_background_color : t -> Color.t -> unit

  (** Title font. *)
  val title_font_family : t -> Font_family.t
  val set_title_font_family : t -> Font_family.t -> unit

  (** Title font size. *)
  val title_font_size : t -> int
  val set_title_font_size : t -> int -> unit

  (** Title font style *)
  val title_font_style : t -> Font_style.t
  val set_title_font_style : t -> Font_style.t -> unit

  (** Title font color *)
  val title_font_color : t -> Color.t
  val set_title_font_color : t -> Color.t -> unit

  (** Spacing to add to top and bottom of each title line. *)
  val title_spacing : t -> int
  val set_title_spacing : t -> int -> unit

  (** Margin to add on bottom of title section. *)
  val title_margin_bottom : t -> int
  val set_title_margin_bottom : t -> int -> unit

  (** Body line font. *)
  val body_font_family : t -> Font_family.t
  val set_body_font_family : t -> Font_family.t -> unit

  (** Body font size. *)
  val body_font_size : t -> int
  val set_body_font_size : t -> int -> unit

  (** Body font style. *)
  val body_font_style : t -> Font_style.t
  val set_body_font_style : t -> Font_style.t -> unit

  (** Body font color. *)
  val body_font_color : t -> Color.t
  val set_body_font_color : t -> Color.t -> unit

  (** Spacing to add to top and bottom of each tooltip item. *)
  val body_spacing : t -> int
  val set_body_spacing : t -> int -> unit

  (** Footer font. *)
  val footer_font_family : t -> Font_family.t
  val set_footer_font_family : t -> Font_family.t -> unit

  (** Footer font size. *)
  val footer_font_size : t -> int
  val set_footer_font_size : t -> int -> unit

  (** Footer font style. *)
  val footer_font_style : t -> Font_style.t
  val set_footer_font_style : t -> Font_style.t -> unit

  (** Footer font color. *)
  val footer_font_color : t -> Color.t
  val set_footer_font_color : t -> Color.t -> unit

  (** Spacing to add to top and bottom of each footer line. *)
  val footer_spacing : t -> int
  val set_footer_spacing : t -> int -> unit

  (** Margin to add before drawing the footer. *)
  val footer_margin_top : t -> int
  val set_footer_margin_top : t -> int -> unit

  (** Padding to add on left and right of tooltip. *)
  val x_padding : t -> int
  val set_x_padding : t -> int -> unit

  (** Padding to add on top and bottom of tooltip. *)
  val y_padding : t -> int
  val set_y_padding : t -> int -> unit

  (** Extra distance to move the end of the tooltip arrow
      away from the tooltip point. *)
  val caret_padding : t -> int
  val set_caret_padding : t -> int -> unit

  (** Size, in px, of the tooltip arrow. *)
  val caret_size : t -> int
  val set_caret_size : t -> int -> unit

  (** Radius of tooltip corner curves. *)
  val corner_radius : t -> int
  val set_corner_radius : t -> int -> unit

  (** Color to draw behind the colored boxes when multiple
      items are in the tooltip. *)
  val multi_key_background : t -> Color.t
  val set_multi_key_background : t -> Color.t -> unit

  (** If true, color boxes are shown in the tooltip. *)
  val display_colors : t -> bool
  val set_display_colors : t -> bool -> unit

  (** Color of the border. *)
  val border_color : t -> Color.t
  val set_border_color : t -> Color.t -> unit

  (** Size of the border. *)
  val border_width : t -> int
  val set_border_width : t -> int -> unit

  val make : ?enabled:bool ->
             ?custom:custom ->
             ?mode:interaction_mode ->
             ?intersect:bool ->
             ?position:position ->
             ?callbacks:Callbacks.t ->
             ?item_sort:sort ->
             ?filter:filter ->
             ?background_color:Color.t ->
             ?title_font_family:Font_family.t ->
             ?title_font_size:int ->
             ?title_font_style:Font_style.t ->
             ?title_font_color:Color.t ->
             ?title_spacing:int ->
             ?title_margin_bottom:int ->
             ?body_font_family:Font_family.t ->
             ?body_font_size:int ->
             ?body_font_style:Font_style.t ->
             ?body_font_color:Color.t ->
             ?body_spacing:int ->
             ?footer_font_family:Font_family.t ->
             ?footer_font_size:int ->
             ?footer_font_style:Font_style.t ->
             ?footer_font_color:Color.t ->
             ?footer_spacing:int ->
             ?footer_margin_top:int ->
             ?x_padding:int ->
             ?y_padding:int ->
             ?caret_padding:int ->
             ?caret_size:int ->
             ?corner_radius:int ->
             ?multi_key_background:Color.t ->
             ?display_colors:bool ->
             ?border_color:Color.t ->
             ?border_width:int ->
             unit ->
             t [@@js.builder]

end

module Hover : sig
  type axis =
    [ `X [@js "x"]
    | `Y [@js "y"]
    | `XY [@js "xy"]
    ] [@js.enum]
  type t

  (** Sets which elements appear in the tooltip. *)
  val mode : t -> interaction_mode
  val set_mode : t -> interaction_mode -> unit

  (** If true, the hover mode only applies when the mouse
      position intersects an item on the chart. *)
  val intersect : t -> bool
  val set_intersect : t -> bool -> unit

  (** Can be set to 'x', 'y', or 'xy' to define which directions
      are used in calculating distances. Defaults to 'x' for index
      mode and 'xy' in dataset and nearest modes. *)
  val axis : t -> axis
  val set_axis : t -> axis -> unit

  (** Duration in milliseconds it takes to animate hover style changes. *)
  val animation_duration : t -> int
  val set_animation_duration : t -> int -> unit

  val make : ?mode:interaction_mode ->
             ?intersect:bool ->
             ?axis:axis ->
             ?animation_duration:int ->
             unit ->
             t [@@js.builder]

end

module Elements : sig

  module Point : sig
    type t

    (** Point radius. *)
    val radius : t -> int
    val set_radius : t -> int -> unit

    (** Point style. *)
    val point_style : t -> point_style
    val set_point_style : t -> point_style -> unit

    (** Point rotation (in degrees). *)
    val rotation : t -> int
    val set_rotation : t -> int -> unit

    (** Point fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Point stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

    (** Point stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Extra radius added to point radius for hit detection. *)
    val hit_radius : t -> int
    val set_hit_radius : t -> int -> unit

    (** Point radius when hovered. *)
    val hover_radius : t -> int
    val set_hover_radius : t -> int -> unit

    (** Stroke width when hovered. *)
    val hover_border_width : t -> int
    val set_hover_border_width : t -> int -> unit

    val make : ?radius:int ->
               ?point_style:point_style ->
               ?rotation:int ->
               ?background_color:Color.t ->
               ?border_width:int ->
               ?border_color:Color.t ->
               ?hit_radius:int ->
               ?hover_radius:int ->
               ?hover_border_width:int ->
               unit ->
               t [@@js.builder]

  end

  module Line : sig
    type t

    type fill =
      [ `Zero
      | `Top
      | `Bottom
      | `Bool of bool (* true -> 'zero', false -> disabled *)
      ] [@js.union]
    val fill_to_js : fill -> Ojs.t
      [@@js.custom
       let fill_to_js : fill -> Ojs.t = function
         | `Zero -> Ojs.string_to_js "zero"
         | `Top -> Ojs.string_to_js "top"
         | `Bottom -> Ojs.string_to_js "bottom"
         | `Bool x -> Ojs.bool_to_js x
      ]
    val fill_of_js : Ojs.t -> fill
      [@@js.custom
       let fill_of_js (js : Ojs.t) : fill =
         match Ojs.obj_type js with
         | "[object Boolean]" -> `Bool (Ojs.bool_of_js js)
         | "[object String]" ->
            begin match Ojs.string_of_js js with
            | "zero" -> `Zero
            | "top" -> `Top
            | "bottom" -> `Bottom
            | _ -> assert false
            end
         | _ -> assert false
      ]

    (** Bézier curve tension (0 for no Bézier curves). *)
    val tension : t -> float
    val set_tension : t -> float -> unit

    (** Line fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Line stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

    (** Line stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Line cap style. *)
    val border_cap_style : t -> line_cap
    val set_border_cap_style : t -> line_cap -> unit

    (** Line dash. *)
    val border_dash : t -> border_dash
    val set_border_dash : t -> border_dash -> unit

    (** Line dash offset. *)
    val border_dash_offset : t -> border_dash_offset
    val set_border_dash_offset : t -> border_dash_offset -> unit

    (** Line join style. *)
    val border_join_style : t -> line_join
    val set_border_join_style : t -> line_join -> unit

    (** 'true' to keep Bézier control inside the chart,
        'false' for no restriction.*)
    val cap_bezier_points : t -> bool
    val set_cap_bezier_points : t -> bool -> unit

    (** Fill location: 'zero', 'top', 'bottom', true (eq. 'zero')
        or false (no fill). *)
    val fill : t -> fill
    val set_fill : t -> fill -> unit

    (** 'true' to show the line as a stepped line
        (tension will be ignored). *)
    val stepped : t -> bool
    val set_stepped : t -> bool -> unit

    val make : ?tension:float ->
               ?background_color:Color.t ->
               ?border_width:int ->
               ?border_color:Color.t ->
               ?border_cap_style:line_cap ->
               ?border_dash:border_dash ->
               ?border_dash_offset:border_dash_offset ->
               ?border_join_style:line_join ->
               ?cap_bezier_points:bool ->
               ?fill:fill ->
               ?stepped:bool ->
               unit ->
               t [@@js.builder]

  end

  module Rectangle : sig
    type t

    type border_skipped =
      [ `Bottom [@js "bottom"]
      | `Left [@js "left"]
      | `Top [@js "top"]
      | `Right [@js "right"]
      ] [@js.enum]

    (** Bar fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Bar stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

    (** Bar stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Skipped (excluded) border: 'bottom', 'left', 'top' or 'right'. *)
    val border_skipped : t -> border_skipped
    val set_border_skipped : t -> border_skipped -> unit

    val make : ?background_color:Color.t ->
               ?border_width:int ->
               ?border_color:Color.t ->
               ?border_skipped:border_skipped ->
               unit ->
               t [@@js.builder]

  end

  module Arc : sig
    type t

    (** Arc fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Arc stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Arc stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

  end

  type t

  (** Point elements are used to represent the points
      in a line chart or a bubble chart. *)
  val point : t -> Point.t
  val set_point : t -> Point.t -> unit

  (** Line elements are used to represent the line in a line chart. *)
  val line : t -> Line.t
  val set_line : t -> Line.t -> unit

  (** Rectangle elements are used to represent the bars in a bar chart. *)
  val rectangle : t -> Rectangle.t
  val set_rectangle : t -> Rectangle.t -> unit

  (** Arcs are used in the polar area, doughnut and pie charts. *)
  val arc : t -> Arc.t
  val set_arc : t -> Arc.t -> unit

  val make : ?point:Point.t ->
             ?line:Line.t ->
             ?rectangle:Rectangle.t ->
             ?arc:Arc.t ->
             unit ->
             t [@@js.builder]
end

module Plugins : sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val make : unit -> t [@@js.builder]
end

(** The configuration is used to change how the chart behaves.
    There are properties to control styling, fonts, the legend, etc. *)
type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

(** Chart.js animates charts out of the box.
    A number of options are provided to configure how the animation
    looks and how long it takes *)
val animation : t -> Animation.t
val set_animation : t -> Animation.t -> unit

(** Layout configurations *)
val layout : t -> Layout.t
val set_layout : t -> Layout.t -> unit

(** The chart legend displays data about the datasets
    that are appearing on the chart. *)
val legend : t -> Legend.t
val set_legend : t -> Legend.t -> unit

(** The chart title defines text to draw at the top of the chart. *)
val title : t -> Title.t
val set_title : t -> Title.t -> unit

(** While chart types provide settings to configure the styling
    of each dataset, you sometimes want to style all datasets the same way.
    A common example would be to stroke all of the bars in a bar chart with
    the same colour but change the fill per dataset.
    Options can be configured for four different types of elements: arc, lines,
    points, and rectangles. When set, these options apply to all objects
    of that type unless specifically overridden by the configuration attached
    to a dataset.*)
val elements : t -> Elements.t
val set_elements : t -> Elements.t -> unit

val plugins : t -> Plugins.t
val set_plugins : t -> Plugins.t -> unit

type legend_callback = Types.t -> string

val legend_callback : t -> legend_callback
val set_legend_callback : t -> legend_callback -> unit

(** Resizes the chart canvas when its container does. *)
val responsive : t -> bool
val set_responsive : t -> bool -> unit

(** Duration in milliseconds it takes to animate
    to new size after a resize event. *)
val responsive_animation_duration : t -> int
val set_responsive_animation_duration : t -> int -> unit

(** Maintain the original canvas aspect ratio (width / height) when resizing. *)
val maintain_aspect_ratio : t -> bool
val set_maintain_aspect_ratio : t -> bool -> unit

(** Canvas aspect ratio (i.e. width / height, a value of 1
    representing a square canvas). Note that this option is
    ignored if the height is explicitly defined either as
    attribute or via the style. *)
val aspect_ratio : t -> float
val set_aspect_ratio : t -> float -> unit

type size =
  { width : int
  ; height : int
  }
type resize_cb = Types.t -> size -> unit

(** Called when a resize occurs. Gets passed two arguments:
    the chart instance and the new size. *)
val on_resize : t -> resize_cb
val set_on_resize : t -> resize_cb -> unit

(** Override the window's default devicePixelRatio. *)
val device_pixel_ratio : t -> float
val set_device_pixel_ratio : t -> float -> unit

type event =
  [ `Mousemove [@js "mousemove"]
  | `Mouseout [@js "mouseout"]
  | `Click [@js "click"]
  | `Touchstart [@js "touchstart"]
  | `Touchmove [@js "touchmove"]
  | `Touchend [@js "touchend"]
  ] [@js.enum]

(* TODO unwrap elts array from Ojs.t to OCaml type *)
type interaction_cb = event:Ojs.t -> elts:Ojs.t -> unit

(** The events option defines the browser events that
    the chart should listen to for tooltips and hovering. *)
val events : t -> event list
val set_events : t -> event list -> unit

(** Called when any of the events fire.
    Called in the context of the chart and passed the event
    and an array of active elements (bars, points, etc). *)
val on_hover : t -> interaction_cb
val set_on_hover : t -> interaction_cb -> unit

(** Called if the event is of type 'mouseup' or 'click'.
    Called in the context of the chart and passed the event
    and an array of active elements. *)
val on_click : t -> interaction_cb
val set_on_click : t -> interaction_cb -> unit

val make : ?elements:Elements.t ->
           ?animation:Animation.t ->
           ?layout:Layout.t ->
           ?legend:Legend.t ->
           ?title:Title.t ->
           ?tooltips:Tooltips.t ->
           ?hover:Hover.t ->
           ?plugins:Plugins.t ->
           ?scales:Scales.scales ->
           ?legend_callback:legend_callback ->
           ?responsive:bool ->
           ?responsive_animation_duration:int ->
           ?maintain_aspect_ratio:bool ->
           ?aspect_ratio:float ->
           ?on_resize:resize_cb ->
           ?device_pixel_ratio:float ->
           ?events:event list ->
           ?on_hover:interaction_cb ->
           ?on_click:interaction_cb ->
           unit ->
           t [@@js.builder]
