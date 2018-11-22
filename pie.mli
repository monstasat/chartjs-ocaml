open Chartjs_types

module Options : sig
  type t = Chartjs_options.t

  module Animation : sig
    type t = Chartjs_options.Animation.t

    (** If true, the chart will animate in with a rotation animation.
        This property is in the options.animation object.*)
    val animate_rotate : t -> bool
    val set_animate_rotate : t -> bool -> unit

    (** If true, will animate scaling the chart from the center outwards. *)
    val animate_scale : t -> bool
    val set_animate_scale : t -> bool -> unit

  end

  (** The percentage of the chart that is cut out of the middle. *)
  val cutout_percentage : t -> float
  val set_cutout_percentage : t -> float -> unit

  (** Starting angle to draw arcs from. *)
  val rotation : t -> float
  val set_rotation : t -> float -> unit

  (** Sweep to allow arcs to cover. *)
  val circumference : t -> float
  val set_circumference : t -> float -> unit

end

module Dataset : sig

  type t = Chartjs_data.Dataset.t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val type_ : t -> typ
  val set_type : t -> typ -> unit

  (** The fill color of the arcs in the dataset. *)
  val background_color : t -> Chartjs_array.Color.t
  val set_background_color : t -> Color.t list -> unit

  (** The border color of the arcs in the dataset. *)
  val border_color : t -> Chartjs_array.Color.t
  val set_border_color : t -> Color.t list -> unit

  (** The border width of the arcs in the dataset. *)
  val border_width : t -> Chartjs_array.Int.t
  val set_border_width : t -> int list -> unit

  (** The fill colour of the arcs when hovered. *)
  val hover_background_color : t -> Chartjs_array.Color.t
  val set_hover_background_color : t -> Color.t list -> unit

  (** The stroke colour of the arcs when hovered. *)
  val hover_border_color : t -> Chartjs_array.Color.t
  val set_hover_border_color : t -> Color.t list -> unit

  (** The stroke width of the arcs when hovered. *)
  val hover_border_width : t -> Chartjs_array.Int.t
  val set_hover_border_width : t -> int list -> unit

  val make : ?type_:typ ->
             ?background_color:Color.t list ->
             ?border_color:Color.t list ->
             ?border_width:int list ->
             ?hover_background_color:Color.t list ->
             ?hover_border_color:Color.t list ->
             ?hover_border_width:int list ->
             ?data:Ojs.t ->
             unit ->
             t [@@js.builder]

end
