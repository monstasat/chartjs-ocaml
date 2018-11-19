open Chartjs_types

module Dataset : sig
  type t

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

  (** For a pie chart, datasets need to contain an array of data points.
      The data points should be a number, Chart.js will total all of the
      numbers and calculate the relative proportion of each. *)
  val data : t -> Chartjs_array.Float.t
  val set_data : t -> float list -> unit

  val make : ?background_color:Color.t list ->
             ?border_color:Color.t list ->
             ?border_width:int list ->
             ?hover_background_color:Color.t list ->
             ?hover_border_color:Color.t list ->
             ?hover_border_width:int list ->
             ?data:float list ->
             unit ->
             t [@@js.builder]

end

module Data : sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val datasets : t -> Dataset.t Chartjs_array.t
  val set_datasets : t -> Dataset.t list -> unit

  val labels : t -> Chartjs_array.String.t
  val set_labels : t -> string list -> unit

  val make : ?datasets:Dataset.t list ->
             ?labels:string list ->
             unit ->
             t [@@js.builder]

end

val data : Chart.t -> Data.t
val set_data : Chart.t -> Data.t -> unit
