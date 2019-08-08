open Js_of_ocaml

(** See https://developer.mozilla.org/en/docs/Web/API/CanvasRenderingContext2D/lineCap *)
module Line_cap : sig
  type t
  val butt : t
  val round : t
  val square : t
  module Unsafe : sig
    val make : string -> t
  end
end

(** See https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineJoin *)
module Line_join : sig
  type t
  val round : t
  val bevel : t
  val miter : t
  module Unsafe : sig
    val make : string -> t
  end
end

(** When configuring interaction with the graph via hover or tooltips,
    a number of different modes are available. *)
module Interaction_mode : sig
  type t

  val point : t
  (** Finds all of the items that intersect the point. *)

  val nearest : t
  (** Gets the items that are at the nearest distance to the point.
      The nearest item is determined based on the distance to the center
      of the chart item (point, bar). You can use the [axis] setting to define
      which directions are used in distance calculation.
      If [intersect] is [true], this is only triggered when the mouse position
      intersects an item in the graph. This is very useful for combo charts
      where points are hidden behind bars. *)

  val index : t
  (** Finds item at the same index. If the [intersect] setting is [true],
      the first intersecting item is used to determine the index in the data.
      If [intersect] false the nearest item, in the x direction,
      is used to determine the index.
      To use index mode in a chart like the horizontal bar chart,
      where we search along the y direction, you can use the [axis] setting
      introduced in v2.7.0. By setting this value to [y] on the y
      direction is used. *)

  val dataset : t
  (** Finds items in the same dataset. If the [intersect] setting is [true],
      the first intersecting item is used to determine the index in the data.
      If [intersect] false the nearest item is used to determine the index.*)

  val x : t
  (** Returns all items that would intersect based on the [X] coordinate of
      the position only. Would be useful for a vertical cursor implementation.
      Note that this only applies to cartesian charts. *)

  val y : t
  (** Returns all items that would intersect based on the Y coordinate of
      the position. This would be useful for a horizontal cursor implementation.
      Note that this only applies to cartesian charts. *)

  module Unsafe : sig
    val make : string -> t
  end
end

module Point_style : sig
  type t
  val circle : t
  val cross : t
  val crossRot : t
  val dash : t
  val line : t
  val rect : t
  val rectRounded : t
  val rectRot : t
  val star : t
  val triangle : t

  module Unsafe : sig
    val make : string -> t
  end
end

(** See https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setLineDash *)
type line_dash = float Js.js_array Js.t

(** See https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineDashOffset *)
type line_dash_offset = float

(** A config object can be provided with additional configuration
    for the update process. This is useful when update is manually
    called inside an event handler and some different animation is
    desired. *)
class type updateConfig = object

  method duration : int Js.optdef_prop
  (** Time for the animation of the redraw in milliseconds. *)

  method lazy_ : bool Js.t Js.optdef_prop
  (** If [true], the animation can be interrupted by other animations. *)

  method easing : unit Js.optdef_prop
  (** The animation easing function. *)
end

val make_update_config :
  ?duration:int
  -> ?lazy_:bool
  -> ?easing:unit
  -> unit
  -> updateConfig Js.t

class type t = object
  method height : int Js.readonly_prop
  method width : int Js.readonly_prop
  method offsetX : int Js.readonly_prop
  method offsetY : int Js.readonly_prop
  method borderWidth : int Js.readonly_prop
  method animating : bool Js.t Js.readonly_prop
  method aspectRatio : float Js.readonly_prop
  method canvas : Dom_html.canvasElement Js.t Js.readonly_prop
  method ctx : Dom_html.canvasRenderingContext2D Js.t Js.readonly_prop

  method destroy : unit Js.meth
  (** Use this to destroy any chart instances that are created.
      This will clean up any references stored to the chart object
      within Chart.js, along with any associated event listeners
      attached by Chart.js. This must be called before the canvas
      is reused for a new chart. *)

  method update : unit Js.meth
  (** Triggers an update of the chart.
      This can be safely called after updating the data object.
      This will update all scales, legends, and then re-render the chart. *)

  method update_withConfig : updateConfig Js.t -> unit Js.meth
  (** Same as [update], but with update config provided *)

  method reset : unit Js.meth
  (** Reset the chart to it's state before the initial animation.
      A new animation can then be triggered using update. *)

  method render : unit Js.meth
  (** Triggers a redraw of all chart elements.
      NOTE this does not update elements for new data.
      Use .update() in that case. *)

  method render_withConfig : updateConfig Js.t -> unit Js.meth
  (** Same as [render], but with update config provided *)

  method stop : t Js.t Js.meth
  (** Use this to stop any current animation loop.
      This will pause the chart during any current animation frame.
      Call .render() to re-animate. *)

  method resize : t Js.t Js.meth
  (** Use this to manually resize the canvas element.
      This is run each time the canvas container is resized,
      but you can call this method manually if you change the size of
      the canvas nodes container element. *)

  method clear : t Js.t Js.meth
  (** Will clear the chart canvas. Used extensively internally between
      animation frames, but you might find it useful. *)

  method toBase64Image : Js.js_string Js.t Js.meth
  (** This returns a base 64 encoded string of the chart
      in it's current state. *)

  method generateLegend : Js.js_string Js.t Js.meth
  (** Returns an HTML string of a legend for that chart.
      The legend is generated from the legendCallback in the options. *)

end
