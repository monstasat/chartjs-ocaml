open Js_of_ocaml
open Chartjs

module Line_mode = struct
  type t = Js.js_string

  let vertical = Js.string "vertical"

  let horizontal = Js.string "horizontal"
end

module Draw_time = struct
  type t = Js.js_string

  let afterDraw = Js.string "afterDraw"

  let afterDatasetsDraw = Js.string "afterDatasetsDraw"

  let beforeDatasetsDraw = Js.string "beforeDatasetsDraw"
end

module Position = struct
  include Position

  let center = Js.string "center"
end

class type baseAnnotation = object
  method _type : Js.js_string Js.t Js.prop
  (** Line/Box. *)

  method drawTime : Draw_time.t Js.t Js.optdef Js.prop
  (** Optional drawTime to control layering,
      overrides global [drawTime] setting. *)

  method id : Js.js_string Js.t Js.prop
  (** Optional annotation ID (must be unique). *)
end

class type label = object
  method backgroundColor : Color.t Js.t Js.prop
  (** Background color of label. *)

  method fontFamily : Js.js_string Js.t Js.prop
  (** Font family of text. *)

  method fontSize : int Js.prop
  (** Font size of text. *)

  method fontStyle : Js.js_string Js.t Js.prop
  (** Font style of text. *)

  method fontColor : Js.js_string Js.t Js.prop
  (** Font color of text. *)

  method xPadding : int Js.prop
  (** Padding of label to add left/right. *)

  method yPadding : int Js.prop
  (** Padding of label to add top/bottom. *)

  method cornerRadius : int Js.prop
  (** Radius of label rectangle. *)

  method position : Position.t Js.t Js.prop
  (** Anchor position of label on line. *)

  method xAdjust : int Js.prop
  (** Adjustment along y-axis (top-bottom) of label relative to above
      number (can be negative).
		  For vertical lines positioned top or bottom, negative values move
		  the label toward the edge, and positive values toward the center. *)

  method yAdjust : int Js.prop
  (** Adjustment along y-axis (top-bottom) of label relative to above
      number (can be negative).
		  For vertical lines positioned top or bottom, negative values move
		  the label toward the edge, and positive values toward the center. *)

  method enabled : bool Js.t Js.prop
  (** Whether the label is enabled and should be displayed. *)

  method content : Js.js_string Js.t Indexable.t Js.t Js.opt Js.prop
  (** Text to display in label - default is [null].
      Provide an array to display values on a new line. *)
end

class type ['a] lineAnnotation = object
  inherit baseAnnotation

  method mode : Line_mode.t Js.t Js.prop
  (** Vertical/horizontal line. *)

  method scaleID : Js.js_string Js.t Js.prop
  (** ID of the scale to bind onto. *)

  method value : 'a Js.prop
  (** Data value to draw the line at. *)

  method endValue : 'a Js.prop
  (** Optional value at which the line draw should end. *)

  method borderColor : Color.t Js.t Js.prop
  (** Line color. *)

  method borderWidth : int Js.prop
  (** Line width. *)

  method borderDash : line_dash Js.prop
  (** Line dash.
	    @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setLineDash> *)

  method borderDashOffset : line_dash_offset Js.prop
  (** Line Dash Offset
	    @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineDashOffset> *)

  method label : label Js.t Js.prop
end

class type ['a, 'b] boxAnnotation = object
  inherit baseAnnotation

  method xScaleID : Js.js_string Js.t Js.prop
  (** ID of the X scale to bind onto. *)

  method yScaleID : Js.js_string Js.t Js.prop
  (** ID of the Y scale to bind onto. *)

  method xMin : 'a Js.optdef Js.prop
  (** Left edge of the box. In units along the x axis. *)

  method xMax : 'a Js.optdef Js.prop
  (** Right edge of the box. *)

  method yMin : 'b Js.optdef Js.prop

  method yMax : 'b Js.optdef Js.prop

  method borderColor : Color.t Js.t Js.prop
  (** Stroke color. *)

  method borderWidth : int Js.prop
  (** Stroke width. *)

  method backgroundColor : Color.t Js.t Js.prop
  (** Fill color. *)
end

class type annotation = object
  method drawTime : Draw_time.t Js.t Js.prop
  (** Defines when the annotations are drawn.
			This allows positioning of the annotation relative to the other
			elements of the graph.

			@see <http://www.chartjs.org/docs/#advanced-usage-creating-plugins> *)

  method events : Js.js_string Js.t Js.js_array Js.t Js.prop
  (** Mouse events to enable on each annotation.
			Should be an array of one or more browser-supported mouse events
			@see <https://developer.mozilla.org/en-US/docs/Web/Events> *)

  method dblClickSpeed : float Js.prop
  (** Double-click speed in ms used to distinguish single-clicks from
			double-clicks whenever you need to capture both. When listening for
			both click and dblclick, click events will be delayed by this
			amount. *)

  method annotations : #baseAnnotation Js.t Js.js_array Js.t Js.prop
  (** Array of annotation configuration objects. *)
end

module CoerceTo = struct
  let line (x : #baseAnnotation Js.t) : 'a lineAnnotation Js.t Js.opt =
    if Js.string "line" == x##._type
    then Js.some (Js.Unsafe.coerce x)
    else Js.null

  let box (x : #baseAnnotation Js.t) : ('a, 'b) boxAnnotation Js.t Js.opt =
    if Js.string "box" == x##._type
    then Js.some (Js.Unsafe.coerce x)
    else Js.null
end

let coerce a : baseAnnotation Js.t = (a :> baseAnnotation Js.t)

let create_label () : label Js.t = Js.Unsafe.obj [||]

let create_box () =
  let (obj : ('a, 'b) boxAnnotation Js.t) = Js.Unsafe.obj [||] in
  obj##._type := Js.string "box";
  obj

let create_line () =
  let (obj : 'a lineAnnotation Js.t) = Js.Unsafe.obj [||] in
  obj##._type := Js.string "line";
  obj

let create () : annotation Js.t = Js.Unsafe.obj [||]
