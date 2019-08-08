open Js_of_ocaml
open Types

module Typ = struct
  type t = Js.js_string Js.t

  let line = Js.string "line"
  let bar = Js.string "bar"
  let horizontal_bar = Js.string "horizontalBar"
  let radar = Js.string "radar"
  let doughnut = Js.string "doughnut"
  let pie = Js.string "pie"
  let polar_area = Js.string "polarArea"
  let bubble = Js.string "bubble"
  let scatter = Js.string "scatter"

  let of_string s = Js.string s
  let to_string t = Js.to_string t
end

type ('a, 'b, 'c) tooltip_cb =
  ('a, ('b -> 'c -> Js.js_string Js.t Indexable.t Js.t))
    Js.meth_callback Js.optdef

(** {1 Chart configuration} *)

class type animationItem = object
  method chart : t Js.t Js.prop
  (** Chart object. *)

  method currentStep : float Js.prop
  (** Current Animation frame number. *)

  method numSteps : float Js.prop
  (** Number of animation frames. *)

  method render : t Js.t -> animationItem Js.t -> unit Js.meth
  (** Function that renders the chart. *)

  method onAnimationProgress : (animationItem Js.t -> unit) Js.callback Js.prop

  method onAnimationComplete : (animationItem Js.t -> unit) Js.callback Js.meth
end

and animation = object

  method duration : int Js.prop
  (** The number of milliseconds an animation takes. *)

  method easing : Easing.t Js.prop
  (** Easing function to use. *)

  method onProgress : (animationItem Js.t -> unit) Js.callback Js.opt Js.prop
  (** Callback called on each step of an animation. *)

  method onComplete : (animationItem Js.t -> unit) Js.callback Js.opt Js.prop
  (** Callback called at the end of an animation. *)

end

and layout = object
  method padding : Padding.t Js.prop
  (** The padding to add inside the chart. *)
end

and legendItem = object
  method text : Js.js_string Js.t Js.prop
  (** Label that will be displayed. *)

  method fillStyle : Color.t Js.prop
  (** Fill style of the legend box. *)

  method hidden : bool Js.t Js.prop
  (** If true, this item represents a hidden dataset.
      Label will be rendered with a strike-through effect. *)

  method lineCap : Line_cap.t Js.prop
  (** For box border. *)

  method lineDash : line_dash Js.prop
  (** For box border. *)

  method lineDashOffset : line_dash_offset Js.prop
  (** For box border. *)

  method lineJoin : Line_join.t Js.prop
  (** For box border. *)

  method lineWidth : int Js.prop
  (** Width of box border. *)

  method strokeStyle : Color.t Js.prop
  (** Stroke style of the legend box. *)

  method pointStyle : Js.js_string Js.t Js.prop
  (** Point style of the legend box (only used if usePointStyle is true) *)

  method datasetIndex : int Js.prop
end

and legendLabels = object
  method boxWidth : int Js.prop
  (** Width of coloured box. *)

  method fontSize : int Js.prop
  (** Font size of text. *)

  method fontStyle : Js.js_string Js.prop
  (** Font style of text. *)

  method fontColor : Color.t Js.prop
  (** Color of text. *)

  method fontFamily : Js.js_string Js.prop
  (** Font family of legend text. *)

  method padding : int Js.prop
  (** Padding between rows of colored boxes. *)

  method generateLabels : (t Js.t -> legendItem Js.t Js.js_array Js.t)
      Js.callback Js.prop
  (** Generates legend items for each thing in the legend.
      Default implementation returns the text + styling for the color box. *)

  method filter : (legendItem Js.t -> data Js.t -> bool Js.t) Js.callback Js.prop
  (** Filters legend items out of the legend. Receives 2 parameters,
      a Legend Item and the chart data. *)

  method usePointStyle : bool Js.t Js.prop
  (** Label style will match corresponding point style
      (size is based on fontSize, boxWidth is not used in this case). *)
end

and legend = object
  method display : bool Js.t Js.prop
  (** Is the legend shown. *)

  method position : Position.t Js.prop
  (** Position of the legend. *)

  method fullWidth : bool Js.t Js.prop
  (** Marks that this box should take the full width of the canvas
      (pushing down other boxes). This is unlikely to need to be changed
      in day-to-day use. *)

  method onClick :
    (Dom_html.event Js.t
     -> legendItem Js.t
     -> unit) Js.callback Js.prop
  (** A callback that is called when a click event is
      registered on a label item *)

  method onHover :
    (Dom_html.event Js.t
     -> legendItem Js.t
     -> unit) Js.callback Js.prop
  (** A callback that is called when a 'mousemove' event is
      registered on top of a label item *)

  method onLeave :
    (Dom_html.event Js.t
     -> legendItem Js.t
     -> unit) Js.callback Js.prop

  method reverse : bool Js.t Js.prop
  (** Legend will show datasets in reverse order. *)

  method labels : legendLabels Js.t Js.prop
  (** Legend label configuration. *)
end

and title = object
  method display : bool Js.t Js.prop
  (** Is the title shown. *)

  method position : Position.t Js.prop
  (** Position of title. *)

  method fontSize : int Js.optdef Js.prop
  (** Font size. *)

  method fontFamily : Js.js_string Js.t Js.prop
  (** Font family for the title text. *)

  method fontColor : Js.js_string Js.prop
  (** Font color. *)

  method fontStyle : Js.js_string Js.prop
  (** Font style. *)

  method padding : int Js.prop
  (** Number of pixels to add above and below the title text. *)

  method lineHeight : Line_height.t Js.prop
  (** Height of an individual line of text. *)

  method text : Js.js_string Js.t Indexable.t Js.prop
  (** Title text to display. If specified as an array,
      text is rendered on multiple lines. *)
end

and tooltipItem = object
  method label : Js.js_string Js.t Js.readonly_prop
  (** Label for the tooltip. *)

  method value : Js.js_string Js.t Js.readonly_prop
  (** Value for the tooltip. *)

  method datasetIndex : int Js.readonly_prop
  (** Index of the dataset the item comes from. *)

  method index : int Js.readonly_prop
  (** Index of this data item in the dataset. *)

  method x : float Js.readonly_prop
  (** X position of matching point. *)

  method y : float Js.readonly_prop
  (** Y position of matching point. *)
end

and tooltipBodyLines = object
  method before : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  method lines : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  method after : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
end

and tooltipModel = object

  method dataPoints : tooltipItem Js.t Js.js_array Js.t Js.readonly_prop
  (** The items that we are rendering in the tooltip. *)

  (** Positioning. *)

  method xPadding : int Js.readonly_prop
  method yPadding : int Js.readonly_prop
  method xAlign : Js.js_string Js.t Js.readonly_prop
  method yAlign : Js.js_string Js.t Js.readonly_prop

  (** X and Y readonly_properties are the top left of the tooltip. *)

  method x : float Js.readonly_prop
  method y : float Js.readonly_prop
  method width : float Js.readonly_prop
  method height : float Js.readonly_prop

  (** Where the tooltip points to. *)
  method caretX : int Js.readonly_prop
  method caretY : int Js.readonly_prop

  (** Body.
      The body lines that need to be rendered.
      Each object contains 3 parameters.
      [before] - lines of text before the line with the color square
      [lines] - lines of text to render as the main item with color square
      [after] - lines of text to render after the main lines. *)

  method body : tooltipBodyLines Js.t Js.readonly_prop
  method beforeBody : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  method afterBody : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  method bodyFontColor : Color.t Js.readonly_prop
  method __bodyFontFamily : Js.js_string Js.t Js.readonly_prop
  method __bodyFontStyle : Js.js_string Js.t Js.readonly_prop
  method __bodyAlign : Js.js_string Js.t Js.readonly_prop
  method bodyFontSize : int Js.readonly_prop
  method bodySpacing : int Js.readonly_prop

  (** Title. Lines of text that form the title. *)

  method title : Js.js_string Js.t Indexable.t Js.readonly_prop
  method titleFontColor : Color.t Js.readonly_prop
  method __titleFontFamily : Js.js_string Js.t Js.readonly_prop
  method __titleFontStyle : Js.js_string Js.t Js.readonly_prop
  method titleFontSize : int Js.readonly_prop
  method __titleAlign : Js.js_string Js.t Js.readonly_prop
  method titleSpacing : int Js.readonly_prop
  method titleMarginBottom : int Js.readonly_prop

  (** Footer. Lines of text that form the footer. *)

  method footer : Js.js_string Js.t Indexable.t Js.readonly_prop
  method footerFontColor : Color.t Js.readonly_prop
  method __footerFontFamily : Js.js_string Js.t Js.readonly_prop
  method __footerFontStyle : Js.js_string Js.t Js.readonly_prop
  method footerFontSize : int Js.readonly_prop
  method __footerAlign : Js.js_string Js.t Js.readonly_prop
  method footerSpacing : int Js.readonly_prop
  method footerMarginTop : int Js.readonly_prop

  (** Appearance. *)

  method caretSize : int Js.readonly_prop
  method caretPadding : int Js.readonly_prop
  method cornerRadius : int Js.readonly_prop
  method backgroundColor : Color.t Js.readonly_prop

  method labelColors : Color.t Js.js_array Js.t Js.readonly_prop
  (** Colors to render for each item in body. This is the color of the
      squares in the tooltip. *)

  method labelTextColors : Color.t Js.js_array Js.t Js.readonly_prop

  method opacity : float Js.readonly_prop
  (** Zero opacity is a hidden tooltip. *)

  method legendColorBackground : Color.t Js.readonly_prop
  method displayColors : bool Js.t Js.readonly_prop
  method borderColor : Color.t Js.readonly_prop
  method borderWidth : int Js.readonly_prop

end

and tooltipCallbacks = object

  method beforeTitle :
    (tooltip Js.t,
     tooltipItem Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Returns the text to render before the title. *)

  method title :
    (tooltip Js.t,
     tooltipItem Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Returns the text to render as the title of the tooltip. *)

  method afterTitle :
    (tooltip Js.t,
     tooltipItem Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Returns the text to render after the title. *)

  method beforeBody :
    (tooltip Js.t,
     tooltipItem Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Returns the text to render before the body section. *)

  method beforeLabel : (tooltip Js.t, tooltipItem Js.t, data Js.t) tooltip_cb Js.prop
  (** Returns the text to render before an individual label.
      This will be called for each item in the tooltip. *)

  method label : (tooltip Js.t, tooltipItem Js.t, data Js.t) tooltip_cb Js.prop
  (** Returns the text to render for an individual item in the tooltip. *)

  method labelColor : (tooltip Js.t, tooltipItem Js.t, t Js.t) tooltip_cb Js.prop
  (** Returns the colors to render for the tooltip item. *)

  method labelTextColor : (tooltip Js.t, tooltipItem Js.t, t Js.t) tooltip_cb Js.prop
  (** Returns the colors for the text of the label for the tooltip item. *)

  method afterLabel : (tooltip Js.t, tooltipItem Js.t, data Js.t) tooltip_cb Js.prop
  (** Returns text to render after an individual label. *)

  method afterBody :
    (tooltip Js.t,
     tooltipItem Js.t Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Returns text to render after the body section. *)

  method beforeFooter :
    (tooltip Js.t,
     tooltipItem Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Returns text to render before the footer section. *)

  method footer :
    (tooltip Js.t,
     tooltipItem Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Returns text to render as the footer of the tooltip. *)

  method afterFooter :
    (tooltip Js.t,
     tooltipItem Js.js_array Js.t,
     data Js.t) tooltip_cb Js.prop
  (** Text to render after the footer section. *)

end

and tooltip = object
  method enabled : bool Js.t Js.prop
  (** Are on-canvas tooltips enabled. *)

  method custom : (tooltipModel Js.t -> unit) Js.callback Js.opt Js.prop
  (** Custom tooltip callback. *)

  method mode : Interaction_mode.t Js.prop
  (** Sets which elements appear in the tooltip. *)

  method intersect : bool Js.t Js.prop
  (** If true, the tooltip mode applies only when the mouse position
      intersects with an element. If false, the mode will be applied
      at all times. *)

  method position : Tooltip_position.t Js.prop
  (** The mode for positioning the tooltip. *)

  method callbacks : tooltipCallbacks Js.t Js.optdef Js.prop
  (** Callbacks. *)

  method itemSort : (tooltipItem Js.t
                     -> tooltipItem Js.t
                     -> data Js.t (* FIXME *)
                     -> int) Js.callback Js.optdef Js.prop
  (** Sort tooltip items. *)

  method filter : (tooltipItem Js.t
                   -> data Js.t
                   -> bool Js.t) Js.callback Js.optdef Js.prop
  (** Filter tooltip items. *)

  method backgroundColor : Color.t Js.prop
  (** Background color of the tooltip. *)

  method titleFontFamily : Js.js_string Js.t Js.prop
  (** Title font. *)

  method titleFontSize : int Js.prop
  (** Title font size. *)

  method titleFontStyle : Js.js_string Js.t Js.prop
  (** Title font style *)

  method titleFontColor : Color.t Js.prop
  (** Title font color *)

  method titleSpacing : int Js.prop
  (** Spacing to add to top and bottom of each title line. *)

  method titleMarginBottom : int Js.prop
  (** Margin to add on bottom of title section. *)

  method bodyFontFamily : Js.js_string Js.t Js.prop
  (** Body line font. *)

  method bodyFontSize : int Js.prop
  (** Body font size. *)

  method bodyFontStyle : Js.js_string Js.t Js.prop
  (** Body font style. *)

  method bodyFontColor : Color.t Js.prop
  (** Body font color. *)

  method bodySpacing : int Js.prop
  (** Spacing to add to top and bottom of each tooltip item. *)

  method footerFontFamily : Js.js_string Js.t Js.prop
  (** Footer font. *)

  method footerFontSize : int Js.prop
  (** Footer font size. *)

  method footerFontStyle : Js.js_string Js.t Js.prop
  (** Footer font style. *)

  method footerFontColor : Color.t Js.prop
  (** Footer font color. *)

  method footerSpacing : int Js.prop
  (** Spacing to add to top and bottom of each footer line. *)

  method footerMarginTop : int Js.prop
  (** Margin to add before drawing the footer. *)

  method xPadding : int Js.prop
  (** Padding to add on left and right of tooltip. *)

  method yPadding : int Js.prop
  (** Padding to add on top and bottom of tooltip. *)

  method caretPadding : int Js.prop
  (** Extra distance to move the end of the tooltip arrow
      away from the tooltip point. *)

  method caretSize : int Js.prop
  (** Size, in px, of the tooltip arrow. *)

  method cornerRadius : int Js.prop
  (** Radius of tooltip corner curves. *)

  method multyKeyBackground : Color.t Js.prop
  (** Color to draw behind the colored boxes when multiple
      items are in the tooltip. *)

  method displayColors : bool Js.t Js.prop
  (** If [true], color boxes are shown in the tooltip. *)

  method borderColor : Color.t Js.prop
  (** Color of the border. *)

  method borderWidth : int Js.prop
  (** Size of the border. *)
end

and hover = object

  method mode : Interaction_mode.t Js.prop
  (** Sets which elements appear in the tooltip. *)

  method intersect : bool Js.t Js.prop
  (** If true, the hover mode only applies when the mouse
      position intersects an item on the chart. *)

  method axis : Hover_axis.t Js.prop
  (** Defines which directions are used in calculating distances.
      Defaults to [x] for index mode and [xy] in [dataset] and [nearest]
      modes. *)

  method animationDuration : int Js.prop
  (** Duration in milliseconds it takes to animate hover style changes. *)

end

and pointElement = object

  method radius : int Js.prop
  (** Point radius. *)

  method pointStyle : Point_style.t Js.prop
  (** Point style. *)

  method rotation : int Js.prop
  (** Point rotation (in degrees). *)

  method backgroundColor : Color.t Js.prop
  (** Point fill color. *)

  method borderWidth : int Js.prop
  (** Point stroke width. *)

  method borderColor : Color.t Js.prop
  (** Point stroke color. *)

  method hitRadius : int Js.prop
  (** Extra radius added to point radius for hit detection. *)

  method hoverRadius : int Js.prop
  (** Point radius when hovered. *)

  method hoverBorderWidth : int Js.prop
  (** Stroke width when hovered. *)

end

and lineElement = object

  method tension : float Js.prop
  (** Bézier curve tension (0 for no Bézier curves). *)

  method backgroundColor : Color.t Js.prop
  (** Line fill color. *)

  method borderWidth : int Js.prop
  (** Line stroke width. *)

  method borderColor : Color.t Js.prop
  (** Line stroke color. *)

  method borderCapStyle : Line_cap.t Js.prop
  (** Line cap style. *)

  method borderDash : line_dash Js.prop
  (** Line dash. *)

  method borderDashOffset : line_dash_offset Js.prop
  (** Line dash offset. *)

  method borderJoinStyle : Line_join.t Js.prop
  (** Line join style. *)

  method capBezierPoints : bool Js.t Js.prop
  (** [true] to keep Bézier control inside the chart,
      [false] for no restriction.*)

  method fill : Fill.t Js.t Js.prop
  (** Fill location. *)

  method stepped : bool Js.t Js.prop
  (** [true] to show the line as a stepped line (tension will be ignored). *)

end

and rectangleElement = object

  method backgroundColor : Js.js_string Js.prop
  (** Bar fill color. *)

  method borderWidth : int Js.prop
  (** Bar stroke width. *)

  method borderColor : Color.t Js.prop
  (** Bar stroke color. *)

  method borderSkipped : Position.t Js.prop
  (** Skipped (excluded) border: 'bottom', 'left', 'top' or 'right'. *)

end

and arcElement = object

  method backgroundColor : Color.t Js.prop
  (** Arc fill color. *)

  method borderAlign : Js.js_string Js.t Js.prop
  (** Arc stroke alignment. *)

  method borderColor : Color.t Js.prop
  (** Arc stroke color. *)

  method borderWidth : int Js.prop
  (** Arc stroke width. *)

end

and elements = object

  method point : pointElement Js.t Js.prop
  (** Point elements are used to represent the points
      in a line chart or a bubble chart. *)

  method line : lineElement Js.t Js.prop
  (** Line elements are used to represent the line in a line chart. *)

  method rectangle : rectangleElement Js.t Js.prop
  (** Rectangle elements are used to represent the bars in a bar chart. *)

  method arc : arcElement Js.t Js.prop
  (** Arcs are used in the polar area, doughnut and pie charts. *)

end

and plugins = object

end

and chartSize = object
  method width : int Js.readonly_prop

  method height : int Js.readonly_prop
end

(** The configuration is used to change how the chart behaves.
    There are properties to control styling, fonts, the legend, etc. *)
and configuration = object

  method scales : Scales.scales Js.t Js.prop

  method animation : animation Js.t Js.prop
  (** Chart.js animates charts out of the box.
      A number of options are provided to configure how the animation
      looks and how long it takes *)

  method layout : layout Js.t Js.prop
  (** Layout configurations *)

  method legend : legend Js.t Js.prop
  (** The chart legend displays data about the datasets
      that are appearing on the chart. *)

  method title : title Js.t Js.prop
  (** The chart title defines text to draw at the top of the chart. *)

  method elements : elements Js.t Js.prop
  (** While chart types provide settings to configure the styling
      of each dataset, you sometimes want to style all datasets the same way.
      A common example would be to stroke all of the bars in a bar chart with
      the same colour but change the fill per dataset.
      Options can be configured for four different types of elements: arc, lines,
      points, and rectangles. When set, these options apply to all objects
      of that type unless specifically overridden by the configuration attached
      to a dataset. *)

  method plugins : plugins Js.t Js.prop

  method legendCallback : (t Js.t -> Js.js_string Js.t) Js.callback Js.optdef_prop
  (** Sometimes you need a very complex legend. In these cases, it makes sense
      to generate an HTML legend. Charts provide a generateLegend() method on their
      prototype that returns an HTML string for the legend.
      NOTE [legendCallback] is not called automatically and you must call
      [generateLegend] yourself in code when creating a legend using this method. *)

  method responsive : bool Js.t Js.prop
  (** Resizes the chart canvas when its container does. *)

  method responsiveAnimationDuration : int Js.prop
  (** Duration in milliseconds it takes to animate
      to new size after a resize event. *)

  method maintainAspectRatio : bool Js.t Js.prop
  (** Maintain the original canvas aspect ratio (width / height) when resizing. *)

  method aspectRatio : float Js.prop
  (** Canvas aspect ratio (i.e. width / height, a value of 1
      representing a square canvas). Note that this option is
      ignored if the height is explicitly defined either as
      attribute or via the style. *)

  method onResize : (t Js.t -> chartSize Js.t -> unit) Js.callback Js.opt Js.prop
  (** Called when a resize occurs. Gets passed two arguments:
      the chart instance and the new size. *)

  method devicePixelRatio : float Js.prop
  (** Override the window's default devicePixelRatio. *)

  method events : Js.js_string Js.t Js.js_array Js.t Js.prop
  (** The events option defines the browser events that
      the chart should listen to for tooltips and hovering. *)

  method onHover : (t Js.t, Dom_html.event Js.t
                    -> 'a Js.t Js.js_array Js.t
                    -> unit)
      Js.meth_callback Js.opt Js.prop
  (** Called when any of the events fire.
      Called in the context of the chart and passed the event
      and an array of active elements (bars, points, etc). *)

  method onClick : (t Js.t, Dom_html.event Js.t
                    -> 'a Js.t Js.js_array Js.t
                    -> unit)
      Js.meth_callback Js.opt Js.prop
  (** Called if the event is of type 'mouseup' or 'click'.
      Called in the context of the chart and passed the event
      and an array of active elements. *)

end

and dataset = object

end

and data = object

  method datasets : dataset Js.t Js.js_array Js.t Js.prop

  method labels : Js.js_string Js.t Js.js_array Js.t Js.prop

  method xLabels : Js.js_string Js.t Js.js_array Js.t Js.prop

  method yLabels : Js.js_string Js.t Js.js_array Js.t Js.prop

end

and updateConfig = object
  method duration : int Js.optdef_prop

  method lazy_ : bool Js.t Js.optdef_prop

  method easing : Easing.t Js.optdef_prop
end

and t = object
  method height : int Js.readonly_prop
  method width : int Js.readonly_prop
  method offsetX : int Js.readonly_prop
  method offsetY : int Js.readonly_prop
  method borderWidth : int Js.readonly_prop
  method animating : bool Js.t Js.readonly_prop
  method aspectRatio : float Js.readonly_prop
  method canvas : Dom_html.canvasElement Js.t Js.readonly_prop
  method ctx : Dom_html.canvasRenderingContext2D Js.t Js.readonly_prop
  method data : data Js.t Js.prop
  method options : configuration Js.t Js.prop

  method destroy : unit Js.meth

  method update : unit Js.meth

  method update_withConfig : updateConfig Js.t -> unit Js.meth

  method reset : unit Js.meth

  method render : unit Js.meth

  method render_withConfig : updateConfig Js.t -> unit Js.meth

  method stop : t Js.t Js.meth

  method resize : t Js.t Js.meth

  method clear : t Js.t Js.meth

  method toBase64Image : Js.js_string Js.t Js.meth

  method generateLegend : Js.js_string Js.t Js.meth

end

let chart_constr = Js.Unsafe.global##._Chart

let chart_from_canvas
    (typ : Typ.t)
    (data : data Js.t)
    (canvas : Dom_html.canvasElement Js.t)
    (options : #configuration Js.t) =
  let config = object%js
    val _type = typ
    val data = data
    val options = options
  end in
  new%js chart_constr canvas config

let chart_from_ctx
    (typ : Typ.t)
    (data : data Js.t)
    (ctx : Dom_html.canvasRenderingContext2D Js.t)
    (options : #configuration Js.t) =
  let config = object%js
    val _type = typ
    val data = data
    val options = options
  end in
  new%js chart_constr ctx config

let chart_from_id
    (typ : Typ.t)
    (data : data Js.t)
    (id : string)
    (options : #configuration Js.t) =
  let config = object%js
    val _type = typ
    val data = data
    val options = options
  end in
  new%js chart_constr (Js.string id) config

let make_update_config ?duration ?lazy_ ?easing () : updateConfig Js.t =
  let iter f = function None -> () | Some x -> f x in
  let (conf : updateConfig Js.t) = Js.Unsafe.obj [||] in
  iter (fun x -> conf##.duration := x) duration;
  iter (fun x -> conf##.lazy_ := Js.bool x) lazy_;
  iter (fun x -> conf##.easing := x) easing;
  conf
