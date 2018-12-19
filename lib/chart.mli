open Types

[@@@js.stop]
type 'a node =
  [ `Canvas of 'a
  | `Id of string
  ]
val node_to_js : ('a -> Ojs.t) -> 'a node -> Ojs.t
[@@@js.start]
[@@@js.implem
 type 'a node =
   [ `Canvas of 'a
   | `Id of string
   ]
 let node_to_js (f : 'a -> Ojs.t) (x : 'a node) : Ojs.t =
   match x with
   | `Id id -> Ojs.string_to_js id
   | `Canvas x -> f x
]

type config
val make_config : ?data:Data.t ->
                  ?options:Options.t ->
                  ?type_:Types.typ ->
                  unit ->
                  config [@@js.builder]

(** Create new chart instance *)
[@@@js.stop]
val new_chart : 'a node -> config -> t
[@@@js.start]
[@@@js.implem
 val new_chart' : Ojs.t -> config -> t [@@js.new "Chart"]
 let new_chart (node : 'a node) (config : config) : t =
   new_chart' (node_to_js Obj.magic node) config
]

module API : sig

  val height : t -> int
  val width : t -> int
  val offset_x : t -> int
  val offset_y : t -> int
  val border_width : t -> int
  val animating : t -> bool
  val aspect_ratio : t -> float
  val canvas : t -> Ojs.t
  val ctx : t -> Ojs.t

  val data : t -> Data.t
  val set_data : t -> Data.t -> unit

  val options : t -> Options.t
  val set_options : t -> Options.t -> unit

  (** TODO add
    'get_element_at_event',
    'get_elements_at_event',
    'get_dataset_meta' *)

  (** A config object can be provided with additional configuration
      for the update process. This is useful when update is manually
      called inside an event handler and some different animation is
      desired. *)
  type config
  val config_to_js : config -> Ojs.t
  val config_of_js : Ojs.t -> config

  val make_config :
    (** Time for the animation of the redraw in milliseconds. *)
    ?duration:int ->
    (** If true, the animation can be interrupted by other animations. *)
    ?lazy_:bool ->
    (** The animation easing function. *)
    ?easing:easing ->
    unit ->
    config [@@js.builder]

  (** Use this to destroy any chart instances that are created.
      This will clean up any references stored to the chart object
      within Chart.js, along with any associated event listeners
      attached by Chart.js. This must be called before the canvas
      is reused for a new chart. *)
  val destroy : t -> unit [@@js.call]
  (** Triggers an update of the chart.
      This can be safely called after updating the data object.
      This will update all scales, legends, and then re-render the chart. *)
  val update : t -> config option -> unit [@@js.call]
  (** Reset the chart to it's state before the initial animation.
      A new animation can then be triggered using update. *)
  val reset : t -> unit [@@js.call]
  (** Triggers a redraw of all chart elements.
      Note, this does not update elements for new data.
      Use .update() in that case. *)
  val render : t -> config option -> unit [@@js.call]
  (** Use this to stop any current animation loop.
      This will pause the chart during any current animation frame.
      Call .render() to re-animate. *)
  val stop : t -> t [@@js.call]
  (** Use this to manually resize the canvas element.
      This is run each time the canvas container is resized,
      but you can call this method manually if you change the size of
      the canvas nodes container element. *)
  val resize : t -> t [@@js.call]
  (** Will clear the chart canvas. Used extensively internally between
      animation frames, but you might find it useful. *)
  val clear : t -> t [@@js.call]
  (** This returns a base 64 encoded string of the chart
      in it's current state. *)
  val to_base64_image : t -> string [@@js.call]
  (** Returns an HTML string of a legend for that chart.
      The legend is generated from the legendCallback in the options. *)
  val generate_legend : t -> string [@@js.call]

end
