type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

[@@@js.stop]
val axis_type : Chartjs.Scales.typ
[@@@js.start]
[@@@js.implem let axis_type : Chartjs.Scales.typ = `Custom "realtime"]

type callback = Chartjs.Types.t -> unit

(** Duration of the chart in milliseconds
    (how much time of data it will show). *)
val duration : t -> int
val set_duration : t -> int -> unit

(** Duration of the data to be kept in milliseconds.
    If not set, old data will be automatically deleted as
    it disappears off the chart. *)
val ttl : t -> int
val set_ttl : t -> int -> unit

(** Delay added to the chart in milliseconds so that upcoming
    values are known before lines are plotted. This makes the
    chart look like a continual stream rather than very jumpy
    on the right hand side. Specify the maximum expected delay. *)
val delay : t -> int
val set_delay : t -> int -> unit

(** Refresh interval of data in milliseconds.
    `onRefresh` callback function will be called at this interval. *)
val refresh : t -> int
val set_refresh : t -> int -> unit

(** Callback function that will be called at a regular interval.
    The callback takes one argument, a reference to the chart object.
    You can update your datasets here. The chart will be automatically
    updated after returning. *)
val on_refresh : t -> callback
val set_on_refresh : t -> callback -> unit

(** Frequency at which the chart is drawn on a display (frames per second).
    This option can be set at chart level but not at axis level.
    Decrease this value to save CPU power. *)
val frame_rate : t -> float
val set_frame_rate : t -> float -> unit

(** If set to `true`, scrolling stops.
    Note that `onRefresh` callback is called even when this is set to true. *)
val pause : t -> bool
val set_pause : t -> bool -> unit

val make : ?duration:int ->
           ?ttl:int ->
           ?delay:int ->
           ?refresh:int ->
           ?on_refresh:callback ->
           ?frame_rate:float ->
           ?pause:bool ->
           unit ->
           t [@@js.builder]

module Per_axis : sig
  val get : Chartjs.Scales.t ->
            t [@@js.get "realtime"]
  val set : Chartjs.Scales.t -> t ->
            unit [@@js.set "realtime"]
end

module Per_chart : sig
  val get : Chartjs.Options.Plugins.t ->
            t [@@js.get "streaming"]
  val set : Chartjs.Options.Plugins.t ->
            t Chartjs.Types.or_false ->
            unit [@@js.set "streaming"]
end
