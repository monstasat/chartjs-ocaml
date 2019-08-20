open Js_of_ocaml

module Axis = struct
  let realtime = Chartjs.Axis.make "realtime"
end

class type updateConfig = object
  inherit Chartjs.updateConfig

  method preservation : bool Js.t Js.optdef_prop
end

class type streaming = object
  method duration : int Js.optdef_prop

  method ttl : int Js.optdef Js.prop

  method delay : int Js.optdef_prop

  method refresh : int Js.optdef_prop

  method onRefresh : (Chartjs.chart Js.t -> unit) Js.callback Js.opt Js.optdef_prop

  method frameRate : float Js.optdef_prop

  method pause : bool Js.t Js.optdef_prop
end

let create_update_config ?preservation () =
  let (obj : updateConfig Js.t) = Js.Unsafe.obj [||] in
  match preservation with
  | None -> obj
  | Some p -> obj##.preservation := Js.bool p; obj

let create ?duration ?ttl ?delay ?refresh ?onRefresh ?frameRate ?pause () =
  let iter f = function None -> () | Some x -> f x in
  let (obj : streaming Js.t) = Js.Unsafe.obj [||] in
  iter (fun x -> obj##.duration := x) duration;
  iter (fun x -> obj##.ttl := Js.def x) ttl;
  iter (fun x -> obj##.delay := x) delay;
  iter (fun x -> obj##.refresh := x) refresh;
  iter (fun x -> obj##.onRefresh := Js.some @@ Js.wrap_callback x) onRefresh;
  iter (fun x -> obj##.frameRate := x) frameRate;
  iter (fun x -> obj##.pause := Js.bool x) pause;
  obj

let of_axis axis =
  (Js.Unsafe.coerce axis)##.realtime

let of_chart chart =
  (Js.Unsafe.coerce chart)##.options##.plugins##.streaming

let of_global () =
  Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.streaming

let set_per_axis axis plugin =
  (Js.Unsafe.coerce axis)##.realtime := plugin

let set_per_chart chart plugin =
  (Js.Unsafe.coerce chart)##.options##.plugins##.streaming := plugin

let set_globally plugin =
  Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.streaming := plugin
