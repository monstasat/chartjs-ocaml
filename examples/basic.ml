open Js_of_ocaml
open Chartjs

let () =
  let line = chart_from_canvas (Chart_type.make "line")
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      Dom_html.(createCanvas document) in
  let bar = chart_from_canvas (Chart_type.make "bar")
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      Dom_html.(createCanvas document) in
  let pie = chart_from_canvas (Chart_type.make "pie")
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      Dom_html.(createCanvas document) in
  ignore @@ Js.Unsafe.global##.console##log line;
  ignore @@ Js.Unsafe.global##.console##log bar;
  ignore @@ Js.Unsafe.global##.console##log pie;
  Dom.appendChild Dom_html.document##.body line##.canvas;
  Dom.appendChild Dom_html.document##.body bar##.canvas;
  Dom.appendChild Dom_html.document##.body pie##.canvas
