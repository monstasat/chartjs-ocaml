open Js_of_ocaml
open Chartjs

let () =
  let line = chart_from_canvas Chart.line
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      Dom_html.(createCanvas document) in
  let bar = chart_from_canvas Chart.bar
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      Dom_html.(createCanvas document) in
  let (pie_dataset : pieDataset Js.t) =
    Js.Unsafe.coerce
    @@ object%js
      val mutable data = Js.array [|10; 15; 20|]
    end in
  pie_dataset##.backgroundColor := Scriptable.of_fun (fun context ->
      match context##.dataIndex with
      | 0 -> Js.string "red"
      | 1 -> Js.string "blue"
      | 2 -> Js.string "green"
      | _ -> Js.string "initial");
  let (pie_data : data Js.t) = object%js
    val mutable datasets = Js.array [|Js.Unsafe.coerce pie_dataset|]
    val mutable labels = Js.array [| Js.string "first"
                                   ; Js.string "second"
                                   ; Js.string "third" |]
    val mutable xLabels = Js.undefined
    val mutable yLabels = Js.undefined
  end in
  let pie = chart_from_canvas Chart.doughnut
      pie_data
      (Js.Unsafe.coerce @@ Js.Unsafe.obj [||])
      Dom_html.(createCanvas document) in
  Js.Unsafe.global##.lineChart := line;
  Js.Unsafe.global##.barChart := bar;
  Js.Unsafe.global##.pieChart := pie;
  Dom.appendChild Dom_html.document##.body line##.canvas;
  Dom.appendChild Dom_html.document##.body bar##.canvas;
  Dom.appendChild Dom_html.document##.body pie##.canvas
