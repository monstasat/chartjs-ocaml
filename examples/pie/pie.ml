open Js_of_ocaml
open Chartjs

let () =
  let border_color_fun = fun context ->
    Color.of_string
    @@ match context##.dataIndex with
    | 0 -> "red"
    | 1 -> "blue"
    | 2 -> "green"
    | _ -> "initial)" in
  let background_color_fun = fun context ->
    Color.of_string
    @@ match context##.dataIndex with
    | 0 -> "pink"
    | 1 -> "lightblue"
    | 2 -> "lightgreen"
    | _ -> "initial)" in
  let dataset = createPieDataset (Js.array [|40; 15; 20|]) in
  dataset##.borderColor := Scriptable_indexable.of_fun border_color_fun;
  dataset##.backgroundColor := Scriptable_indexable.of_fun background_color_fun;
  dataset##.borderWidth := Scriptable_indexable.of_single 5;
  dataset##.label := Js.string "Dataset 1";
  let data = createData
      ~datasets:[dataset]
      ~labels:["first"; "second"; "third"]
      () in
  let legend = createLegend () in
  let animation = createPieAnimation () in
  let options = createPieOptions () in
  animation##.animateScale := Js._true;
  animation##.animateRotate := Js._true;
  legend##.position := Position.left;
  legend##.onHover := Js.wrap_meth_callback (fun _ _ item ->
      let log x : unit = Js.Unsafe.global##.console##log x in
      log item;
      print_endline "hover legend");
  options##.cutoutPercentage := 20.;
  options##.animation := animation;
  options##.legend := legend;
  let pie = chart_from_id Chart.doughnut (Js.Unsafe.coerce data) options "chart" in
  Js.Unsafe.global##.chart := pie;
  Dom.appendChild Dom_html.document##.body pie##.canvas
