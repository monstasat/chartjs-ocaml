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
  (* Create dataset. *)
  let dataset = create_pie_dataset () in
  dataset##.data := Js.array [|40; 15; 20|];
  dataset##.borderColor := Scriptable_indexable.of_fun border_color_fun;
  dataset##.backgroundColor := Scriptable_indexable.of_fun background_color_fun;
  dataset##.borderWidth := Scriptable_indexable.of_single 5;
  dataset##.label := Js.string "Dataset 1";
  (* Create chart data. *)
  let data = create_data () in
  data##.datasets := Js.array [|dataset|];
  data##.labels := Js.array @@ Array.map Js.string [|"first"; "second"; "third"|];
  let legend = create_legend () in
  let animation = create_pie_animation () in
  let options = create_pie_options () in
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
  let pie = chart_from_id Chart.doughnut data options "chart" in
  Js.Unsafe.global##.chart := pie
