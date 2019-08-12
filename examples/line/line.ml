open Js_of_ocaml
open Chartjs

let log x : unit = Js.Unsafe.global##.console##log x

let () =
  let dataset1 =
    createLineDataset
    @@ Js.array [|10.; 15.; 30.; 20.; 25.; 10.; 7.|] in
  dataset1##.borderColor := Color.of_string "red";
  dataset1##.backgroundColor := Color.of_string "rgba(255, 0, 0, 0.4)";
  dataset1##.label := Js.string "Dataset 1";
  dataset1##.pointStyle := Point_style.star;
  dataset1##.pointRadius := Scriptable_indexable.of_single 10;
  dataset1##.pointBorderWidth := Scriptable_indexable.of_single 2;
  let dataset2 =
    createLineDataset
    @@ Js.array [|20.; 10.; nan; 15.; 5.; 7.; 30.|] in
  dataset2##.borderColor := Color.of_string "blue";
  dataset2##.backgroundColor := Color.of_string "rgba(0, 0, 255, 0.4)";
  dataset2##.label := Js.string "Dataset 2";
  dataset2##.spanGaps := Js._false;
  dataset2##.pointStyle := Point_style.rectRot;
  dataset2##.pointRadius := Scriptable_indexable.of_single 10;
  dataset2##.pointBorderWidth := Scriptable_indexable.of_single 2;
  let data = createData
      ~datasets:[dataset1; dataset2]
      ~labels:[ "January"
              ; "February"
              ; "March"
              ; "April"
              ; "May"
              ; "June"
              ; "July"
              ]
      () in
  (* Initialize legend *)
  let legend_labels = createLegendLabels () in
  let legend = createLegend () in
  legend_labels##.fontSize := 12;
  legend_labels##.fontColor := Color.of_string "blue";
  legend_labels##.fontStyle := Js.string "bold";
  legend_labels##.fontFamily := Js.string "monospace";
  legend_labels##.padding := 20;
  legend_labels##.usePointStyle := Js._true;
  legend_labels##.filter := Js.wrap_meth_callback (fun _labels item data ->
      print_endline "filter called";
      log item;
      log data;
      Js._true);
  legend##.fullWidth := Js._false;
  legend##.reverse := Js._true;
  legend##.onHover := Js.wrap_meth_callback (fun _ _ item ->
      log item;
      print_endline "hover legend");
  legend##.labels := legend_labels;
  (* Initialize title *)
  let title = createTitle () in
  title##.display := Js._true;
  title##.fontFamily := Js.string "monospace";
  title##.fontColor := Js.string "indigo";
  title##.fontStyle := Js.string "italic";
  title##.fontSize := 15;
  title##.padding := 20;
  title##.lineHeight := Line_height.of_float 2.;
  title##.position := Position.left;
  title##.text := Indexable.of_list [Js.string "Title"; Js.string "subtitle"];
  (* Initialize tooltips *)
  let tooltips = createTooltip () in
  tooltips##.mode := Interaction_mode.index;
  tooltips##.intersect := Js._false;
  tooltips##.backgroundColor := Color.of_string "lime";
  tooltips##.titleFontStyle := Js.string "italic";
  tooltips##.itemSort := Js.wrap_meth_callback (fun _self a b data ->
      print_endline "tooltip item sorting fun called";
      log _self;
      log a;
      log b;
      log data;
      0);
  (* Initialize other options *)
  let options = createLineOptions () in
  options##.legend := legend;
  options##.title := title;
  options##.tooltips := tooltips;
  options##.maintainAspectRatio := Js._false;
  let chart = chart_from_id Chart.line (Js.Unsafe.coerce data) options "chart" in
  Js.Unsafe.global##.chart := chart
