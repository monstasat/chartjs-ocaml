open Js_of_ocaml
open Chartjs

let log x : unit = Js.Unsafe.global##.console##log x

let () =
  (* Create first dataset. *)
  let dataset1 = create_line_dataset () in
  dataset1##.data := Js.array [|10.; 15.; 30.; 20.; 25.; 10.; 7.|];
  dataset1##.borderColor := Color.of_string "red";
  dataset1##.backgroundColor := Color.of_string "rgba(255, 0, 0, 0.4)";
  dataset1##.label := Js.string "Dataset 1";
  dataset1##.pointStyle := Point_style.star;
  dataset1##.pointRadius := Scriptable_indexable.of_single 10;
  dataset1##.pointBorderWidth := Scriptable_indexable.of_single 2;
  (* Create second dataset. *)
  let dataset2 = create_line_dataset () in
  dataset2##.data := Js.array [|20.; 10.; nan; 15.; 5.; 7.; 30.|];
  dataset2##.borderColor := Color.of_string "blue";
  dataset2##.backgroundColor := Color.of_string "rgba(0, 0, 255, 0.4)";
  dataset2##.label := Js.string "Dataset 2";
  dataset2##.spanGaps := Js._false;
  dataset2##.pointStyle := Point_style.rectRot;
  dataset2##.pointRadius := Scriptable_indexable.of_single 10;
  dataset2##.pointBorderWidth := Scriptable_indexable.of_single 2;
  (* Create chart data. *)
  let labels =
    Array.map Js.string
      [| "January"
       ; "February"
       ; "March"
       ; "April"
       ; "May"
       ; "June"
       ; "July"
      |] in
  let data = create_data () in
  data##.datasets := Js.array [|dataset1; dataset2|];
  data##.labels := Js.array labels;
  (* Initialize legend *)
  let legend_labels = create_legend_labels () in
  let legend = create_legend () in
  legend_labels##.fontSize := 12;
  legend_labels##.fontColor := Color.of_string "blue";
  legend_labels##.fontStyle := Js.string "bold";
  legend_labels##.fontFamily := Js.string "monospace";
  legend_labels##.padding := 20;
  legend_labels##.usePointStyle := Js._true;
  legend##.fullWidth := Js._false;
  legend##.reverse := Js._true;
  legend##.labels := legend_labels;
  (* Initialize title *)
  let title = create_title () in
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
  let tooltips = create_tooltip () in
  tooltips##.mode := Interaction_mode.index;
  tooltips##.intersect := Js._false;
  (* Initialize scales *)
  let axis = create_category_cartesian_axis () in
  let scales = create_line_scales () in
  axis##.display := Axis_display.auto;
  scales##.xAxes := Js.array [|axis|];
  (* Initialize other options *)
  let options = create_line_options () in
  (Js.Unsafe.coerce options)##.scales := scales;
  options##.legend := legend;
  options##.title := title;
  options##.tooltips := tooltips;
  options##.maintainAspectRatio := Js._false;
  let chart = chart_from_id Chart.line data options "chart" in
  Js.Unsafe.global##.chart := chart
