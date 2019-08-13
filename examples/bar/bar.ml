open Js_of_ocaml
open Chartjs

let log x : unit = Js.Unsafe.global##.console##log x

let () =
  let dataset1 =
    createBarDataset
    @@ Js.array [|10.; 15.; 30.; 20.; 25.; 10.; 7.|] in
  dataset1##.borderColor := Scriptable_indexable.of_single @@ Color.of_string "red";
  dataset1##.backgroundColor := (
    Scriptable_indexable.of_single
    @@ Color.of_string "rgba(255, 0, 0, 0.4)");
  dataset1##.label := Js.string "Dataset 1";
  let dataset2 =
    createBarDataset
    @@ Js.array [|20.; 10.; nan; 15.; 5.; 7.; 30.|] in
  dataset2##.borderColor := Scriptable_indexable.of_single @@ Color.of_string "blue";
  dataset2##.backgroundColor := (
    Scriptable_indexable.of_single
    @@ Color.of_string "rgba(0, 0, 255, 0.4)");
  dataset2##.label := Js.string "Dataset 2";
  let labels =
    List.map Js.string
      [ "January"
      ; "February"
      ; "March"
      ; "April"
      ; "May"
      ; "June"
      ; "July"
      ] in
  let data = createData
      ~datasets:[dataset1; dataset2]
      ~labels
      () in
  (* Initialize title *)
  let title = createTitle () in
  title##.display := Js._true;
  title##.text := Indexable.of_single @@ Js.string "Bar Chart";
  (* Initialize tooltips *)
  let tooltips = createTooltip () in
  tooltips##.mode := Interaction_mode.index;
  tooltips##.intersect := Js._false;
  (* Initialize other options *)
  let options = createBarOptions () in
  options##.title := title;
  options##.tooltips := tooltips;
  options##.maintainAspectRatio := Js._false;
  let chart = chart_from_id Chart.bar (Js.Unsafe.coerce data) options "bar" in
  Js.Unsafe.global##.chart := chart
