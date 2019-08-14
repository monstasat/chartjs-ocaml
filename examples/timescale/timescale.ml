open Js_of_ocaml
open Chartjs

let log x : unit = Js.Unsafe.global##.console##log x

let format = "MM/DD/YYYY HH:mm"

let new_date year = Time.of_array [|year|]

let random () = Random.int 10

let () =
  Random.init (Float.to_int (new%js Js.date_now)##getTime);
  let dataset1 = create_line_dataset () in
  dataset1##.data := Js.array @@ Array.init 7 (fun _ -> random ());
  dataset1##.label := Js.string "My First Dataset";
  dataset1##.backgroundColor := Color.of_string "rgba(255, 0, 0, 0.5)";
  dataset1##.borderColor := Color.of_string "red";
  dataset1##.fill := Line_fill._false;
  let dataset2 = create_line_dataset () in
  dataset2##.data := Js.array @@ Array.init 7 (fun _ -> random ());
  dataset2##.label := Js.string "My Second Dataset";
  dataset2##.backgroundColor := Color.of_string "rgba(0, 0, 255, 0.5)";
  dataset2##.borderColor := Color.of_string "blue";
  dataset2##.fill := Line_fill._false;
  let dataset3 = create_line_dataset () in
  dataset3##.data :=
    Js.array
    @@ [| create_data_point ~x:(new_date 1990) ~y:(random ())
        ; create_data_point ~x:(new_date 1992) ~y:(random ())
        ; create_data_point ~x:(new_date 1994) ~y:(random ())
        ; create_data_point ~x:(new_date 1996) ~y:(random ())
       |];
  dataset3##.label := Js.string "Dataset with point data";
  dataset3##.backgroundColor := Color.of_string "rgba(0, 255, 0, 0.5)";
  dataset3##.borderColor := Color.of_string "green";
  dataset3##.fill := Line_fill._false;
  let data = create_data () in
  data##.labels := Js.array [| new_date 1990
                             ; new_date 1991
                             ; new_date 1992
                             ; new_date 1993
                             ; new_date 1994
                             ; new_date 1995
                             ; new_date 1996 |];
  data##.datasets := Js.array [| coerce_dataset dataset1
                               ; coerce_dataset dataset2
                               ; coerce_dataset dataset3 |];
  (* Initialize title *)
  let title = create_title () in
  title##.display := Js._true;
  title##.text := Indexable.of_single @@ Js.string "Chart.js Time Scale";
  (* Initialize scales *)
  let time = create_time_cartesian_options () in
  let (scaleLabel : scaleLabel Js.t) = Js.Unsafe.obj [||] in
  let xAxis = create_time_cartesian_axis () in
  time##._parser := Time_parser.of_string format;
  time##.tooltipFormat := Js.string "ll HH:mm";
  scaleLabel##.display := Js._true;
  scaleLabel##.labelString := Js.string "Date";
  xAxis##.time := time;
  xAxis##.scaleLabel := scaleLabel;
  let yAxis = create_cartesian_axis () in
  let (scaleLabel : scaleLabel Js.t) = Js.Unsafe.obj [||] in
  scaleLabel##.display := Js._true;
  scaleLabel##.labelString := Js.string "value";
  yAxis##.scaleLabel := scaleLabel;
  let scales = create_line_scales () in
  scales##.xAxes := Js.array [|xAxis|];
  scales##.yAxes := Js.array [|yAxis|];
  (* Initialize other options *)
  let options = create_line_options () in
  options##.scales := scales;
  options##.title := title;
  options##.maintainAspectRatio := Js._false;
  let chart = chart_from_id Chart.line data options "chart" in
  Js.Unsafe.global##.chart := chart
