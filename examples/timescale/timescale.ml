open Js_of_ocaml
open Chartjs

let log x : unit = Js.Unsafe.global##.console##log x

let format = "MM/DD/YYYY HH:mm"

let new_date days =
  ((Js.Unsafe.global##moment)##add days (Js.string "d"))##toDate

let new_date_string days =
  ((Js.Unsafe.global##moment)##add days (Js.string "d"))##format
    (Js.string format)

let random () = Random.int 10

let () =
  Random.init (Float.to_int (new%js Js.date_now)##getTime);
  let dataset1 =
    createLineDataset
    @@ Js.array
    @@ Array.init 7 (fun _ -> random ()) in
  dataset1##.label := Js.string "My First Dataset";
  dataset1##.backgroundColor := Color.of_string "rgba(255, 0, 0, 0.5)";
  dataset1##.borderColor := Color.of_string "red";
  dataset1##.fill := Line_fill._false;
  let dataset2 =
    createLineDataset
    @@ Js.array
    @@ Array.init 7 (fun _ -> random ()) in
  dataset2##.label := Js.string "My Second Dataset";
  dataset2##.backgroundColor := Color.of_string "rgba(0, 0, 255, 0.5)";
  dataset2##.borderColor := Color.of_string "blue";
  dataset2##.fill := Line_fill._false;
  let dataset3 =
    createLineDataset
    @@ Js.array
    @@ [| createDataPoint ~x:(new_date_string 0) ~y:(random ())
        ; createDataPoint ~x:(new_date_string 5) ~y:(random ())
        ; createDataPoint ~x:(new_date_string 7) ~y:(random ())
        ; createDataPoint ~x:(new_date_string 15) ~y:(random ())
       |] in
  dataset3##.label := Js.string "Dataset with point data";
  dataset3##.backgroundColor := Color.of_string "rgba(0, 255, 0, 0.5)";
  dataset3##.borderColor := Color.of_string "green";
  dataset3##.fill := Line_fill._false;
  let data = createData
      ~labels:[ new_date 0
              ; new_date 1
              ; new_date 2
              ; new_date 3
              ; new_date 4
              ; new_date 5
              ; new_date 6 ]
      ~datasets:[ coerce_dataset dataset1
                ; coerce_dataset dataset2
                ; coerce_dataset dataset3 ]
      () in
  (* Initialize title *)
  let title = createTitle () in
  title##.display := Js._true;
  title##.text := Indexable.of_single @@ Js.string "Chart.js Time Scale";
  (* Initialize scales *)
  let time = createTimeCartesianOptions () in
  let (scaleLabel : scaleLabel Js.t) = Js.Unsafe.obj [||] in
  let xAxis = createTimeCartesianAxis () in
  time##._parser := Time_parser.of_string format;
  time##.tooltipFormat := Js.string "ll HH:mm";
  scaleLabel##.display := Js._true;
  scaleLabel##.labelString := Js.string "Date";
  xAxis##.time := time;
  xAxis##.scaleLabel := scaleLabel;
  let yAxis = createCartesianAxis () in
  let (scaleLabel : scaleLabel Js.t) = Js.Unsafe.obj [||] in
  scaleLabel##.display := Js._true;
  scaleLabel##.labelString := Js.string "value";
  yAxis##.scaleLabel := scaleLabel;
  let scales = createLineScales ~xAxes:[xAxis] ~yAxes:[yAxis] () in
  (* Initialize other options *)
  let options = createLineOptions () in
  options##.scales := scales;
  options##.title := title;
  options##.maintainAspectRatio := Js._false;
  let chart = chart_from_id Chart.line data options "chart" in
  Js.Unsafe.global##.chart := chart
