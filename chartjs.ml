open Chartjs_types

module Array = Chartjs_array

module type Jsable = sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end

module Data_types = struct

  module Int : Jsable with type t = int = struct
    type t = int
    let t_to_js = Ojs.int_to_js
    let t_of_js = Ojs.int_of_js
  end
  module Float : Jsable with type t = float = struct
    type t = float
    let t_to_js = Ojs.float_to_js
    let t_of_js = Ojs.float_of_js
  end
  module Int32 : Jsable with type t = int32 = struct
    type t = int32
    let t_to_js x = Ojs.float_to_js @@ Int32.to_float x
    let t_of_js x = Int32.of_float @@ Ojs.float_of_js x
  end
  module Int64 : Jsable with type t = int64 = struct
    type t = int64
    let t_to_js x = Ojs.float_to_js @@ Int64.to_float x
    let t_of_js x = Int64.of_float @@ Ojs.float_of_js x
  end
  module String : Jsable with type t = string = struct
    type t = string
    let t_to_js = Ojs.string_to_js
    let t_of_js = Ojs.string_of_js
  end
  module Time : Jsable with type t = Ptime.t = Time

end

module Scales = struct
  include Axes
end

module Data = struct
  include Chart.Data
end

module Options = struct
  include Chartjs_options
end

module Line = struct
  module Options = Line.Options
  module Dataset = struct
    type ('a, 'b) point = { x : 'a; y : 'b }

    module Make_point(X : Jsable)(Y : Jsable) = struct
      type t = (X.t, Y.t) point

      let t_to_js (t : (X.t, Y.t) point) : Ojs.t =
        Ojs.obj [|("x", X.t_to_js t.x); ("y", Y.t_to_js t.y)|]
      let t_of_js (j : Ojs.t) : (X.t, Y.t) point =
        { x = X.t_of_js @@ Ojs.get j "x"
        ; y = Y.t_of_js @@ Ojs.get j "y"
        }
    end

    module type DS = sig
      type dot
      type data = dot list
      include module type of Line.Dataset
      val make :
        ?data:data ->
        ?type_:typ ->
        ?label:string ->
        ?x_axis_id:string ->
        ?y_axis_id:string ->
        ?background_color:Color.t ->
        ?border_color:Color.t ->
        ?border_width:int ->
        ?border_dash:border_dash ->
        ?border_dash_offset:border_dash_offset ->
        ?border_cap_style:line_cap ->
        ?border_join_style:line_join ->
        ?cubic_interpolation_mode:cubic_interpolation_mode ->
        ?fill:fill ->
        ?line_tension:float ->
        ?show_line:bool ->
        ?span_gaps:bool ->
        ?stepped_line:stepped_line ->
        ?point_background_color:Color.t indexable ->
        ?point_border_color:Color.t indexable ->
        ?point_border_width:int indexable ->
        ?point_radius:int indexable ->
        ?point_rotation:int indexable ->
        ?point_hit_radius:int indexable ->
        ?point_hover_background_color:Color.t indexable ->
        ?point_hover_border_color:Color.t indexable ->
        ?point_hover_border_width:int indexable ->
        ?point_hover_radius:int indexable ->
        unit ->
        t
    end

    module Make(M : Jsable) = struct
      type data = M.t list
      let data_to_js (t : data) : Ojs.t = Ojs.list_to_js M.t_to_js t
      let data_of_js (j : Ojs.t) : data = Ojs.list_of_js M.t_of_js j
      include Line.Dataset
      let make ?(data : data option) =
        let data = match data with
          | None -> None
          | Some x -> Some (data_to_js x) in
        make ?data
    end

    module Raw = Line.Dataset

    module Int = Make(Data_types.Int)
    module Int32 = Make(Data_types.Int32)
    module Int64 = Make(Data_types.Int64)
    module Float = Make(Data_types.Float)
    module String = Make(Data_types.String)
    module Time = Make(Data_types.Time)
  end
end

module Bar = struct
  module Options = Bar.Options
  module Dataset = struct
    type ('a, 'b) point = { x : 'a; y : 'b }

    module Make_point(X : Jsable) (Y : Jsable) = struct
      type t = (X.t, Y.t) point

      let t_to_js (t : (X.t, Y.t) point) : Ojs.t =
        Ojs.obj [|("x", X.t_to_js t.x); ("y", Y.t_to_js t.y)|]
      let t_of_js (j : Ojs.t) : (X.t, Y.t) point =
        { x = X.t_of_js @@ Ojs.get j "x"
        ; y = Y.t_of_js @@ Ojs.get j "y"
        }
    end
    module type DS = sig
      type dot
      type data = dot list
      include module type of Bar.Dataset
      val make :
        ?data:data ->
        ?type_:typ ->
        ?label:string ->
        ?x_axis_id:string ->
        ?y_axis_id:string ->
        ?stack:string ->
        ?border_skipped:border_skipped ->
        ?background_color:Color.t indexable ->
        ?border_color:Color.t indexable ->
        ?border_width:int indexable ->
        ?hover_background_color:Color.t indexable ->
        ?hover_border_color:Color.t indexable ->
        ?hover_border_width:int indexable ->
        unit ->
        t
    end
    module Make(M : Jsable) = struct
      type data = M.t list
      let data_to_js (t : data) : Ojs.t = Ojs.list_to_js M.t_to_js t
      let data_of_js (j : Ojs.t) : data = Ojs.list_of_js M.t_of_js j
      include Bar.Dataset
      let make ?(data : data option) =
        let data = match data with
          | None -> None
          | Some x -> Some (data_to_js x) in
        make ?data
    end

    module Raw = Bar.Dataset
    module Int = Make(Data_types.Int)
    module Int32 = Make(Data_types.Int32)
    module Int64 = Make(Data_types.Int64)
    module Float = Make(Data_types.Float)
    module String = Make(Data_types.String)
    module Time = Make(Data_types.Time)
  end
end

module Radar = struct
  (* TODO implement *)
end

module Pie = struct
  module Options = Pie.Options
  module Dataset = struct
    module type DS = sig
      type dot
      include module type of Pie.Dataset
      module Array : sig
        include Chartjs_array.Typed_array with type item := dot
      end

      val data : t -> Array.t
      val set_data : t -> dot list -> unit
      val make :
        ?data:dot list ->
        ?type_:typ ->
        ?background_color:Color.t list ->
        ?border_color:Color.t list ->
        ?border_width:int list ->
        ?hover_background_color:Color.t list ->
        ?hover_border_color:Color.t list ->
        ?hover_border_width:int list ->
        unit ->
        t
    end
    module Make(M : Jsable) = struct
      module Array = Chartjs_array.Make(M)
      include Pie.Dataset

      val data : t -> Array.t [@@js.get]
      val set_data : t -> M.t list -> unit [@@js.set]

      let make ?(data : M.t list option) =
        let data = match data with
          | None -> None
          | Some x -> Some (Ojs.list_to_js M.t_to_js x) in
        make ?data
    end

    module Raw = Pie.Dataset
    module Int = Make(Data_types.Int)
    module Int32 = Make(Data_types.Int32)
    module Int64 = Make(Data_types.Int64)
    module Float = Make(Data_types.Float)
  end
end

module Polar_area = struct
  (* TODO implement *)
end

module Bubble = struct
  (* TODO implement *)
end

module Scatter = struct
  (* TODO implement *)
end

include Array.Infix

type canvas = Chart.canvas
type context = Chart.context
type node = Chart.node
type t = Chart.t

let make ?(options : Options.t option)
      ?(data : Data.t option)
      (typ : typ)
      (node : node) : t =
  let config = Chart.make_config ?data ?options ~type_:typ () in
  Chart.new_chart node config

include Chart.API
