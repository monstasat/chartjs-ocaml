open Chartjs_types

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
    include Line.Dataset
          
    type ('a, 'b) point = { x : 'a; y : 'b }

    module type DS = sig
      type item

      include module type of Line.Dataset
      module A : Chartjs_array.Typed_array with type item := item

      val data : t -> A.t
      val set_data : t -> item list -> unit
      val make :
        ?data:item list ->
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

    module Make_point(X : Jsable)(Y : Jsable) = struct
      type t = (X.t, Y.t) point
      let t_to_js (t : (X.t, Y.t) point) : Ojs.t =
        Ojs.obj [|("x", X.t_to_js t.x); ("y", Y.t_to_js t.y)|]
      let t_of_js (j : Ojs.t) : (X.t, Y.t) point =
        { x = X.t_of_js @@ Ojs.get j "x"
        ; y = Y.t_of_js @@ Ojs.get j "y"
        }
    end
    module Make(M : Jsable) = struct
      type item

      include Line.Dataset
      module A = Chartjs_array.Make(M)

      val data : t -> A.t [@@js.get]
      val set_data : t -> M.t list -> unit [@@js.set]
      let make ?(data : M.t list option) =
        let data = match data with
          | None -> None
          | Some x -> Some (Ojs.list_to_js M.t_to_js x) in
        make ?data
    end

    (** Dataset with data represented as an array with values of simple type *)
    module Int = Make(Int)
    module Int32 = Make(Int32)
    module Int64 = Make(Int64)
    module Float = Make(Float)
    module String = Make(String)
    module Time = Make(Time)
  end
end

module Bar = struct
  module Options = Bar.Options
  module Dataset = struct
    include Bar.Dataset
    type ('a, 'b) point = { x : 'a; y : 'b }

    module type DS = sig
      type item

      include module type of Bar.Dataset
      module A : Chartjs_array.Typed_array with type item := item

      val data : t -> A.t
      val set_data : t -> item list -> unit
      val make :
        ?data:item list ->
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

    module Make_point(X : Jsable) (Y : Jsable) = struct
      type t = (X.t, Y.t) point
      let t_to_js (t : (X.t, Y.t) point) : Ojs.t =
        Ojs.obj [|("x", X.t_to_js t.x); ("y", Y.t_to_js t.y)|]
      let t_of_js (j : Ojs.t) : (X.t, Y.t) point =
        { x = X.t_of_js @@ Ojs.get j "x"
        ; y = Y.t_of_js @@ Ojs.get j "y"
        }
    end
    module Make(M : Jsable) : DS with type item := M.t = struct
      type item

      include Bar.Dataset
      module A = Chartjs_array.Make(M)

      val data : t -> A.t [@@js.get]
      val set_data : t -> M.t list -> unit [@@js.set]
      let make ?(data : M.t list option) =
        let data = match data with
          | None -> None
          | Some x -> Some (Ojs.list_to_js M.t_to_js x) in
        make ?data
    end

    (** Dataset with data represented as an array with values of simple type *)
    module Int = Make(Int)
    module Int32 = Make(Int32)
    module Int64 = Make(Int64)
    module Float = Make(Float)
    module String = Make(String)
    module Time = Make(Time)

  end
end

module Radar = struct
  (* TODO implement *)
end

module Pie = struct
  module Options = Pie.Options
  module Dataset = struct
    include Pie.Dataset

    module type DS = sig
      type item

      include module type of Pie.Dataset
      module A : Chartjs_array.Typed_array with type item := item

      val data : t -> A.t
      val set_data : t -> item list -> unit
      val make :
        ?data:item list ->
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

    module Make(M : Jsable) : DS with type item := M.t = struct
      type item

      include Pie.Dataset
      module A = Chartjs_array.Make(M)

      val data : t -> A.t [@@js.get]
      val set_data : t -> M.t list -> unit [@@js.set]
      let make ?(data : M.t list option) =
        let data = match data with
          | None -> None
          | Some x -> Some (Ojs.list_to_js M.t_to_js x) in
        make ?data
    end

    (** Dataset with data represented as an array with values of simple type *)
    module Int = Make(Int)
    module Int32 = Make(Int32)
    module Int64 = Make(Int64)
    module Float = Make(Float)
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
