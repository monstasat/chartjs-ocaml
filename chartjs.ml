module Array = Chartjs_array

module Scales = struct
  include Axes
end

module Data = struct
  include Chart.Data
end

module Options = struct
  include Chartjs_options
end

module Doughnut = struct
  include Pie
end

module Pie = struct
  include Pie
end

include Array.Infix

type canvas = Chart.canvas
type context = Chart.context
type node = Chart.node
type t = Chart.t

let make ?(options : Options.t option)
      ?(data : Data.t option)
      (typ : Chartjs_types.typ)
      (node : node) : t =
  let config = Chart.make_config ?data ?options ~type_:typ () in
  Chart.new_chart node config

include Chart.API
