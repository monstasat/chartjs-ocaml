open Chartjs_types
open Chartjs_scales

module Scale_label = Scale_label
module Grid_lines = Grid_lines

type axis

module Cartesian = struct

  module Category = struct

    module Ticks = struct
      include Ticks
      include (Category.Ticks : module type of Category.Ticks
                                               with type t := Ticks.t)
    end

    include Cartesian

    let make = make ~type_:"category"

    let to_axis (t : t) : axis =
      Obj.magic t
  end

  module Linear = struct

    module Ticks = struct
      include Ticks
      include (Linear.Ticks : module type of Linear.Ticks
                                             with type t := Ticks.t)
    end

    include Cartesian

    let make = make ~type_:"linear"

    let to_axis (t : t) : axis =
      Obj.magic t

  end

  module Logarithmic = struct

    module Ticks = struct
      include Ticks
      include (Logarithmic.Ticks : module type of Logarithmic.Ticks
                                                  with type t := Ticks.t)
    end

    include Cartesian

    let make = make ~type_:"logarithmic"

    let to_axis (t : t) : axis =
      Obj.magic t
  end

  module Time = struct

    module Ticks = struct
      include Ticks
      include (Time.Ticks : module type of Time.Ticks
                                           with type t := Ticks.t)
    end

    include Cartesian
    include (Time : module type of Time with type t := Cartesian.t
                                   with module Ticks := Ticks)
    let make = make ~type_:"time"
  end

end

module Radial = struct

end

let make = make
