open Chartjs_types
open Chartjs_scales

module Scale_label = Scale_label
module Grid_lines = Grid_lines

module Cartesian = struct
  open Cartesian

  module Category = struct

    module Ticks = struct
      include Ticks
      include (Category.Ticks : module type of Category.Ticks
                                               with type t := Ticks.t)
    end
    include Common
    include (Category : module type of Category with module Ticks := Ticks)
    let make = make ~type_:"category"
  end

  module Linear = struct

    module Ticks = struct
      include Ticks
      include (Linear.Ticks : module type of Linear.Ticks
                                             with type t := Ticks.t)
    end
    include Common
    include (Linear : module type of Linear with module Ticks := Ticks)
    let make = make ~type_:"linear"
  end

  module Logarithmic = struct

    module Ticks = struct
      include Ticks
      include (Logarithmic.Ticks : module type of Logarithmic.Ticks
                                                  with type t := Ticks.t)
    end

    include Common
    include (Logarithmic : module type of Logarithmic
                                          with module Ticks := Ticks)
    let make = make ~type_:"logarithmic"

  end

  module Time = struct

    module Ticks = struct
      include Ticks
      include (Time.Ticks : module type of Time.Ticks
                                           with type t := Ticks.t)
    end

    include Common
    include (Time : module type of Time with module Ticks := Ticks)
    let make = make ~type_:"time"
  end

end

module Radial = struct
  open Radial

  module Linear = struct

    module Ticks = struct
      include Ticks
      include (Linear.Ticks : module type of Linear.Ticks
                                     with type t := Ticks.t)
    end

    include Common
    include (Linear : module type of Linear with module Ticks := Ticks)
  end

end

let make = make
