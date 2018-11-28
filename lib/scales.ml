open Scales_raw

module Scale_label = Scale_label
module Grid_lines = Grid_lines
module Ticks = Ticks

type typ = Scales_raw.typ

type t = Scales_raw.t
let t_to_js = Scales_raw.t_to_js
let t_of_js = Scales_raw.t_of_js

type scales = Scales_raw.scales
let scales_to_js = Scales_raw.scales_to_js
let scales_of_js = Scales_raw.scales_of_js
let make = make

module Cartesian = struct
  open Cartesian
  type position = Cartesian.position

  module Ticks = struct
    include Ticks
    include (Common_ticks : module type of Common_ticks with type t := Ticks.t)
  end

  module Category = struct

    module Ticks = struct
      include Ticks
      include (Category.Ticks : module type of Category.Ticks
                                               with type t := Ticks.t)
    end
    include Common
    include (Category : module type of Category with module Ticks := Ticks)
    let make ?(type_ = `Category) = make ~type_
  end

  module Linear = struct

    module Ticks = struct
      include Ticks
      include (Linear.Ticks : module type of Linear.Ticks
                                             with type t := Ticks.t)
    end
    include Common
    include (Linear : module type of Linear with module Ticks := Ticks)
    let make ?(type_ = `Linear) = make ~type_
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
    let make ?(type_ = `Logarithmic) = make ~type_
  end

  module Time = struct

    module Ticks = struct
      include Ticks
      include (Time.Ticks : module type of Time.Ticks
                                           with type t := Ticks.t)
    end

    include Common
    include (Time : module type of Time with module Ticks := Ticks)
    let make ?(type_ = `Time) = make ~type_
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
