open Js_of_ocaml

module Line_cap = struct
  type t = Js.js_string Js.t

  let butt = Js.string "butt"
  let round = Js.string "round"
  let square = Js.string "square"

  module Unsafe = struct
    let make s = Js.string s
  end
end

module Line_join = struct
  type t = Js.js_string Js.t

  let round = Js.string "round"
  let bevel = Js.string "bevel"
  let miter = Js.string "miter"

  module Unsafe = struct
    let make s = Js.string s
  end
end

module Interaction_mode = struct
  type t = Js.js_string Js.t

  let point = Js.string "point"
  let nearest = Js.string "nearest"
  let index = Js.string "index"
  let dataset = Js.string "dataset"
  let x = Js.string "x"
  let y = Js.string "y"

  module Unsafe = struct
    let make s = Js.string s
  end
end

module Point_style = struct
  type t = Js.js_string Js.t

  let circle = Js.string "circle"
  let cross = Js.string "cross"
  let crossRot = Js.string "crossRot"
  let dash = Js.string "dash"
  let line = Js.string "line"
  let rect = Js.string "rect"
  let rectRounded = Js.string "rectRounded"
  let rectRot = Js.string "rectRot"
  let star = Js.string "star"
  let triangle = Js.string "triangle"

  module Unsafe = struct
    let make s = Js.string s
  end
end

module Easing = struct
  type t = Js.js_string Js.t

  let linear = Js.string "linear"
  let easeInQuad = Js.string "easeInQuad"
  let easeOutQuad = Js.string "easeOutQuad"
  let easeInOutQuad = Js.string "easeInOutQuad"
  let easeInCubic = Js.string "easeInCubic"
  let easeOutCubic = Js.string "easeOutCubic"
  let easeInOutCubic = Js.string "easeInOutCubic"
  let easeInQuart = Js.string "easeInQuart"
  let easeOutQuart = Js.string "easeOutQuart"
  let easeInOutQuart = Js.string "easeInOutQuart"
  let easeInQuint = Js.string "easeInQuint"
  let easeOutQuint = Js.string "easeOutQuint"
  let easeInOutQuint = Js.string "easeInOutQuint"
  let easeInSine = Js.string "easeInSine"
  let easeOutSine = Js.string "easeOutSine"
  let easeInOutSine = Js.string "easeInOutSine"
  let easeInExpo = Js.string "easeInExpo"
  let easeOutExpo = Js.string "easeOutExpo"
  let easeInOutExpo = Js.string "easeInOutExpo"
  let easeInCirc = Js.string "easeInCirc"
  let easeOutCirc = Js.string "easeOutCirc"
  let easeInOutCirc = Js.string "easeInOutCirc"
  let easeInElastic = Js.string "easeInElastic"
  let easeOutElastic = Js.string "easeOutElastic"
  let easeInOutElastic = Js.string "easeInOutElastic"
  let easeInBack = Js.string "easeInBack"
  let easeOutBack = Js.string "easeOutBack"
  let easeInOutBack = Js.string "easeInOutBack"
  let easeInBounce = Js.string "easeInBounce"
  let easeOutBounce = Js.string "easeOutBounce"
  let easeInOutBounce = Js.string "easeInOutBounce"

  module Unsafe = struct
    let make s = Js.string s
  end
end

module Padding = struct

  type t

  class type obj = object
    method top : int Js.prop
    method right : int Js.prop
    method bottom : int Js.prop
    method left : int Js.prop
  end

  let make ~top ~right ~bottom ~left : t Js.t =
    let (obj : obj Js.t) = object%js
      val mutable top = top
      val mutable right = right
      val mutable bottom = bottom
      val mutable left = left
    end in
    Js.Unsafe.coerce obj

  let int (x : int) : t Js.t =
    Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int x

  let obj (x : obj Js.t) : t Js.t =
    Js.Unsafe.coerce x

  let cast_int (x : t Js.t) : int Js.opt =
    match Js.to_string @@ Js.typeof x with
    | "number" ->
      Js.some
      @@ int_of_float
      @@ Js.float_of_number
      @@ Js.Unsafe.coerce x
    | _ -> Js.null

  let cast_obj (x : t Js.t) : obj Js.t Js.opt =
    match Js.to_string @@ Js.typeof x with
    | "object" -> Js.some @@ Js.Unsafe.coerce x
    | _ -> Js.null

end

module Color = struct

  type t = Js.js_string Js.t

end

module Position = struct
  type t = Js.js_string Js.t

  let left = Js.string "left"
  let right = Js.string "right"
  let top = Js.string "top"
  let bottom = Js.string "bottom"

  module Unsafe = struct
    let make s = Js.string s
  end
end

module Tooltip_position = struct
  type t = Js.js_string Js.t

  let average = Js.string "average"
  let nearest = Js.string "nearest"

  module Unsafe = struct
    let make s = Js.string s
  end
end

(* FIXME *)
module Line_height = struct
  type t = int
end

module Hover_axis = struct
  type t = Js.js_string Js.t

  let x = Js.string "x"
  let y = Js.string "y"
  let xy = Js.string "xy"

  module Unsafe = struct
    let make s = Js.string s
  end
end

module Fill = struct
  type t

  let zero : t Js.t = Js.Unsafe.coerce @@ Js.string "zero"
  let top : t Js.t = Js.Unsafe.coerce @@ Js.string "top"
  let bottom : t Js.t = Js.Unsafe.coerce @@ Js.string "bottom"
  let _true : t Js.t = Js.Unsafe.coerce Js._true
  let _false : t Js.t = Js.Unsafe.coerce Js._false

  module Unsafe = struct
    let of_string s : t Js.t = Js.Unsafe.coerce @@ Js.string s
    let of_bool b : t Js.t = Js.Unsafe.coerce @@ Js.bool b
  end
end

module Time = struct
  type t = float
end

module Or_false = struct
  (** Type 'or_false' is the OCaml option type mapped
      to 'false' when the value is None *)
  type 'a t = 'a option
  (* [@@@js.start]
   * [@@@js.implem
   *  type 'a or_false = 'a option
   *  let or_false_to_js f = function
   *    | None -> Ojs.bool_to_js false
   *    | Some x -> f x
   *  let or_false_of_js f js =
   *    match Ojs.obj_type js with
   *    | "[object Boolean]" ->
   *       if Ojs.bool_of_js js then assert false else None
   *    | _ -> Some (f js)
   * ] *)
end

type line_dash = float Js.js_array Js.t

type line_dash_offset = float
