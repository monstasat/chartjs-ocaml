module Dataset : sig

  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

end

[@@@js.stop]
module Datasets : sig
  include module type of Js_array.Make(Dataset)
end
[@@@js.start]
[@@@js.implem
 module Datasets = Js_array.Make(Dataset)
]

type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

val datasets : t -> Dataset.t Js_array.t
val set_datasets : t -> Dataset.t list -> unit

val labels : t -> string Js_array.t
val set_labels : t -> string list -> unit

val x_labels : t -> string Js_array.t
val set_x_labels : t -> string list -> unit

val y_labels : t -> string Js_array.t
val set_y_labels : t -> string list -> unit

val make : ?datasets:Dataset.t list ->
           ?labels:string list ->
           ?x_labels:string list ->
           ?y_labels:string list ->
           unit ->
           t [@@js.builder]
