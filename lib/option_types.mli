module Option_context : sig
  type t
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val chart : t -> Types.t
  val data_index : t -> int
  val dataset : t -> Data.Dataset.t
  val dataset_index : t -> int
end

(** 'indexable' type represents a single value or a list of values *)
[@@@js.stop]
type 'a indexable =
  [ `Single of 'a
  | `List of 'a list
  ]
val indexable_to_js : ('a -> Ojs.t) -> 'a indexable -> Ojs.t
val indexable_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a indexable
[@@@js.start]
[@@@js.implem
 type 'a indexable =
   [ `Single of 'a
   | `List of 'a list
   ]
 let indexable_to_js (f : 'a -> Ojs.t) = function
   | `Single x -> f x
   | `List x -> Ojs.list_to_js f x
 let indexable_of_js (f : Ojs.t -> 'a) (js : Ojs.t) =
   match Ojs.obj_type js with
   | "[object Array]" -> `List (Ojs.list_of_js f js)
   | _ -> `Single (f js)
]

[@@@js.stop]
type 'a scriptable =
  [ 'a indexable
  | `Fun of (Option_context.t -> 'a)
  ]
val scriptable_to_js : ('a -> Ojs.t) -> 'a scriptable -> Ojs.t
val scriptable_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a scriptable
[@@@js.start]
[@@@js.implem
 type 'a scriptable =
   [ 'a indexable
   | `Fun of (Option_context.t -> 'a)
   ]
 let scriptable_to_js (f : 'a -> Ojs.t) = function
   | (`Single _ | `List _) as indexable -> indexable_to_js f indexable
   | `Fun f -> Ojs.fun_to_js 1 (fun js -> f (Option_context.t_of_js js))
 let scriptable_of_js (f : Ojs.t -> 'a) (j : Ojs.t) =
   match Ojs.type_of j with
   | "function" -> `Fun (fun x -> f @@ Ojs.apply j [|Option_context.t_to_js x|])
   | _ -> indexable_of_js f j
]
