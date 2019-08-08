module Option_context = struct
  type t

  (* val chart : t -> Types.t
   * val data_index : t -> int
   * val dataset : t -> Data.Dataset.t
   * val dataset_index : t -> int *)
end


type 'a t =
  [ (* 'a indexable *)
  | `Fun of (Option_context.t -> 'a)
  ]
(* [@@@js.start]
 * [@@@js.implem
 *  type 'a scriptable =
 *    [ 'a indexable
 *    | `Fun of (Option_context.t -> 'a)
 *    ]
 *  let scriptable_to_js (f : 'a -> Ojs.t) = function
 *    | (`Single _ | `List _ | `Js_array _) as x -> indexable_to_js f x
 *    | `Fun f -> Ojs.fun_to_js 1 (fun js -> f (Option_context.t_of_js js))
 *  let scriptable_of_js (f : Ojs.t -> 'a) (j : Ojs.t) =
 *    match Ojs.type_of j with
 *    | "function" -> `Fun (fun x -> f @@ Ojs.apply j [|Option_context.t_to_js x|])
 *    | _ -> indexable_of_js f j
 * ] *)
