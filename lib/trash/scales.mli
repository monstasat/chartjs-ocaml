module Typ : sig
  type t

  val category : t

  val linear : t

  val logarithmic : t

  val time : t

  val make : string -> t

end
