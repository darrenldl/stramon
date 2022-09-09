type 'a t

val empty : 'a t

val add : Abs_path.t -> 'a -> 'a t -> 'a t

val find : Abs_path.t -> 'a t -> 'a option
