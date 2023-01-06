type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val add : Abs_path.t -> 'a -> 'a t -> 'a t

val remove : Abs_path.t -> 'a t -> 'a t

val find : Abs_path.t -> 'a t -> 'a option

val find_exn : Abs_path.t -> 'a t -> 'a

val merge : (Abs_path.t -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

val union : (Abs_path.t -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val to_seq : 'a t -> (Abs_path.t * 'a) Seq.t

val of_seq : (Abs_path.t * 'a) Seq.t -> 'a t
