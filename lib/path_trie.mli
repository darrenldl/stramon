type 'a t

val empty : 'a t

val add : Abs_path.t -> 'a -> 'a t -> 'a t

val remove : Abs_path.t -> 'a t -> 'a t

val find : Abs_path.t -> 'a t -> 'a option

val find_exn : Abs_path.t -> 'a t -> 'a

val to_seq : 'a t -> (Abs_path.t * 'a) Seq.t
