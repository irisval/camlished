type coordinates
type resources

type building_type_id 

module type BuildingType = sig
  type t
  type id
  val get_cost : t -> id
  val get_output : t -> resources
end


(** instances of buildings *)
module type Building = sig
  type t
  type building_type 
  val get_pos : t -> coordinates
  val get_type_id : t -> building_type
end