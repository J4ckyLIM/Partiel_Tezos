#import "storage.mligo" "Storage"
#import "parameter.mligo" "Parameter"
#importe "errors.mligo" "Errors"

type parameter =
  AddAdmin of Parameter.add_admin_param
  | RemoveAdmin of Parameter.remove_admin_param
  | AcceptAdmin of Parameter.accept_admin_param
  | BlacklistCreator of Parameter.add_creator_to_blacklist_param
  | CreateCollection of Parameter.create_collection
  | PayFeesToContract of Parameter.pay_fees_to_contract_param

type lambda_create_contract = (key_hash option * tez * ext_storage) -> (operation * address) 

let assert_admin (_assert_admin_param, store : Parameter.assert_admin_param * Storage.t) : unit =
  match  Map.find_opt(Tezos.get_sender():address) store.admin_list with
		Some (is_admin) -> 
			if is_admin then () else failwith Errors.not_admin
		| None -> failwith Errors.not_admin

let assert_creator_is_whitelisted (_assert_creator_is_whitelisted_param, store : Parameter.assert_creator_is_whitelisted_param * Storage.t) : unit =
  match  Map.find_opt(Tezos.get_sender():address) store.creator_whitelist with
    Some (is_whitelisted) -> 
      if is_whitelisted then () else failwith Errors.creator_not_whitelisted
    | None -> failwith Errors.creator_not_whitelisted
    
// ADMIN PART
let add_admin(add_admin_param, store: Parameter.add_admin_param * Storage.t) : Storage.t = 
	let admin_list : Storage.admin_mapping = 
		match Map.find_opt add_admin_param store.admin_list with
			Some _ -> failwith Errors.invitation_already_sent
			| None -> Map.add add_admin_param false store.admin_list
		in
	{ store with admin_list }

let accept_admin(_accept_admin_param, store: Parameter.accept_admin_param * Storage.t) : Storage.t =
	let sender : address = Tezos.get_sender() in
	let admin_list : Storage.admin_mapping = 
		match Map.find_opt sender store.admin_list with
			Some _ -> Map.update sender (Some(true)) store.admin_list
			| None -> failwith Errors.no_admin_invitation
		in
	{ store with admin_list }

let remove_admin(remove_admin_param, store: Parameter.remove_admin_param * Storage.t) : Storage.t = 
	let sender:address = Tezos.get_sender() in
	if(sender = remove_admin_param) then 
		failwith Errors.cant_remove_self_admin
	else
		let admin_list : Storage.admin_mapping = 
			match Map.find_opt remove_admin_param store.admin_list with
				Some _ -> Map.remove remove_admin_param store.admin_list
				| None -> failwith Errors.wasnt_admin
			in
		{ store with admin_list }

// CREATOR PART
let whitelist_creator(whitelist_creator_param, store: Parameter.add_creator_to_whitelist_param * Storage.t) : Storage.t = 
  let creator_whitelist : Storage.creator_whitelist = 
    match Map.find_opt whitelist_creator_param store.creator_whitelist with
      Some _ -> failwith Errors.creator_already_whitelisted
      | None -> Map.add whitelist_creator_param true store.creator_whitelist
    in
  { store with creator_whitelist }

let blacklist_creator(blacklist_creator_param, store: Parameter.add_creator_to_blacklist_param * Storage.t) : Storage.t =
  let creator_blacklist : Storage.creator_blacklist = 
    match Map.find_opt blacklist_creator_param store.creator_blacklist with
      Some _ -> failwith Errors.creator_already_blacklisted
      | None -> Map.add blacklist_creator_param true store.creator_blacklist
    in
  { store with creator_blacklist }

// This allow a creator to become whitelisted
let pay_fees_to_contract(_pay_fees_to_contract, store : Parameter.pay_fees_to_contract_param * Storage.t) : Storage.t =
	let amount : tez = Tezos.get_amount() in
	let sender: address = Tezos.get_sender() in
	if(amount = 10tez) then
		whitelist_creator(sender, store)
	else
		failwith Errors.wrong_fees_amount
	store


// Main entry point
let main (action, store : parameter * Storage.t) : operation list * Storage.t =
  ([] : operation list),
  (match action with 
    AddAdmin (address) -> 
      let () : unit = assert_admin(Tezos.get_sender(), store) in
      add_admin (n, store)
    | RemoveAdmin param -> 
      let () : unit = assert_admin(Tezos.get_sender(), store) in
      remove_admin param store
    | AcceptAdmin param -> 
      let () : unit = assert_admin(Tezos.get_sender(), store) in
      accept_admin param store
    | BlacklistCreator param -> 
      let () : unit = assert_admin(Tezos.get_sender(), store) in
      blacklist_creator param store
    | CreateCollection param -> Storage.create_collection param store
    | PayFeesToContract param -> Storage.pay_fees_to_contract param store
  )

[@view] let get_collections ((), storage : unit * Storage.t) : Storage.collection_list = storage.collection_list

[@view] let get_collection_owners (owner_id , storage : nat * Storage.t) : Storage.collection_list = 
  match Map.find_opt owner_id storage.collection_by_owner with
    Some collections -> collections
    | None -> failwith Errors.collection_not_found