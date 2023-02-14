#import "storage.mligo" "Storage"
#import "parameter.mligo" "Parameter"
#import "errors.mligo" "Errors"
#import "./generic_fa2/fa2_storage.mligo" "FA2Storage"

type parameter =
  AddAdmin of Parameter.add_admin_param
  | RemoveAdmin of Parameter.remove_admin_param
  | AcceptAdmin of Parameter.accept_admin_param
  | BlacklistCreator of Parameter.add_creator_to_blacklist_param
  | CreateCollection of Parameter.create_collection_param
  | PayFeesToContract of Parameter.pay_fees_to_contract_param

type nft_storage = FA2Storage.t
type lambda_create_contract = (key_hash option * tez * nft_storage) -> (operation * address) 

let assert_admin (_assert_admin_param, store : Parameter.assert_admin_param * Storage.t) : unit =
  match  Map.find_opt(Tezos.get_sender():address) store.admin_list with
		Some (is_admin) -> 
			if is_admin then () else failwith Errors.not_admin
		| None -> failwith Errors.not_admin

let assert_creator_is_whitelisted (_assert_creator_whitelisted_param, store : Parameter.assert_creator_whitelisted_param * Storage.t) : unit =
  match  Map.find_opt(Tezos.get_sender():address) store.creator_whitelist with
    Some (is_whitelisted) -> 
      if is_whitelisted then () else failwith Errors.creator_not_whitelisted
    | None -> failwith Errors.creator_not_whitelisted
    
// ADMIN PART
let add_admin(add_admin_param, store: Parameter.add_admin_param * Storage.t) : Storage.t = 
	let admin_list : Storage.admin_mapping = 
		match Map.find_opt add_admin_param store.admin_list with
			Some _ -> failwith Errors.admin_already_added
			| None -> Map.add add_admin_param false store.admin_list
		in
	{ store with admin_list }

let accept_admin(_accept_admin_param, store: Parameter.accept_admin_param * Storage.t) : Storage.t =
	let sender : address = Tezos.get_sender() in
	let admin_list : Storage.admin_mapping = 
		match Map.find_opt sender store.admin_list with
			Some _ -> Map.update sender (Some(true)) store.admin_list
			| None -> failwith Errors.no_pending_admin_request
		in
	{ store with admin_list }

let remove_admin(remove_admin_param, store: Parameter.remove_admin_param * Storage.t) : Storage.t = 
	let sender:address = Tezos.get_sender() in
	if(sender = remove_admin_param) then 
		failwith Errors.cannot_remove_yourself
	else
		let admin_list : Storage.admin_mapping = 
			match Map.find_opt remove_admin_param store.admin_list with
				Some _ -> Map.remove remove_admin_param store.admin_list
				| None -> failwith Errors.not_admin
			in
		{ store with admin_list }

// CREATOR PART
let whitelist_creator(whitelist_creator_param, store: Parameter.add_creator_to_whitelist_param * Storage.t) : Storage.t = 
  let creator_whitelist : Storage.whitelisted_creator_mapping = 
    match Map.find_opt whitelist_creator_param store.creator_whitelist with
      Some _ -> failwith Errors.creator_already_whitelisted
      | None -> Map.add whitelist_creator_param true store.creator_whitelist
    in
  { store with creator_whitelist }

let blacklist_creator(blacklist_creator_param, store: Parameter.add_creator_to_blacklist_param * Storage.t) : Storage.t =
  let creator_blacklist : Storage.blacklisted_creator_mapping = 
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


// COLLECTION PART
let create_collection(_create_collection_param, store : Parameter.create_collection_param * Storage.t) : Storage.t =
  let sender = Tezos.get_sender() in
    let initial_storage: nft_storage = {
      ledger = Big_map.empty;
      token_metadata = Big_map.empty;
      operators = Big_map.empty;
      metadata = Big_map.empty;
    } in
      let create_my_contract () : (operation * address) =
        [%Michelson ( {| {
              UNPAIR ;
              UNPAIR ;
              CREATE_CONTRACT
#include "./generic_fa2/FA2_NFT.tz"
               ;
            PAIR } |}
              : lambda_create_contract)] ((None : key_hash option), 0tez, initial_storage)
    in
    let originate : operation * address = create_my_contract() in
    // insert into collections
    let new_all_collections = Big_map.add originate.1 sender store.collection_map in
    // insert into owned_collections
    let new_collection_by_owner = match Big_map.find_opt sender store.collection_by_owner with
    | None -> Big_map.add sender ([originate.1]: address list) store.collection_by_owner
    | Some addr_lst -> Big_map.update sender (Some(originate.1 :: addr_lst)) store.collection_by_owner
    in
    ([originate.0], { store with collection_map=new_all_collections; collection_by_owner=new_collection_by_owner})


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
    | CreateCollection param -> 
      let () : unit = assert_creator_is_whitelisted(Tezos.get_sender(), store) in	
			create_collection((), store)
    | PayFeesToContract param -> pay_fees_to_contract param store
  )

[@view] let get_collections ((), storage : unit * Storage.t) : Storage.collection_map = storage.collection_map

[@view] let get_collection_owners (owner_id , storage : nat * Storage.t) : Storage.collection_by_owner = 
  match Map.find_opt owner_id storage.collection_by_owner with
    Some collections -> collections
    | None -> failwith Errors.collection_not_found