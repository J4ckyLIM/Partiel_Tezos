#import "storage.mligo" "Storage"
#import "parameter.mligo" "Parameter"
#importe "errors.mligo" "Errors"

type parameter =
  AddAdmin of Parameter.add_admin_param
  | RemoveAdmin of Parameter.remove_admin_param
  | AcceptAdmin of Parameter.accept_admin_param
  | WhitelistCreator of Parameter.add_creator_to_whitelist_param
  | BlacklistCreator of Parameter.add_creator_to_blacklist_param
  | CreateCollection of Parameter.create_collection

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
    

// Main entry point
let main (action, store : parameter * Storage.t) : operation list * Storage.t =
  ([] : operation list),
  (match action with 
    AddAdmin (address) -> Storage.add_admin param store
    | RemoveAdmin param -> Storage.remove_admin param store
    | AcceptAdmin param -> Storage.accept_admin param store
    | WhitelistCreator param -> Storage.whitelist_creator param store
    | BlacklistCreator param -> Storage.blacklist_creator param store
    | CreateCollection param -> Storage.create_collection param store
  )

