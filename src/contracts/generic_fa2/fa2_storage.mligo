module Errors = struct
   let undefined_token = "FA2_TOKEN_UNDEFINED"
   let ins_balance     = "FA2_INSUFFICIENT_BALANCE"
   let no_transfer     = "FA2_TX_DENIED"
   let not_owner       = "FA2_NOT_OWNER"
   let not_operator    = "FA2_NOT_OPERATOR"
   let not_supported   = "FA2_OPERATORS_UNSUPPORTED"
   let rec_hook_fail   = "FA2_RECEIVER_HOOK_FAILED"
   let send_hook_fail  = "FA2_SENDER_HOOK_FAILED"
   let rec_hook_undef  = "FA2_RECEIVER_HOOK_UNDEFINED"
   let send_hook_under = "FA2_SENDER_HOOK_UNDEFINED"
end

module Operators = struct
   type owner    = address
   type operator = address
   type token_id = nat
   type t = ((owner * operator), token_id set) big_map

   let init () : t = Big_map.empty

(** if transfer policy is Owner_or_operator_transfer *)
   let assert_authorisation (operators : t) (from_ : address) (token_id : nat) : unit =
      let sender_ = Tezos.get_sender () in
      if (sender_ = from_) then ()
      else
      let authorized = match Big_map.find_opt (from_,sender_) operators with
         Some (a) -> a | None -> Set.empty
      in if Set.mem token_id authorized then ()
      else failwith Errors.not_operator
(** if transfer policy is Owner_transfer
   let assert_authorisation (operators : t) (from_ : address) : unit =
      let sender_ = Tezos.sender in
      if (sender_ = from_) then ()
      else failwith Errors.not_owner
*)

(** if transfer policy is No_transfer
   let assert_authorisation (operators : t) (from_ : address) : unit =
      failwith Errors.no_owner
*)

   let assert_update_permission (owner : owner) : unit =
      assert_with_error (owner = Tezos.get_sender ()) "The sender can only manage operators for his own token"
   (** For an administator
      let admin = tz1.... in
      assert_with_error (Tezos.sender = admiin) "Only administrator can manage operators"
   *)

   let add_operator (operators : t) (owner : owner) (operator : operator) (token_id : token_id) : t =
      if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
      else
         let () = assert_update_permission owner in
         let auth_tokens = match Big_map.find_opt (owner,operator) operators with
            Some (ts) -> ts | None -> Set.empty in
         let auth_tokens  = Set.add token_id auth_tokens in
         Big_map.update (owner,operator) (Some auth_tokens) operators

   let remove_operator (operators : t) (owner : owner) (operator : operator) (token_id : token_id) : t =
      if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
      else
         let () = assert_update_permission owner in
         let auth_tokens = match Big_map.find_opt (owner,operator) operators with
         None -> None | Some (ts) ->
            let ts = Set.remove token_id ts in
            if (Set.size ts = 0n) then None else Some (ts)
         in
         Big_map.update (owner,operator) auth_tokens operators
end

module Ledger = struct
   type token_id = nat
   type owner = address
   type t = (token_id,owner) big_map

   let init () = Big_map.literal [
      (0n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (1n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (2n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (3n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (4n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (5n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (6n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (7n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (8n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      (9n, ("tz1h8DGEKrMBYQph1NiT8J1BLmnTCa6ocwEZ" : address));
      ]

   let is_owner_of (ledger:t) (token_id : token_id) (owner: address) : bool =
      (** We already sanitized token_id, a failwith here indicated a patological storage *)
      let current_owner = Option.unopt (Big_map.find_opt token_id ledger) in
      current_owner=owner

   let assert_owner_of (ledger:t) (token_id : token_id) (owner: address) : unit =
      assert_with_error (is_owner_of ledger token_id owner) Errors.ins_balance

   let transfer_token_from_user_to_user (ledger : t) (token_id : token_id) (from_ : owner) (to_ : owner) : t =
      let () = assert_owner_of ledger token_id from_ in
      let ledger = Big_map.update token_id (Some to_) ledger in
      ledger
end

module TokenMetadata = struct
   (**
      This should be initialized at origination, conforming to either
      TZIP-12 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#token-metadata
      or TZIP-16 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#contract-metadata-tzip-016
   *)
   (* with TZIP-12 *)
   type data = {token_id:nat;token_info:(string,bytes)map}
   type t = (nat,data) big_map

   let init () : t = Big_map.literal [
      (0n, {token_id=0n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 0", "symbol" : "WNF0", "decimal" : "0",}|}]]});
      (1n, {token_id=1n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 1", "symbol" : "WNF1", "decimal" : "0",}|}]]});
      (2n, {token_id=2n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 2", "symbol" : "WNF2", "decimal" : "0",}|}]]});
      (3n, {token_id=3n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 3", "symbol" : "WNF3", "decimal" : "0",}|}]]});
      (4n, {token_id=4n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 4", "symbol" : "WNF4", "decimal" : "0",}|}]]});
      (5n, {token_id=5n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 5", "symbol" : "WNF5", "decimal" : "0",}|}]]});
      (6n, {token_id=6n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 6", "symbol" : "WNF6", "decimal" : "0",}|}]]});
      (7n, {token_id=7n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 7", "symbol" : "WNF7", "decimal" : "0",}|}]]});
      (8n, {token_id=8n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 8", "symbol" : "WNF8", "decimal" : "0",}|}]]});
      (9n, {token_id=9n;token_info=Map.literal ["",[%bytes {|{"name" : "Wulfy FA2 NFT 9", "symbol" : "WNF9", "decimal" : "0",}|}]]});
   ]
end
#import "metadata.mligo" "Metadata"
type token_id = nat
   type t = [@layout:comb] {
      ledger : Ledger.t;
      operators : Operators.t;
      token_metadata : TokenMetadata.t;
      metadata  : Metadata.t;
   }

   let init () : t = {
      ledger = Ledger.init ();
      operators = Operators.init ();
      token_metadata = TokenMetadata.init ();
      metadata = Metadata.init ();
   }

   let is_owner_of (s:t) (owner : address) (token_id : token_id) : bool =
      Ledger.is_owner_of s.ledger token_id owner

   let assert_token_exist (s:t) (token_id : nat) : unit  =
      let _ = Option.unopt_with_error (Big_map.find_opt token_id s.token_metadata)
         Errors.undefined_token in
      ()

   let set_ledger (s:t) (ledger:Ledger.t) = {s with ledger = ledger}

   let get_operators (s:t) = s.operators
   let set_operators (s:t) (operators:Operators.t) = {s with operators = operators}