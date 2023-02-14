type user = address

type collectionContract = address
type collectionOwner = address

type admin_mapping = (user, bool) map

type whitelisted_creator_mapping = (user, bool) map
type blacklisted_creator_mapping = (user, bool) map


type t = {
  creator_blacklist: blacklisted_creator_mapping;
  creator_whitelist: whitelisted_creator_mapping;
  admins: admin_mapping;
  collection_contract: collectionContract;
  collection_owner: collectionOwner;
}