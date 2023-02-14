type user = address

type collectionContract = address

type admin_mapping = (user, bool) map

type whitelisted_creator_mapping = (user, bool) map
type blacklisted_creator_mapping = (user, bool) map

type collection_map = (collectionContract, user) big_map
type collection_by_owner = (user, collectionContract) big_map


type t = {
  creator_blacklist: blacklisted_creator_mapping;
  creator_whitelist: whitelisted_creator_mapping;
  admin_list: admin_mapping;
  collection_map: collection_map;
  collection_by_owner: collection_by_owner;
}