type user = address

type collectionContract = address

type admin_mapping = (user, bool) map

type whitelisted_creator_mapping = (user, bool) map
type blacklisted_creator_mapping = (user, bool) map

type collection_list = collectionContract list
type collection_by_owner = (user, collection_list) map


type t = {
  creator_blacklist: blacklisted_creator_mapping;
  creator_whitelist: whitelisted_creator_mapping;
  admins: admin_mapping;
  collection_list: collection_list;
  collection_by_owner: collection_by_owner;
}