import { InMemorySigner } from "@taquito/signer";
import { MichelsonMap, TezosToolkit } from "@taquito/taquito";
import mainContract from "../src/compiled/main.json";
import * as dotenv from "dotenv";
import * as path from "path";
// Read environment variables from .env file
dotenv.config({ path: path.join(__dirname, "..", ".env") });

// Initialize RPC connection
const Tezos = new TezosToolkit(process.env.NODE_URL || "");

const deployMain = async () => {
  const storage = {
    user_map: new MichelsonMap(),
    user_blacklist: [],
    admin_list: new MichelsonMap(),
    has_paid: new MichelsonMap(),
};
const op = await Tezos.contract.originate({
      code: mainContract,
      storage: storage,
  });
  await op.confirmation();
  console.log(`Confirmed: ${op.contractAddress}`);
  // check contract storage with CLI
  console.log(
      `tezos-client --endpoint http://localhost:20000 get contract storage for ${op.contractAddress}`
  );
}

const main = async () => {
  const signer = await InMemorySigner.fromSecretKey(
      process.env.ADMIN_SK || ""
  );
  const admin: string = await signer.publicKeyHash();
  Tezos.setProvider({ signer });

  await deployMain();
}

main().catch(e => console.error(e));