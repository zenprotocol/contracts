module PK = Consensus.Crypto.PublicKey

let fromString s =
    match Consensus.Hash.fromString s with
    | Some x -> Consensus.Hash.bytes x
    | None -> failwithf "%s isn't a valid hash string" s

//let __derivationPath = "m/44'/258'/0'/3/0"

let oracle_pk_str = "02ad784974b3f86ad97e008e20d2c107429041ed2d991ada2a1461b5077c11944c"

let oracle_pk = PK.fromString oracle_pk_str |> Option.get

let timestamp = 1234UL

let ticker = "ABCD"

let price = 12UL

let leaf = fromString "9b46882c07f5213e938d57de4c5871b82bff5fc7a04a82c06df1b4c4b9001293" |> Hash.Hash

let root = fromString "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> Hash.Hash

let index = 0UL

let strings_path =
    [ "8bedf63712734899064f7c342d38447360ce4d0377cf8b984710b1fd48341a3b"
    ; "0ecb254e1ff36f9b6a09f35926041a01a955171a29d8500775fb58a0acbff54c"
    ]

let path =
    Infrastructure.Option.traverseM Consensus.Hash.fromString strings_path
    |> function
       | Some x -> x
       | None -> failwith "Couldn't parse path"
