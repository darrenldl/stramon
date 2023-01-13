# Changlog

## Stramon 0.2.0

- Stramon now still collects access information even if `kind_of_file` fails

## Stramon-lib 0.3.0

- Adjusted code to be buildable on OCaml 4.08

- Added handling for following syscalls:

  - `socket`

## Stramon-lib 0.2.1

- Documentation and metadata adjustment

## Stramon-lib 0.2.0

- Renamed `File_utils` to `Utils`

- Added following functions to `Path_trie`:

  - `is_empty`

  - `remove`

  - `merge`

  - `union`

  - `equal`

  - `of_seq`

- Added following functions to `Path_trie_set`:

  - `is_empty`

  - `remove`

  - `union`

  - `inter`

  - `equal`

  - `to_seq`

  - `of_seq`

- Renamed `data` in `Monitor_result.t` to `ctx`

- Renamed `init_data` parameter of `monitor` to `init_ctx`

- Removed escaped slash handling logic

## Stramon 0.1.0

- Base version

## Stramon-lib 0.1.0

- Base version
