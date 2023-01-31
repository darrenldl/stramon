# Changlog

## Stramon 0.3.0

- Added `tid` collection

## Stramon-lib 0.4.0

- Added handling for following syscalls:

  - `fork`
  - `clone`
  - `clone3`

## Stramon 0.2.2

- Fixed `stramon-latest.json` symlink update

## Stramon 0.2.1

- Added `--version` flag

## Stramon 0.2.0

- Stramon now still collects access information even if `kind_of_file` fails

- Depends on Stramon-lib >= 0.3.0

## Stramon-lib 0.3.0

- Adjusted code to be buildable on OCaml 4.08

- Added handling for following syscalls:

  - `socket`
  - `chown`
  - `fchown`
  - `lchown`
  - `fchownat`
  - `chmod`
  - `fchmod`
  - `fchmodat`
  - `stat`
  - `fstat`
  - `lstat`
  - `fstatat64`
  - `newfstatat`
  - `statx`
  - `accept`
  - `connect`
  - `open`
  - `bind`
  - `listen`

- Added `--seccomp-bpf` flag to strace invocation

- Added `--trace` argument to specify syscalls to capture

  - Removed debug level `All` as a result

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
