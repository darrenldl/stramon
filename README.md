# Stramon
Process behavior monitoring utility and library based on strace

## Installation

Stramon is primarily distributed as a statically linked binary
in releases right now.

#### Building from source

Easiest way is to first build a container image using the Dockerfile under `containers/static-build`

```
$ cd containers/static-build
$ podman build . -t stramon
```

Then run `start-container.sh` to start and enter the container, navigate to the
mounted repo location within the container and use the static build make command

```
$ ./start-container.sh
# cd /root/stramon # now we are inside the container
# make release-static
```

The statically linked binary should now be available as `static-build/stramon`

## Usage

To trace a command:
```
$ stramon CMD
```

`stramon-latest.json` symlink is updated to point
to the newest JSON file

```
$ cat stramon-latest.json | jq "."
```

## Help message

```
$ straamon --help
stramon [-o JSON_OUTPUT] -- prog arg ...
  -o JSON file output path, defaults to: stramon_DATE-TIME.json.
If provided path PATH is a directory, then output path is PATH/stramon_DATE-TIME.json
  -f Force overwrite of output file
  --no-link Disable adding/updating symlink stramon-latest.json
  --version Print version and exit
  --debug Enable debugging output to OUTPUT_PATH.debug
  --raw Copy received strace output to OUTPUT_PATH.raw
  -help  Display this list of options
  --help  Display this list of options
```

## Example JSON files

- Firefox: `examples/firefox.json`
