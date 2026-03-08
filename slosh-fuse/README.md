# Slosh FUSE Integration

FUSE (Filesystem in Userspace) integration for slosh. Creates virtual files in a mounted directory whose contents come from slosh expressions evaluated at registration time.

## Use Case

Dynamic configuration files for services like systemd, where environment files need values from slosh variables or computed values:

```bash
cat /tmp/dynamic/config.env
# => HOST=meow
# => CUSTOM_TYPE=production
```

## Architecture

A single per-user FUSE daemon manages all mounts. The daemon:
- Is the slosh binary itself, started with `--fuse-daemon`
- Manages multiple mount points (one FUSE session thread per mount)
- Accepts connections from multiple REPL instances via Unix domain socket
- Auto-starts when a REPL first needs it
- Detaches from terminal (double-fork + setsid)
- Uses a PID file with advisory lock for single-instance enforcement

Content is **evaluated at registration time** in the REPL, then sent as static bytes to the daemon via Unix socket (base64-encoded, tab-delimited protocol).

Components:
1. **slosh-fuse crate** - FUSE filesystem, daemon core, socket server, and client library
2. **Builtin functions** (in slosh_lib) - `mount-eval-fs`, `register-eval-file`, `unmount-eval-fs`, `concat-eval-files`, `list-eval-files`, `list-mounts`

Runtime files (under `$HOME/.local/share/slosh/`):
- `fuse-daemon.pid` - PID file with advisory lock
- `fuse.sock` - Unix domain socket
- `fuse-daemon.log` - Daemon log output

## Usage

```lisp
;; Mount a dynamic filesystem (auto-starts daemon if needed)
(def mount-id (mount-eval-fs "/tmp/dynamic"))

;; Register a file with content (evaluated now, served statically)
(register-eval-file mount-id "config.env"
  (str "HOST=" (hostname) "\n" "TYPE=" *my-global-var* "\n"))

;; The file /tmp/dynamic/config.env is now readable

;; Concatenate multiple real files as a single virtual file
(concat-eval-files mount-id "combined.txt" "/path/a.txt" "/path/b.txt")

;; Unmount when done
(unmount-eval-fs mount-id)
```

## Building

Requires Linux with libfuse3 (FUSE doesn't work natively on macOS).

```bash
# On Linux
cargo build -p slosh --features fuse
```

### Containers (for macOS or CI)

Two Containerfiles live in `slosh-fuse/`. Both are built from the **repo root** and
run as a non-root `slosh` user with `~/.config` and `~/.local/share` pre-created.

#### `Containerfile` — active development

Source code is **volume-mounted** at runtime, not copied into the image. Edits are
reflected instantly and incremental builds are fast. Includes `socat` for socket debugging.

```bash
# Build the toolchain image (once, or after changing the Containerfile)
docker build -t dyn-slosh-fuse -f slosh-fuse/Containerfile .

# Run unit tests (no FUSE device needed)
docker run --rm -v "$PWD:/home/slosh/src" dyn-slosh-fuse cargo test -p slosh-fuse

# Run the full integration test
docker run --rm -v "$PWD:/home/slosh/src" --cap-add SYS_ADMIN --device /dev/fuse \
    dyn-slosh-fuse bash slosh-fuse/test-fuse.sh

# Interactive shell for debugging
docker run --rm -it -v "$PWD:/home/slosh/src" --cap-add SYS_ADMIN --device /dev/fuse \
    dyn-slosh-fuse bash
```

#### `Containerfile_static` — stable/CI testing

Source is **copied into the image** and compiled during `docker build`. The resulting
image is self-contained. Slower to rebuild after edits, but useful for testing a stable
version more robustly or in CI where you want a reproducible artifact.

```bash
# Build (copies source, compiles everything)
docker build -t slosh-fuse-static -f slosh-fuse/Containerfile_static .

# Run unit tests
docker run --rm slosh-fuse-static cargo test -p slosh-fuse

# Run the full integration test
docker run --rm --cap-add SYS_ADMIN --device /dev/fuse \
    slosh-fuse-static bash slosh-fuse/test-fuse.sh
```

## Testing

```bash
# Daemon test (inside container or on Linux, requires socat)
bash slosh-fuse/test-fuse.sh

# Manual daemon testing
slosh --fuse-daemon-foreground &
echo -e "PING" | socat - UNIX-CONNECT:$HOME/.local/share/slosh/fuse.sock
```

## Protocol

The REPL communicates with the daemon via Unix domain socket using tab-delimited request/response lines:

```
Request:                                          Response:
MOUNT\t<mount_point>\n                         -> OK\t<mount-id>\n
REGISTER\t<mount-id>\t<path>\t<base64>\n       -> OK\n
CONCAT\t<mount-id>\t<vpath>\t<p1>\t<p2>...\n   -> OK\n
REMOVE\t<mount-id>\t<path>\n                   -> OK\n
UNMOUNT\t<mount-id>\n                          -> OK\n
LIST\t<mount-id>\n                             -> OK\t<path1>\t<path2>...\n
PING\n                                         -> OK\tPONG\n
(any error)                                    -> ERR\t<message>\n
```
