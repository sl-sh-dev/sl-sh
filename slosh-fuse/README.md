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

Content is **evaluated at registration time** in the parent slosh process, then sent as static bytes to a child FUSE server process via stdin (base64-encoded, tab-delimited protocol). This avoids the complexity of IPC-based expression evaluation since the Slosh VM is `thread_local!` and can't be accessed from child processes.

Components:
1. **slosh-fuse crate** - FUSE filesystem serving static file content, plus the `FuseMount` parent-side handle
2. **slosh-fuse-server binary** - Child process that mounts FUSE and reads `REGISTER`/`REMOVE` commands from stdin
3. **Builtin functions** (in slosh_lib) - `mount-eval-fs`, `register-eval-file`, `unmount-eval-fs`, `list-eval-files`, `list-mounts`

## Usage

```lisp
;; Mount a dynamic filesystem
(def mount-id (mount-eval-fs "/tmp/dynamic"))

;; Register a file with content (evaluated now, served statically)
(register-eval-file mount-id "config.env"
  (str "HOST=" (hostname) "\n" "TYPE=" *my-global-var* "\n"))

;; The file /tmp/dynamic/config.env is now readable

;; To update content, re-register:
(register-eval-file mount-id "config.env"
  (str "HOST=" (hostname) "\n" "TYPE=" *my-global-var* "\n"))

;; Unmount when done
(unmount-eval-fs mount-id)
```

## Building

Requires Linux with libfuse3 (FUSE doesn't work natively on macOS).

```bash
# On Linux
cargo build -p slosh-fuse
cargo build -p slosh_lib --features fuse

# On macOS, use an Apple Container
container build -t slosh-fuse -f Containerfile .
container run --name slosh-fuse slosh-fuse bash /src/slosh-fuse/test-fuse.sh
```

## Testing

```bash
# Standalone FUSE server test (inside container or on Linux)
bash slosh-fuse/test-fuse.sh
```

## Protocol

The parent communicates with the FUSE server child via stdin using tab-delimited lines:

```
REGISTER\t<path>\t<base64-content>\n
REMOVE\t<path>\n
```

The server uses `AutoUnmount` so the filesystem is automatically unmounted when the server process exits.
