#!/bin/bash
set -e

MOUNT_POINT="/tmp/test-eval-fs"
SLOSH_BIN="./target/debug/slosh"
PASSED=0
FAILED=0

pass() { PASSED=$((PASSED + 1)); echo "  PASS: $1"; }
fail() { FAILED=$((FAILED + 1)); echo "  FAIL: $1"; }

cleanup() {
    # Stop daemon if we started it
    if [ -n "$DAEMON_PID" ] && kill -0 "$DAEMON_PID" 2>/dev/null; then
        kill "$DAEMON_PID" 2>/dev/null
        wait "$DAEMON_PID" 2>/dev/null || true
    fi
    fusermount3 -u "$MOUNT_POINT" 2>/dev/null || true
    rm -rf "$MOUNT_POINT"
    rm -rf /tmp/fuse-concat-test-* 2>/dev/null || true
    # Clean up daemon socket/pid
    rm -f "$HOME/.local/share/slosh/fuse.sock" 2>/dev/null || true
    rm -f "$HOME/.local/share/slosh/fuse-daemon.pid" 2>/dev/null || true
}
trap cleanup EXIT

echo "=== FUSE Daemon Test ==="

# Ensure binary exists
if [ ! -f "$SLOSH_BIN" ]; then
    echo "Building slosh with fuse feature..."
    cargo build -p slosh --features fuse
fi

# Start daemon in foreground mode (background it ourselves)
"$SLOSH_BIN" --fuse-daemon-foreground &
DAEMON_PID=$!
echo "Daemon PID: $DAEMON_PID"

# Wait for socket to appear
SOCK="$HOME/.local/share/slosh/fuse.sock"
for i in $(seq 1 30); do
    if [ -S "$SOCK" ]; then
        break
    fi
    sleep 0.2
done

if [ ! -S "$SOCK" ]; then
    fail "Daemon socket did not appear"
    exit 1
fi

# Create mount point
mkdir -p "$MOUNT_POINT"

# Test 1: PING
echo ""
echo "--- Test 1: PING ---"
REPLY=$(echo -e "PING" | socat - UNIX-CONNECT:"$SOCK")
if [ "$REPLY" = "OK	PONG" ]; then
    pass "PING/PONG works"
else
    fail "PING response: '$REPLY'"
fi

# Test 2: MOUNT
echo ""
echo "--- Test 2: MOUNT ---"
MOUNT_ID=$(echo -e "MOUNT\t$MOUNT_POINT" | socat - UNIX-CONNECT:"$SOCK" | cut -f2)
if [ -n "$MOUNT_ID" ] && [ "$MOUNT_ID" != "" ]; then
    pass "Got mount ID: $MOUNT_ID"
else
    fail "No mount ID returned"
fi

# Give FUSE mount time to come up
sleep 2

# Test 3: Mount point is active
echo ""
echo "--- Test 3: Mount point is active ---"
if mountpoint -q "$MOUNT_POINT" 2>/dev/null || mount | grep -q "$MOUNT_POINT"; then
    pass "Mount point is active"
else
    fail "Mount point not detected"
fi

# Test 4: Register a file and read it
echo ""
echo "--- Test 4: Register and read a file ---"
CONTENT="hello from fuse daemon"
B64_CONTENT=$(echo -n "$CONTENT" | base64)
REPLY=$(echo -e "REGISTER\t$MOUNT_ID\ttest.txt\t$B64_CONTENT" | socat - UNIX-CONNECT:"$SOCK")
if [ "$REPLY" = "OK" ]; then
    sleep 1
    if [ -f "$MOUNT_POINT/test.txt" ]; then
        ACTUAL=$(cat "$MOUNT_POINT/test.txt")
        if [ "$ACTUAL" = "$CONTENT" ]; then
            pass "File content matches: '$ACTUAL'"
        else
            fail "Content mismatch: expected '$CONTENT', got '$ACTUAL'"
        fi
    else
        fail "File test.txt not found in mount"
    fi
else
    fail "REGISTER failed: $REPLY"
fi

# Test 5: LIST files
echo ""
echo "--- Test 5: LIST files ---"
REPLY=$(echo -e "LIST\t$MOUNT_ID" | socat - UNIX-CONNECT:"$SOCK")
if echo "$REPLY" | grep -q "test.txt"; then
    pass "LIST includes test.txt"
else
    fail "LIST response: '$REPLY'"
fi

# Test 6: REMOVE a file
echo ""
echo "--- Test 6: REMOVE a file ---"
REPLY=$(echo -e "REMOVE\t$MOUNT_ID\ttest.txt" | socat - UNIX-CONNECT:"$SOCK")
if [ "$REPLY" = "OK" ]; then
    sleep 1
    if [ ! -f "$MOUNT_POINT/test.txt" ]; then
        pass "test.txt removed successfully"
    else
        fail "test.txt still exists after REMOVE"
    fi
else
    fail "REMOVE failed: $REPLY"
fi

# Test 7: CONCAT
echo ""
echo "--- Test 7: CONCAT basic concatenation ---"
CONCAT_DIR="/tmp/fuse-concat-test-$$"
mkdir -p "$CONCAT_DIR"
echo -n "AAA" > "$CONCAT_DIR/a.txt"
echo -n "BBB" > "$CONCAT_DIR/b.txt"
echo -n "CCC" > "$CONCAT_DIR/c.txt"

REPLY=$(echo -e "CONCAT\t$MOUNT_ID\tcombined.txt\t$CONCAT_DIR/a.txt\t$CONCAT_DIR/b.txt\t$CONCAT_DIR/c.txt" | socat - UNIX-CONNECT:"$SOCK")
if [ "$REPLY" = "OK" ]; then
    sleep 1
    if [ -f "$MOUNT_POINT/combined.txt" ]; then
        ACTUAL=$(cat "$MOUNT_POINT/combined.txt")
        if [ "$ACTUAL" = "AAABBBCCC" ]; then
            pass "CONCAT content matches: '$ACTUAL'"
        else
            fail "CONCAT content mismatch: expected 'AAABBBCCC', got '$ACTUAL'"
        fi
    else
        fail "combined.txt not found in mount"
    fi
else
    fail "CONCAT failed: $REPLY"
fi

# Test 8: UNMOUNT
echo ""
echo "--- Test 8: UNMOUNT ---"
REPLY=$(echo -e "UNMOUNT\t$MOUNT_ID" | socat - UNIX-CONNECT:"$SOCK")
if [ "$REPLY" = "OK" ]; then
    pass "UNMOUNT succeeded"
else
    fail "UNMOUNT failed: $REPLY"
fi

# Clean up concat test files
rm -rf "$CONCAT_DIR"

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="
if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
