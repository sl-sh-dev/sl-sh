#!/bin/bash
set -e

MOUNT_POINT="/tmp/test-eval-fs"
SERVER_BIN="./target/debug/slosh-fuse-server"
PASSED=0
FAILED=0

pass() { PASSED=$((PASSED + 1)); echo "  PASS: $1"; }
fail() { FAILED=$((FAILED + 1)); echo "  FAIL: $1"; }

cleanup() {
    exec 3>&- 2>/dev/null || true
    if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    # AutoUnmount should handle this, but just in case
    fusermount3 -u "$MOUNT_POINT" 2>/dev/null || true
    rm -rf "$MOUNT_POINT"
    rm -f "$FIFO"
    rm -rf /tmp/fuse-concat-test-* 2>/dev/null || true
}
trap cleanup EXIT

echo "=== FUSE Server Test ==="

# Ensure binary exists
if [ ! -f "$SERVER_BIN" ]; then
    echo "Building slosh-fuse-server..."
    cargo build -p slosh-fuse
fi

# Create mount point
mkdir -p "$MOUNT_POINT"

# Start the server with stdin piped from a fifo.
# The server opens the FIFO read end in background, then we open the
# write end. This order avoids a deadlock: opening a FIFO for writing
# blocks until a reader exists, and vice versa.
FIFO="/tmp/fuse-test-fifo-$$"
mkfifo "$FIFO"
"$SERVER_BIN" "$MOUNT_POINT" < "$FIFO" &
SERVER_PID=$!
exec 3>"$FIFO"
echo "Server PID: $SERVER_PID"

# Give the FUSE mount time to come up
sleep 2

# Test 1: Mount exists
echo ""
echo "--- Test 1: Mount point is a FUSE mount ---"
if mountpoint -q "$MOUNT_POINT" 2>/dev/null || mount | grep -q "$MOUNT_POINT"; then
    pass "Mount point is active"
else
    fail "Mount point not detected"
fi

# Test 2: Register a file and read it
echo ""
echo "--- Test 2: Register and read a file ---"
CONTENT="hello from fuse"
B64_CONTENT=$(echo -n "$CONTENT" | base64)
printf "REGISTER\ttest.txt\t%s\n" "$B64_CONTENT" >&3

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

# Test 3: Register a second file
echo ""
echo "--- Test 3: Register a second file ---"
CONTENT2="HOST=myhost"
B64_CONTENT2=$(echo -n "$CONTENT2" | base64)
printf "REGISTER\tconfig.env\t%s\n" "$B64_CONTENT2" >&3

sleep 1

if [ -f "$MOUNT_POINT/config.env" ]; then
    ACTUAL2=$(cat "$MOUNT_POINT/config.env")
    if [ "$ACTUAL2" = "$CONTENT2" ]; then
        pass "config.env content matches"
    else
        fail "config.env content mismatch: expected '$CONTENT2', got '$ACTUAL2'"
    fi
else
    fail "File config.env not found in mount"
fi

# Test 4: Directory listing shows both files
echo ""
echo "--- Test 4: Directory listing ---"
LISTING=$(ls "$MOUNT_POINT")
if echo "$LISTING" | grep -q "test.txt" && echo "$LISTING" | grep -q "config.env"; then
    pass "Both files appear in directory listing"
else
    fail "Directory listing incomplete: $LISTING"
fi

# Test 5: Remove a file
echo ""
echo "--- Test 5: Remove a file ---"
printf "REMOVE\ttest.txt\n" >&3

sleep 1

if [ ! -f "$MOUNT_POINT/test.txt" ]; then
    pass "test.txt removed successfully"
else
    fail "test.txt still exists after REMOVE"
fi

# Test 6: CONCAT - create real files and concatenate them
echo ""
echo "--- Test 6: CONCAT basic concatenation ---"
CONCAT_DIR="/tmp/fuse-concat-test-$$"
mkdir -p "$CONCAT_DIR"
echo -n "AAA" > "$CONCAT_DIR/a.txt"
echo -n "BBB" > "$CONCAT_DIR/b.txt"
echo -n "CCC" > "$CONCAT_DIR/c.txt"

printf "CONCAT\tcombined.txt\t%s/a.txt\t%s/b.txt\t%s/c.txt\n" "$CONCAT_DIR" "$CONCAT_DIR" "$CONCAT_DIR" >&3

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

# Test 7: CONCAT reflects changes to source files
echo ""
echo "--- Test 7: CONCAT reflects source file changes ---"
echo -n "XXX" > "$CONCAT_DIR/b.txt"

ACTUAL=$(cat "$MOUNT_POINT/combined.txt")
if [ "$ACTUAL" = "AAAXXXCCC" ]; then
    pass "CONCAT reflects file change: '$ACTUAL'"
else
    fail "CONCAT did not reflect change: expected 'AAAXXXCCC', got '$ACTUAL'"
fi

# Test 8: grep works across CONCAT virtual file
echo ""
echo "--- Test 8: grep across CONCAT virtual file ---"
printf "line1 apple\n" > "$CONCAT_DIR/a.txt"
printf "line2 banana\n" > "$CONCAT_DIR/b.txt"
printf "line3 cherry\n" > "$CONCAT_DIR/c.txt"

sleep 1

if grep -q "banana" "$MOUNT_POINT/combined.txt"; then
    pass "grep found 'banana' in concatenated file"
else
    fail "grep did not find 'banana' in concatenated file"
fi

if grep -q "cherry" "$MOUNT_POINT/combined.txt"; then
    pass "grep found 'cherry' in concatenated file"
else
    fail "grep did not find 'cherry' in concatenated file"
fi

# Clean up concat test files
rm -rf "$CONCAT_DIR"

# Close the fifo writer fd
exec 3>&-
rm -f "$FIFO"

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="
if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
