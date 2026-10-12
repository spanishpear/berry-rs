#!/usr/bin/env bash
# Compare two berry parser trees on one lockfile: equivalence first, then speed.
#
#   scripts/bench-compare.sh <lockfile> [options]
#
# 1. builds BASELINE_DIR and CANDIDATE_DIR in --release,
# 2. dumps every parsed field (berry-dump-bin, canonical format) from both and
#    diffs them: ANY difference makes the script exit 1,
# 3. runs hyperfine (process level) and berry-parse-bench (in-process, parse
#    only: min/median/mean, MB/s, peak RSS) on both, interleaved,
# 4. with --alloc, builds a counting-allocator variant of both and reports
#    allocation counts / bytes / peak live heap.
#
# Options:
#   --runs N          in-process iterations per round (default 20)
#   --rounds N        interleaved baseline/candidate rounds (default 3)
#   --alloc           also report allocation counts (separate build)
#   --no-speed        only build + equivalence diff
#   --show-diff       print an excerpt of the first dump differences
#                     (off by default: dumps contain names from the lockfile)
#   --keep            keep the dump files (default: deleted on exit)
#   -h, --help
#
# Environment:
#   BASELINE_DIR   worktree with the reference parser
#                  (default /home/ubuntu/contrib/berry-wt/baseline)
#   CANDIDATE_DIR  worktree under test (default: the repo containing this script)
#   WORK_DIR       scratch directory for dumps (default: a mktemp dir)
#   RUSTFLAGS      forwarded to cargo, e.g. "-Ctarget-cpu=x86-64-v2" to also
#                  measure without the repo's target-cpu=native setting
#   CANDIDATE_ONCE_ARGS
#                  extra berry-parse-bench args for the candidate's hyperfine
#                  run (default "--api file": berry::parse_file, i.e. mmap +
#                  parallel validation; the baseline is always
#                  read_to_string + parse_lockfile). Set to "" for
#                  read_to_string + berry::parse.
#
# Exit status: 0 = outputs identical; 1 = outputs differ; 2 = usage/build error.
set -euo pipefail

usage() { sed -n '2,/^set -euo/p' "$0" | sed '$d' | sed 's/^# \{0,1\}//'; }

lockfile=""
runs=20
rounds=3
do_alloc=0
do_speed=1
show_diff=0
keep=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --runs) runs="$2"; shift 2 ;;
    --rounds) rounds="$2"; shift 2 ;;
    --alloc) do_alloc=1; shift ;;
    --no-speed) do_speed=0; shift ;;
    --show-diff) show_diff=1; shift ;;
    --keep) keep=1; shift ;;
    -h|--help) usage; exit 0 ;;
    -*) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
    *) if [[ -n "$lockfile" ]]; then echo "only one lockfile allowed" >&2; exit 2; fi
       lockfile="$1"; shift ;;
  esac
done
if [[ -z "$lockfile" ]]; then usage >&2; exit 2; fi
if [[ ! -f "$lockfile" ]]; then echo "no such file: $lockfile" >&2; exit 2; fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASELINE_DIR="${BASELINE_DIR:-/home/ubuntu/contrib/berry-wt/baseline}"
CANDIDATE_DIR="${CANDIDATE_DIR:-$(cd "$script_dir/.." && pwd)}"
for d in "$BASELINE_DIR" "$CANDIDATE_DIR"; do
  [[ -f "$d/Cargo.toml" ]] || { echo "not a cargo workspace: $d" >&2; exit 2; }
done
for tool in cargo jq; do
  command -v "$tool" >/dev/null || { echo "missing tool: $tool" >&2; exit 2; }
done
if [[ $do_speed -eq 1 ]]; then
  command -v hyperfine >/dev/null || { echo "missing tool: hyperfine (or use --no-speed)" >&2; exit 2; }
fi

lockfile="$(realpath "$lockfile")"
work="${WORK_DIR:-$(mktemp -d)}"
mkdir -p "$work"
if [[ $keep -eq 0 ]]; then trap 'rm -rf "$work"' EXIT; fi

build() { # dir [extra cargo args...]
  local dir="$1"; shift
  (cd "$dir" && cargo build --release -q -p berry-dump-bin -p berry-bench-bin "$@")
}

echo "== building (release) =="
echo "baseline : $BASELINE_DIR ($(git -C "$BASELINE_DIR" rev-parse --short HEAD 2>/dev/null || echo '?'))"
echo "candidate: $CANDIDATE_DIR ($(git -C "$CANDIDATE_DIR" rev-parse --short HEAD 2>/dev/null || echo '?'))"
build "$BASELINE_DIR"
build "$CANDIDATE_DIR"

dump_base="$BASELINE_DIR/target/release/berry-dump-bin"
dump_cand="$CANDIDATE_DIR/target/release/berry-dump-bin"
bench_base="$BASELINE_DIR/target/release/berry-parse-bench"
bench_cand="$CANDIDATE_DIR/target/release/berry-parse-bench"

echo
echo "== equivalence: canonical dump of every parsed field =="
set +e
"$dump_base" "$lockfile" -o "$work/base.dump" 2>"$work/base.err"; base_rc=$?
"$dump_cand" "$lockfile" -o "$work/cand.dump" 2>"$work/cand.err"; cand_rc=$?
set -e
status=0
if [[ $base_rc -ne $cand_rc ]]; then
  echo "DIFF: exit codes differ (baseline $base_rc, candidate $cand_rc)"
  status=1
fi
if [[ $base_rc -ne 0 || $cand_rc -ne 0 ]]; then
  # No dump is written on a parse failure; equivalence means "fails the same way".
  if [[ $base_rc -eq $cand_rc ]]; then
    echo "both parsers fail identically (exit code $base_rc); nothing to diff"
  fi
elif cmp -s "$work/base.dump" "$work/cand.dump"; then
  echo "dump identical: $(stat -c %s "$work/base.dump") bytes, sha256 $(sha256sum < "$work/base.dump" | cut -c1-16), exit code 0"
else
  status=1
  echo "DIFF: canonical dumps differ"
  n=$(diff "$work/base.dump" "$work/cand.dump" | grep -c '^[<>]' || true)
  first=$(cmp "$work/base.dump" "$work/cand.dump" 2>&1 | sed -n 's/.*line \([0-9]*\).*/\1/p' | head -1) || true
  echo "  $n differing lines, first at line ${first:-?}"
  if [[ $show_diff -eq 1 ]]; then diff "$work/base.dump" "$work/cand.dump" | head -60 || true; fi
fi
if [[ $base_rc -ne 0 || $cand_rc -ne 0 ]]; then do_speed=0; do_alloc=0; fi
if ! cmp -s "$work/base.err" "$work/cand.err"; then
  echo "note: stderr differs (e.g. error text); not counted as a diff"
fi

if [[ $do_speed -eq 1 ]]; then
  echo
  cand_once_args="${CANDIDATE_ONCE_ARGS---api file}"
  echo "== hyperfine (process level: read + parse + drop, one parse per process; candidate args: ${cand_once_args:-none}) =="
  hyperfine --warmup 2 --min-runs 10 -N \
    -n baseline "$bench_base --once $lockfile" \
    -n candidate "$bench_cand --once $cand_once_args $lockfile"

  echo
  echo "== in-process parse only ($runs iters x $rounds interleaved rounds, drop excluded) =="
  for r in $(seq 1 "$rounds"); do
    for which in baseline candidate; do
      bin="$bench_base"; [[ $which == candidate ]] && bin="$bench_cand"
      "$bin" "$lockfile" -n "$runs" --json | jq -r --arg w "$which" --argjson r "$r" \
        '"round \($r) \($w|.+" "*(9-length)): min \(.parse.min_ms*1000|round/1000) ms  median \(.parse.median_ms*1000|round/1000) ms  mean \(.parse.mean_ms*1000|round/1000) ms  \(.parse.mb_per_s_median*10|round/10) MB/s  peak RSS \((.peak_rss_bytes/1048576*10|round/10)) MiB"'
    done
  done
  # Final summary from the medians of the last round pair, as a delta.
  b=$("$bench_base" "$lockfile" -n "$runs" --json | jq .parse.median_ms)
  c=$("$bench_cand" "$lockfile" -n "$runs" --json | jq .parse.median_ms)
  jq -nr --argjson b "$b" --argjson c "$c" \
    '"median parse: baseline \($b*1000|round/1000) ms, candidate \($c*1000|round/1000) ms, delta \(($c-$b)/$b*10000|round/100)% (negative = faster)"'
fi

if [[ $do_alloc -eq 1 ]]; then
  echo
  echo "== allocations (counting allocator build; separate target dir) =="
  for pair in "baseline:$BASELINE_DIR" "candidate:$CANDIDATE_DIR"; do
    name="${pair%%:*}"; dir="${pair#*:}"
    (cd "$dir" && CARGO_TARGET_DIR="$dir/target/alloc-count" \
      cargo build --release -q -p berry-bench-bin --features alloc-count)
    "$dir/target/alloc-count/release/berry-parse-bench" "$lockfile" -n 1 -w 0 --alloc --json \
      | jq -r --arg w "$name" \
        '"\($w|.+" "*(9-length)): \(.alloc.allocs) allocs + \(.alloc.reallocs) reallocs, \(.alloc.bytes_allocated) bytes allocated, peak live \(.alloc.peak_live_bytes_during_parse) bytes, retained \(.alloc.retained_bytes_after_parse) bytes"'
  done
fi

echo
if [[ $status -eq 0 ]]; then echo "RESULT: outputs identical"; else echo "RESULT: OUTPUTS DIFFER"; fi
exit $status
