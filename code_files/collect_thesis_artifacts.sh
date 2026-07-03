#!/usr/bin/env bash
# =============================================================================
# collect_thesis_artifacts.sh  --  Give the final-run outputs a tracked home
# =============================================================================
# The benchmark runners write under code_files/results/, which is git-ignored
# (it is scratch space, regenerated at will). The thesis, however, needs a
# versioned copy of the final numbers and figures. This script publishes one
# completed run of run_all_final.sh (or the EC2 tarball) into two tracked
# places:
#
#   1. final_results/            -- the FULL results tree, copied verbatim
#      (all_runs.csv, summary_curve.csv, final_summary.csv,
#      paired_vs_random.csv, convergence plots, per objective). This is the
#      provenance record: the exact numbers behind the thesis.
#
#   2. written_files/tesis_escrito/Figures/conv_<label>.png -- each
#      convergence plot under a STABLE name that the LaTeX source references,
#      so re-running the collector after a new run refreshes every figure
#      without touching the .tex files.
#
# Usage (from the repository root):
#   bash code_files/collect_thesis_artifacts.sh
#   RESULTS_ROOT=/path/to/untarred/results bash code_files/collect_thesis_artifacts.sh
#
# Exits non-zero if any expected artifact is missing, listing each one, so a
# partially failed run is loud rather than silently under-reported.
# =============================================================================
set -uo pipefail

cd "$(dirname "$0")/.."   # repository root

RESULTS_ROOT="${RESULTS_ROOT:-code_files/results}"
DEST_TREE="final_results"
DEST_FIGS="written_files/tesis_escrito/Figures"

if [ ! -d "$RESULTS_ROOT" ]; then
  echo "ERROR: results root '$RESULTS_ROOT' not found. Run the benchmarks first" >&2
  echo "(bash code_files/run_all_final.sh) or point RESULTS_ROOT at the results" >&2
  echo "directory from the EC2 tarball." >&2
  exit 1
fi

# label:relative-source pairs. The label doubles as the stable figure name
# (Figures/conv_<label>.png). Keep in sync with run_all_final.sh.
MAPPINGS="
branin:branin
rastrigin:rastrigin
synthetic:synthetic
func2C:func2C
func3C:func3C
cat_ackley_easy:cat_ackley_d3_L5
cat_ackley_medium:cat_ackley_d4_L7
cat_ackley_hard:cat_ackley_d6_L11
tpe_gamma_branin:tpe_sensitivity/branin
tpe_gamma_cat_ackley:tpe_sensitivity/cat_ackley
elastic_net:elastic_net
"

mkdir -p "$DEST_TREE" "$DEST_FIGS"

missing=0
copied=0

# 1) Full tree, verbatim.
echo "Publishing full results tree: $RESULTS_ROOT -> $DEST_TREE/"
cp -a "$RESULTS_ROOT/." "$DEST_TREE/"

# 2) Stable figure names for LaTeX.
for map in $MAPPINGS; do
  label="${map%%:*}"
  src="$RESULTS_ROOT/${map#*:}/convergence_mean_ci.png"
  if [ -f "$src" ]; then
    cp -f "$src" "$DEST_FIGS/conv_${label}.png"
    echo "  Figures/conv_${label}.png  <-  ${map#*:}"
    copied=$((copied + 1))
  else
    echo "MISSING: $src (expected for conv_${label}.png)" >&2
    missing=$((missing + 1))
  fi
done

echo ""
echo "Copied $copied convergence plots; $missing missing."
echo "Tracked outputs: $DEST_TREE/ and $DEST_FIGS/conv_*.png -- review and commit."
[ "$missing" -eq 0 ]
