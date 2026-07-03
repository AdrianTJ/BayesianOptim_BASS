#!/usr/bin/env bash
# =============================================================================
# run_on_ec2.sh  --  One-shot, self-contained runner for a remote Linux box
# =============================================================================
# Provisions a fresh Linux EC2 instance, clones the repo (BRANCH defaults to
# main; override with BRANCH=...), and runs EVERYTHING we need in one go:
#
#   1. The full thesis benchmark suite (continuous + categorical + TPE
#      sensitivity + Elastic Net), exactly the headline protocol.
#   2. An extensive BASS "why does it trail on categoricals?" investigation:
#      acquisition (EI vs Thompson) x posterior resolution (fast vs rich MCMC)
#      x evaluation budget (read off a single long run) x objective and
#      categorical dimension. GP-BO and Random are run as references.
#
# All CSVs, plots, logs, package versions, hardware info and timings are
# collected under one results tree and tar-gzipped at the end for download.
#
# -----------------------------------------------------------------------------
# TESTED TARGET: Ubuntu 22.04 / 24.04 (the common EC2 AMIs). A best-effort
# Amazon Linux / RHEL (dnf) path is included but Ubuntu is first-class.
#
# RECOMMENDED INSTANCE: compute-optimised with many vCPUs and >=16 GB RAM,
# e.g. c6i.8xlarge (32 vCPU) or c6i.16xlarge (64 vCPU). Seeds fan out across
# cores, so wall-clock scales almost linearly with vCPU count. Rough guide at
# the defaults below on 32 vCPU: full suite ~1-2 h, diagnostics ~3-6 h.
#
# -----------------------------------------------------------------------------
# USAGE
#   # Private repo (default): supply a GitHub token with 'repo' scope.
#   GITHUB_TOKEN=ghp_xxx bash run_on_ec2.sh
#
#   # ...or if you scp'd the repo onto the box already:
#   LOCAL_REPO=$HOME/BayesianOptim_BASS bash run_on_ec2.sh
#
#   # Validate the whole pipeline end-to-end in ~10 min before the real run:
#   SMOKE=1 GITHUB_TOKEN=ghp_xxx bash run_on_ec2.sh
#
#   # Skip parts (e.g. environment already set up):
#   RUN_SETUP=0 GITHUB_TOKEN=ghp_xxx bash run_on_ec2.sh
#
# Run it under tmux/screen (or nohup) so an SSH drop doesn't kill the job:
#   tmux new -s bass 'GITHUB_TOKEN=ghp_xxx bash run_on_ec2.sh; bash'
# =============================================================================

set -uo pipefail

# --- Configuration (all env-overridable) -------------------------------------
REPO_URL="${REPO_URL:-https://github.com/AdrianTJ/BayesianOptim_BASS.git}"
BRANCH="${BRANCH:-main}"
LOCAL_REPO="${LOCAL_REPO:-}"          # if set, copy from here instead of cloning
GITHUB_TOKEN="${GITHUB_TOKEN:-}"      # for private https clone
WORKDIR="${WORKDIR:-$HOME/bass-run}"

# Headline benchmark protocol (matches the thesis: budget 80, 25 seeds).
REPS="${REPS:-25}"
BUDGET="${BUDGET:-80}"
WITH_TPE="${WITH_TPE:-true}"
ENET_REPS="${ENET_REPS:-50}"
ENET_BUDGET="${ENET_BUDGET:-100}"

# Diagnostics protocol. DIAG_BUDGET is run once and sliced at 80 & DIAG_BUDGET,
# so the evaluation-budget axis is free.
DIAG_REPS="${DIAG_REPS:-20}"
DIAG_BUDGET="${DIAG_BUDGET:-160}"
# "rich" posterior resolution for BASS (default fast is 4000/2000/10 from the lib).
export DIAG_RICH_NMCMC="${DIAG_RICH_NMCMC:-12000}"
export DIAG_RICH_NBURN="${DIAG_RICH_NBURN:-6000}"
export DIAG_RICH_THIN="${DIAG_RICH_THIN:-10}"
export DIAG_INCLUDE_FULL="${DIAG_INCLUDE_FULL:-1}"    # cat_ackley d6 + func3C, all 4 BASS variants
export DIAG_INCLUDE_LIGHT="${DIAG_INCLUDE_LIGHT:-1}"  # cat_ackley d4/d8 + func2C, EI variants only

# Stage toggles.
RUN_SETUP="${RUN_SETUP:-1}"
RUN_SUITE="${RUN_SUITE:-1}"
RUN_DIAG="${RUN_DIAG:-1}"

# SMOKE=1 shrinks everything for a fast end-to-end validation.
if [ "${SMOKE:-0}" = "1" ]; then
  REPS=2; BUDGET=8; ENET_REPS=2; ENET_BUDGET=6
  DIAG_REPS=2; DIAG_BUDGET=12
  export DIAG_RICH_NMCMC=2000 DIAG_RICH_NBURN=1000 DIAG_RICH_THIN=10
fi

# --- Plumbing ----------------------------------------------------------------
mkdir -p "$WORKDIR"
LOG="$WORKDIR/run.log"
# Mirror all stdout/stderr to the run log.
exec > >(tee -a "$LOG") 2>&1

if [ "$(id -u)" -eq 0 ]; then SUDO=""; else SUDO="sudo"; fi

ts()  { date +"%Y-%m-%dT%H:%M:%S%z"; }
log() { echo ""; echo "[$(ts)] === $* ==="; }

# run_step "name" cmd...  -- time it, log it, and KEEP GOING on failure so one
# bad objective never throws away a multi-hour job. Records outcomes in STEPLOG.
STEPLOG="$WORKDIR/steps.tsv"
run_step() {
  local name="$1"; shift
  local start end rc
  log "START: $name"
  start=$(date +%s)
  "$@"; rc=$?
  end=$(date +%s)
  printf '%s\t%s\t%ss\trc=%s\n' "$(ts)" "$name" "$((end-start))" "$rc" >> "$STEPLOG"
  if [ "$rc" -ne 0 ]; then
    log "FAILED (rc=$rc, ${name}) -- continuing"
  else
    log "OK ($((end-start))s): $name"
  fi
  return 0
}

log "Config: REPS=$REPS BUDGET=$BUDGET DIAG_REPS=$DIAG_REPS DIAG_BUDGET=$DIAG_BUDGET WITH_TPE=$WITH_TPE SMOKE=${SMOKE:-0}"
log "WORKDIR=$WORKDIR"

# =============================================================================
# 1. System dependencies + R + Python
# =============================================================================
PKG_MGR="unknown"
command -v apt-get >/dev/null 2>&1 && PKG_MGR="apt"
command -v dnf     >/dev/null 2>&1 && PKG_MGR="${PKG_MGR/unknown/dnf}"
command -v yum     >/dev/null 2>&1 && [ "$PKG_MGR" = "unknown" ] && PKG_MGR="yum"

install_system_deps() {
  if [ "$PKG_MGR" = "apt" ]; then
    export DEBIAN_FRONTEND=noninteractive
    $SUDO apt-get update -y
    $SUDO apt-get install -y --no-install-recommends \
      build-essential gfortran ca-certificates curl git \
      r-base r-base-dev \
      libcurl4-openssl-dev libssl-dev libxml2-dev \
      libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
      libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
      python3 python3-pip python3-venv
  elif [ "$PKG_MGR" = "dnf" ] || [ "$PKG_MGR" = "yum" ]; then
    echo "WARNING: non-apt system; this path is best-effort (Ubuntu is first-class)."
    $SUDO "$PKG_MGR" install -y \
      gcc gcc-c++ gcc-gfortran make git curl \
      R R-devel \
      libcurl-devel openssl-devel libxml2-devel \
      fontconfig-devel harfbuzz-devel fribidi-devel \
      freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel \
      python3 python3-pip || true
  else
    echo "ERROR: no supported package manager (apt/dnf/yum) found."; return 1
  fi
}

install_r_packages() {
  # Use Posit Public Package Manager BINARY builds on Ubuntu -- this turns a
  # ~40 min source compile of tidyverse et al. into a couple of minutes.
  local codename="" repo="https://cloud.r-project.org"
  if [ "$PKG_MGR" = "apt" ] && [ -r /etc/os-release ]; then
    codename="$(. /etc/os-release; echo "${VERSION_CODENAME:-}")"
    case "$codename" in
      jammy|noble|focal|bookworm|bullseye)
        repo="https://packagemanager.posit.co/cran/__linux__/${codename}/latest" ;;
    esac
  fi
  echo "R package repo: $repo"
  Rscript --no-save - "$repo" <<'R_EOF'
args <- commandArgs(trailingOnly = TRUE)
repo <- args[[1]]
# Make PPM serve binaries matched to this R/distro.
options(
  repos = c(CRAN = repo),
  HTTPUserAgent = sprintf(
    "R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
  ),
  Ncpus = max(1L, parallel::detectCores())
)
pkgs <- c("tidyverse", "lhs", "BASS", "GPfit", "future", "furrr",
          "testthat", "glmnet", "reticulate")  # MASS ships with R
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) {
  cat("Installing:", paste(need, collapse = ", "), "\n")
  install.packages(need)
}
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) { cat("STILL MISSING:", paste(miss, collapse = ", "), "\n"); quit(status = 1) }
cat("All R packages present.\n")
R_EOF
}

install_python_optuna() {
  local venv="$WORKDIR/optuna-venv"
  if [ ! -x "$venv/bin/python" ]; then
    python3 -m venv "$venv"
  fi
  "$venv/bin/pip" install --upgrade pip >/dev/null
  "$venv/bin/pip" install "optuna>=3.4" numpy
  # reticulate honours RETICULATE_PYTHON; make it visible to every R call below.
  export RETICULATE_PYTHON="$venv/bin/python"
  echo "RETICULATE_PYTHON=$RETICULATE_PYTHON"
  "$venv/bin/python" -c "import optuna; print('optuna', optuna.__version__)"
}

if [ "$RUN_SETUP" = "1" ]; then
  run_step "system-deps"   install_system_deps
  run_step "r-packages"    install_r_packages
  run_step "python-optuna" install_python_optuna
else
  # Still need reticulate pointed at optuna if the venv exists.
  [ -x "$WORKDIR/optuna-venv/bin/python" ] && export RETICULATE_PYTHON="$WORKDIR/optuna-venv/bin/python"
fi

# =============================================================================
# 2. Get the code
# =============================================================================
REPO_DIR="$WORKDIR/BayesianOptim_BASS"
get_repo() {
  rm -rf "$REPO_DIR"
  if [ -n "$LOCAL_REPO" ]; then
    echo "Copying from LOCAL_REPO=$LOCAL_REPO"
    cp -a "$LOCAL_REPO" "$REPO_DIR"
    git -C "$REPO_DIR" checkout "$BRANCH" || true
  else
    local url="$REPO_URL"
    if [ -n "$GITHUB_TOKEN" ]; then
      url="https://x-access-token:${GITHUB_TOKEN}@github.com/AdrianTJ/BayesianOptim_BASS.git"
    fi
    git clone --branch "$BRANCH" --single-branch "$url" "$REPO_DIR"
  fi
  echo "HEAD: $(git -C "$REPO_DIR" rev-parse HEAD)  ($(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD))"
}
run_step "get-repo" get_repo

CODE_DIR="$REPO_DIR/code_files"
RESULTS_DIR="$CODE_DIR/results"          # runners nest per-objective subfolders here
mkdir -p "$RESULTS_DIR"

# =============================================================================
# 3. Write the BASS diagnostics driver into the repo (self-contained)
# =============================================================================
DRIVER="$CODE_DIR/run_bass_diagnostics.R"
cat > "$DRIVER" <<'R_EOF'
#!/usr/bin/env Rscript
# Embedded by run_on_ec2.sh. Investigates WHY BASS-BO trails on categorical
# problems, holding the (now schema-aware) candidate generator fixed.
#
# Matrix: BASS acquisition {EI, Thompson} x posterior resolution {fast, rich}
# x evaluation budget (one long run, sliced at 80 and DIAG_BUDGET) x objective
# and categorical dimension. GP-BO and Random are run once per objective as
# references. Each (objective, seed) shares one initial design across methods.
suppressPackageStartupMessages({
  library(tidyverse); library(lhs); library(BASS); library(GPfit)
  library(future); library(furrr)
})

this_file  <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(this_file)) dirname(normalizePath(this_file)) else getwd()
lib_dir    <- file.path(script_dir, "R")
source(file.path(lib_dir, "bootstrap.R")); source_library(lib_dir)

cfg0 <- parse_cli_args(commandArgs(trailingOnly = TRUE))   # --reps --budget --out_dir --n_cand ...
DIAG_REPS   <- cfg0$reps
DIAG_BUDGET <- cfg0$budget
OUT         <- cfg0$out_dir
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

# Posterior-resolution presets. "fast" matches the library default; "rich" is
# tunable from the environment. Set as globals so furrr exports them per call.
mcmc_presets <- list(
  fast = c(nmcmc = 4000L, nburn = 2000L, thin = 10L),
  rich = c(nmcmc = as.integer(Sys.getenv("DIAG_RICH_NMCMC", "12000")),
           nburn = as.integer(Sys.getenv("DIAG_RICH_NBURN",  "6000")),
           thin  = as.integer(Sys.getenv("DIAG_RICH_THIN",     "10")))
)
set_bass_mcmc <- function(p) {
  assign("BASS_NMCMC", as.integer(p["nmcmc"]), envir = .GlobalEnv)
  assign("BASS_NBURN", as.integer(p["nburn"]), envir = .GlobalEnv)
  assign("BASS_THIN",  as.integer(p["thin"]),  envir = .GlobalEnv)
  assign("BASS_KEEP",
         (as.integer(p["nmcmc"]) - as.integer(p["nburn"])) %/% as.integer(p["thin"]),
         envir = .GlobalEnv)
}

# Run a set of methods for one seed on one objective, from a shared design.
run_seed_methods <- function(seed, objname, d, budget, method_specs, ncand, eps, duptol) {
  objective <- load_objective(objname, d)
  schema    <- objective$schema
  set.seed(seed)
  dd     <- objective$d
  n0     <- max(2 * dd + 1, 8)
  X_init <- lhs::maximinLHS(n0, dd)
  y_init <- objective$fn(X_init)
  cand   <- function(X, y) hybrid_candidates(X, y, ncand, schema)

  rows <- list()
  for (ms in method_specs) {
    acq <- switch(ms$kind,
      random = NULL,
      gp     = make_gp_acquire(list(eps = eps)),
      bass   = make_bass_acquire(list(acquisition = ms$acq), schema)
    )
    m   <- list(name = ms$label,
                candidates = if (is.null(acq)) NULL else cand,
                acquire = acq)
    res <- run_bo(objective, m, list(budget = budget, dup_tol = duptol), X_init, y_init)
    nm  <- if (identical(ms$kind, "bass")) BASS_NMCMC else NA_integer_   # verifies the override propagated
    rows[[ms$label]] <- tibble::tibble(
      objective = sprintf("%s_d%d", objname, dd),
      seed = seed, iter = 0:budget, method = ms$label, best = res$best, nmcmc = nm
    )
  }
  dplyr::bind_rows(rows)
}

# Objective grid. full = TRUE -> include Thompson variants (the harder cells).
specs <- list(
  list(name = "cat_ackley", d = 6L, full = TRUE),
  list(name = "func3C",     d = 5L, full = TRUE),
  list(name = "cat_ackley", d = 4L, full = FALSE),
  list(name = "cat_ackley", d = 8L, full = FALSE),
  list(name = "func2C",     d = 4L, full = FALSE)
)
if (Sys.getenv("DIAG_INCLUDE_FULL",  "1") != "1") specs <- Filter(function(s) !s$full, specs)
if (Sys.getenv("DIAG_INCLUDE_LIGHT", "1") != "1") specs <- Filter(function(s)  s$full, specs)

seeds <- cfg0$seed_start + 0:(DIAG_REPS - 1)
fopts <- furrr::furrr_options(seed = TRUE, packages = c("BASS", "GPfit", "lhs"))

run_objective <- function(spec) {
  objname <- spec$name; d <- spec$d; full <- spec$full
  cat(sprintf("\n#### diagnostics: %s (d=%d, full=%s)\n", objname, d, full))

  # FAST pass: references + fast BASS variants.
  set_bass_mcmc(mcmc_presets$fast)
  ms_fast <- list(
    list(kind = "random", label = "Random"),
    list(kind = "gp",     label = "GP-BO"),
    list(kind = "bass", acq = "ei",       label = "BASS-EI (fast)")
  )
  if (full) ms_fast <- c(ms_fast, list(list(kind = "bass", acq = "thompson", label = "BASS-TS (fast)")))
  rows_fast <- furrr::future_map_dfr(
    seeds, ~ run_seed_methods(.x, objname, d, DIAG_BUDGET, ms_fast,
                              cfg0$n_cand, cfg0$eps, cfg0$dup_tol),
    .options = fopts)

  # RICH pass: rich BASS variants only (references already captured).
  set_bass_mcmc(mcmc_presets$rich)
  ms_rich <- list(list(kind = "bass", acq = "ei", label = "BASS-EI (rich)"))
  if (full) ms_rich <- c(ms_rich, list(list(kind = "bass", acq = "thompson", label = "BASS-TS (rich)")))
  rows_rich <- furrr::future_map_dfr(
    seeds, ~ run_seed_methods(.x, objname, d, DIAG_BUDGET, ms_rich,
                              cfg0$n_cand, cfg0$eps, cfg0$dup_tol),
    .options = fopts)

  dplyr::bind_rows(rows_fast, rows_rich)
}

all_runs <- dplyr::bind_rows(lapply(specs, function(s)
  tryCatch(run_objective(s),
           error = function(e) { cat("ERROR on", s$name, ":", conditionMessage(e), "\n"); NULL })))

readr::write_csv(all_runs, file.path(OUT, "diag_all_runs.csv"))

# Final-performance leaderboard at budget 80 and at the full diagnostic budget.
points <- sort(unique(c(min(80L, DIAG_BUDGET), DIAG_BUDGET)))
final <- all_runs |>
  dplyr::filter(iter %in% points) |>
  dplyr::group_by(objective, budget = iter, method) |>
  dplyr::summarise(mean_final = mean(best), sd_final = sd(best), n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(objective, budget, mean_final)
readr::write_csv(final, file.path(OUT, "diag_final_summary.csv"))

# Convergence curves (mean +/- 95% CI) for later plotting.
curve <- all_runs |>
  dplyr::group_by(objective, method, iter) |>
  dplyr::summarise(mean_best = mean(best), sd_best = sd(best), n = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(se = sd_best / sqrt(n), ci_low = mean_best - 1.96 * se, ci_high = mean_best + 1.96 * se)
readr::write_csv(curve, file.path(OUT, "diag_summary_curve.csv"))

# One convergence plot per objective.
for (o in unique(curve$objective)) {
  p <- ggplot2::ggplot(dplyr::filter(curve, objective == o),
                       ggplot2::aes(iter, mean_best, color = method, fill = method)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, linewidth = 0) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = sprintf("BASS diagnostics: %s", o),
                  x = "Iteration", y = "Best so far (lower is better)") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(OUT, sprintf("diag_convergence_%s.png", o)),
                  p, width = 9, height = 5, dpi = 150)
}

cat("\n==== Diagnostics final summary ====\n")
print(final, n = 500)
cat(sprintf("\nDiagnostics written to: %s\n", normalizePath(OUT)))
plan(sequential)
R_EOF
echo "Wrote diagnostics driver: $DRIVER"

# =============================================================================
# 4. Capture run metadata (hardware, versions, git SHA)
# =============================================================================
META="$RESULTS_DIR/run_metadata.txt"
capture_metadata() {
  {
    echo "timestamp:    $(ts)"
    echo "host:         $(hostname)"
    echo "uname:        $(uname -a)"
    echo "nproc:        $(nproc 2>/dev/null)"
    echo "git_head:     $(git -C "$REPO_DIR" rev-parse HEAD 2>/dev/null)"
    echo "git_branch:   $(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null)"
    echo "RETICULATE_PYTHON: ${RETICULATE_PYTHON:-<unset>}"
    echo "protocol:     REPS=$REPS BUDGET=$BUDGET | DIAG_REPS=$DIAG_REPS DIAG_BUDGET=$DIAG_BUDGET"
    echo "rich_mcmc:    nmcmc=$DIAG_RICH_NMCMC nburn=$DIAG_RICH_NBURN thin=$DIAG_RICH_THIN"
    echo ""
    echo "--- CPU ---";    (lscpu 2>/dev/null | grep -E 'Model name|^CPU\(s\)|Thread|Core') || true
    echo "--- Memory ---"; (free -h 2>/dev/null || grep MemTotal /proc/meminfo) || true
    echo ""
    echo "--- R + packages ---"
    Rscript -e 'cat("R", as.character(getRversion()), "\n"); for (p in c("tidyverse","lhs","BASS","GPfit","future","furrr","glmnet","reticulate")) cat(sprintf("  %-12s %s\n", p, tryCatch(as.character(packageVersion(p)), error=function(e) "MISSING")))' 2>/dev/null || true
    echo "--- optuna ---"
    [ -n "${RETICULATE_PYTHON:-}" ] && "$RETICULATE_PYTHON" -c "import optuna; print('  optuna', optuna.__version__)" 2>/dev/null || echo "  (optuna not resolved)"
  } > "$META"
  cat "$META"
}
run_step "metadata" capture_metadata

# =============================================================================
# 5. Full thesis benchmark suite (each objective isolated, failures non-fatal)
# =============================================================================
run_suite() {
  cd "$CODE_DIR" || return 1
  # Continuous benchmarks.
  run_step "bench-branin"    Rscript run_benchmark.R --objective=branin    --d=2 --budget="$BUDGET" --reps="$REPS"
  run_step "bench-rastrigin" Rscript run_benchmark.R --objective=rastrigin  --d=4 --budget="$BUDGET" --reps="$REPS"
  run_step "bench-synthetic" Rscript run_benchmark.R --objective=synthetic  --d=3 --budget="$BUDGET" --reps="$REPS"
  # Categorical / mixed benchmarks (TPE is the strongest comparison here).
  run_step "bench-func2C"     Rscript run_benchmark.R --objective=func2C            --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"
  run_step "bench-func3C"     Rscript run_benchmark.R --objective=func3C            --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"
  # Cat-Ackley at three sizes: easy (5^3) / medium (7^4) / hard (11^6).
  run_step "bench-cat_ackley-easy" Rscript run_benchmark.R --objective=cat_ackley --d=3 --cat_L=5  --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"
  run_step "bench-cat_ackley-med"  Rscript run_benchmark.R --objective=cat_ackley --d=4 --cat_L=7  --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"
  run_step "bench-cat_ackley-hard" Rscript run_benchmark.R --objective=cat_ackley --d=6 --cat_L=11 --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"
  # TPE gamma-sensitivity ablation (Branin + Cat-Ackley).
  run_step "tpe-sensitivity"  Rscript 2_tpe_sensitivity/run_tpe_sensitivity.R --budget="$BUDGET" --reps="$REPS"
  # Elastic Net case study.
  run_step "elastic-net"      Rscript 4_regression_test_case/run_elastic_net.R --reps="$ENET_REPS" --budget="$ENET_BUDGET"
}
[ "$RUN_SUITE" = "1" ] && run_suite

# =============================================================================
# 6. BASS-trailing investigation
# =============================================================================
run_diag() {
  cd "$CODE_DIR" || return 1
  Rscript run_bass_diagnostics.R \
    --reps="$DIAG_REPS" --budget="$DIAG_BUDGET" --out_dir="results/diagnostics"
}
[ "$RUN_DIAG" = "1" ] && run_step "bass-diagnostics" run_diag

# =============================================================================
# 7. Package everything for download
# =============================================================================
package_results() {
  cp -f "$LOG" "$RESULTS_DIR/run.log" 2>/dev/null || true
  cp -f "$STEPLOG" "$RESULTS_DIR/steps.tsv" 2>/dev/null || true
  local stamp; stamp="$(date +%Y%m%d_%H%M%S)"
  local tarball="$WORKDIR/bass_results_${stamp}.tar.gz"
  tar -czf "$tarball" -C "$CODE_DIR" results
  echo ""
  echo "######################################################################"
  echo "ALL DONE."
  echo "Results tree : $RESULTS_DIR"
  echo "Tarball      : $tarball  ($(du -h "$tarball" | cut -f1))"
  echo ""
  echo "Step outcomes:"; column -t -s $'\t' "$STEPLOG" 2>/dev/null || cat "$STEPLOG"
  echo ""
  echo "Download it from your laptop with, e.g.:"
  echo "  scp ubuntu@<this-ec2-host>:$tarball ."
  echo "or push to S3 (if the instance has a role/creds):"
  echo "  aws s3 cp $tarball s3://<your-bucket>/"
  echo "######################################################################"
}
run_step "package" package_results

log "Finished."
