#!/usr/bin/env bash
# Build and install conttables2 into jamovi desktop and/or a running jamovi
# Docker container.
#
#   bash install.sh              both targets, whichever are available
#   bash install.sh desktop
#   bash install.sh docker [container]     (default container: jamovi)
#
set -euo pipefail

TARGET="${1:-both}"
CONTAINER="${2:-jamovi}"

HERE="$(cd "$(dirname "$0")/../conttables2" && pwd)"
MODULE=conttables2
VERSION="$(awk -F': *' '$1 == "Version" { print $2; exit }' "$HERE/DESCRIPTION")"
ARTIFACT="$HERE/${MODULE}_${VERSION}.jmo"

# ── desktop ──────────────────────────────────────────────────────────────────
install_desktop() {
  local ARCH PD APP APP_R LOG
  ARCH="$(uname -m)"
  case "$ARCH" in
    arm64)   PD="$HOME/R/.Rlib-arm" ;;
    x86_64)  PD="$HOME/R/.Rlib-x64" ;;
    *)       echo "unsupported architecture: $ARCH" >&2; return 1 ;;
  esac
  [ -d "$PD" ] || { echo "error: $PD not found" >&2; return 1; }

  APP=/Applications/jamovi.app
  APP_R="$APP/Contents/Frameworks/R.framework/Versions/Current/Resources/bin/R"
  [ -x "$APP_R" ] || { echo "error: no R inside $APP" >&2; return 1; }

  echo ">> desktop: building conttables2 for $ARCH using $PD"
  cd "$HERE"

  LOG="$(mktemp)"
  R_ENVIRON_USER=/dev/null R_PROFILE_USER=/dev/null R_LIBS_USER="$PD" \
    Rscript --vanilla -e 'jmvtools::install()' 2>&1 | tee "$LOG" | grep -vE '^\s*$' || true

  # jmvtools::install() can report errors on stdout while exiting successfully.
  # It can also claim installation succeeded after a SingletonLock failure.
  [ -f "$ARTIFACT" ] || {
    echo "error: jmvtools did not produce $ARTIFACT" >&2
    rm -f "$LOG"; return 1
  }
  if grep -q 'SingletonLock' "$LOG"; then
    echo
    echo "!! jamovi.app could not be driven (SingletonLock denied)."
    echo "!! The .jmo was still built. Install it by hand:"
    echo "!!   jamovi -> Modules -> Install from file -> $ARTIFACT"
    rm -f "$LOG"
    return 0
  fi
  if ! grep -q 'Module installed successfully' "$LOG"; then
    echo "error: jmvtools::install() did not install the module (see above)" >&2
    rm -f "$LOG"; return 1
  fi
  rm -f "$LOG"

  local MODDIR="$HOME/Library/Application Support/jamovi/modules/$MODULE"
  if [ -d "$MODDIR" ]; then
    echo ">> desktop: installed at $MODDIR"
  else
    echo "!! desktop: install reported success but $MODDIR does not exist."
    echo "!! Install $ARTIFACT by hand (Modules -> Install from file)."
    return 1
  fi
}

# ── docker ───────────────────────────────────────────────────────────────────
install_docker() {
  if ! docker ps --format '{{.Names}}' 2>/dev/null | grep -qx "$CONTAINER"; then
    echo "!! docker: container '$CONTAINER' is not running — skipping"
    return 0
  fi

  if ! docker exec "$CONTAINER" sh -c 'command -v jmc >/dev/null 2>&1'; then
    echo "!! docker: jmc is not in the container." >&2
    echo "!! Install the jamovi compiler in the image before using this target." >&2
    return 1
  fi

  echo ">> docker: copying source into $CONTAINER"
  # --no-mac-metadata/--no-xattrs: AppleDouble ._ files otherwise land in the
  # container and jmc tries to compile them.
  tar --no-mac-metadata --no-xattrs -C "$HERE" -cf - DESCRIPTION NAMESPACE R jamovi \
    | docker exec -i "$CONTAINER" sh -c \
        'rm -rf /tmp/conttables2-src && mkdir -p /tmp/conttables2-src && tar -C /tmp/conttables2-src -xf -'

  echo ">> docker: jmc --install"
  docker exec -i "$CONTAINER" bash -s <<'INCONTAINER'
set -euo pipefail
source /usr/lib/jamovi/bin/env.conf 2>/dev/null || true
RHOME="${R_HOME:-$(R RHOME 2>/dev/null || true)}"
[ -n "$RHOME" ] || { echo "   error: no R in the container" >&2; exit 1; }
RLIBS=/usr/lib/jamovi/modules/base/R

jmc --install /tmp/conttables2-src \
    --to /usr/lib/jamovi/modules \
    --rhome "$RHOME" \
    --rlibs "$RLIBS" \
    --patch-version --skip-deps

[ -f /usr/lib/jamovi/modules/conttables2/jamovi.yaml ] || {
  echo "   error: jmc did not install conttables2" >&2; exit 1; }
INCONTAINER

  echo ">> docker: restarting $CONTAINER to load the module"
  docker restart "$CONTAINER" >/dev/null
  echo ">> docker: testing 2xK comparative measures"
  docker exec -i "$CONTAINER" bash -s <<'INCONTAINER'
set -euo pipefail
Rscript --vanilla -e '
    .libPaths(c(
        "/usr/lib/jamovi/modules/base/R",
        "/usr/lib/jamovi/modules/conttables2/R",
        .libPaths()
    ))
    library(conttables2)

    # 2x3 table: 2-level outcome (status) x 3-level exposure (dose)
    data <- data.frame(
        status = factor(rep(c("neg", "pos"), 3), c("neg", "pos")),
        dose   = factor(c("low","low","med","med","high","high"), c("low","med","high")),
        n      = c(50, 10, 40, 20, 30, 25)
    )
    r <- contTables(
        data=data, rows="status", cols="dose", counts="n", compare="columns",
        diffProp=TRUE, logOdds=TRUE, odds=TRUE, relRisk=TRUE)

    odds <- r$odds$asDF
    stopifnot(nrow(odds) == 2)
    stopifnot(identical(odds[["dose"]], c("med", "high")))
    stopifnot(isTRUE(all.equal(odds[["v[o]"]][1], 2.5)))
    cat(sprintf("   2xK comparative-measures smoke test passed (OR[med vs low] = %.3f)\n", odds[["v[o]"]][1]))
'
INCONTAINER
  echo ">> docker: installed conttables2; open Frequencies > Contingency Tables > Independent Samples (2xK) to verify"
}

case "$TARGET" in
  desktop) install_desktop ;;
  docker)  install_docker ;;
  both)    install_desktop || true; echo; install_docker || true ;;
  *)       echo "usage: install.sh [desktop|docker|both] [container]" >&2; exit 1 ;;
esac
