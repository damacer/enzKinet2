#!/usr/bin/env bash
# Builds the documentation site and publishes it to https://eka.nz/docs by hand.
# Normally GitHub Actions does this on every push to master (.github/workflows/pkgdown.yaml);
# use this when Actions can't, or to check a change on the live site before pushing.
# It publishes the last commit: commit your changes first.
# Needs R with pkgdown, and AWS access: run `aws login --profile eka` first.
# See enzKinet_webpage/docs/public-docs.md.
set -euo pipefail
cd "$(dirname "$0")/.."

# R must run in a UTF-8 locale, or characters such as "µ" are garbled in plots.
export LANG=en_NZ.UTF-8 LC_ALL=en_NZ.UTF-8
export AWS_PROFILE="${AWS_PROFILE:-eka}" AWS_PAGER=""
BUCKET=eka-nz-site
DISTRIBUTION=E2Y2J5TXS8QQHB

aws sts get-caller-identity --query Arn --output text

# Build from a clean copy of the last commit, as GitHub Actions does. pkgdown renders every
# Markdown file in the package folder and keeps old files in docs/, so building in the working
# folder could publish local notes or leftovers (including in the sitemap and search index).
if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "Note: uncommitted changes won't be published."
fi
BUILD="$(mktemp -d)"
trap 'rm -rf "$BUILD"' EXIT
git archive HEAD | tar -x -C "$BUILD"
echo "Building $(git rev-parse --short HEAD) in $BUILD"
(cd "$BUILD" && Rscript -e 'pkgdown::build_site(preview = FALSE, new_process = FALSE, install = TRUE)')

aws s3 sync "$BUILD/docs/" "s3://$BUCKET/docs/" --delete --cache-control "max-age=300" --only-show-errors
aws s3 cp "$BUILD/tools/site-root/robots.txt" "s3://$BUCKET/robots.txt" --content-type "text/plain" --cache-control "max-age=3600" --only-show-errors
aws s3 cp "$BUILD/pkgdown/favicon/favicon.ico" "s3://$BUCKET/favicon.ico" --content-type "image/x-icon" --cache-control "max-age=86400" --only-show-errors
aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION" --paths "/docs/*" "/robots.txt" "/favicon.ico" --query Invalidation.Id --output text
echo "Published. Changes show at https://eka.nz/docs within a few minutes."
