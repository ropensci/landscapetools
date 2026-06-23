_default:
    just --list

# Format R code
fmt:
    air format .

# Run jarl linter on the R package
lint:
    jarl check . --fix --allow-dirty

# Update R documentation
document:
    Rscript -e "devtools::document()"

# Build and test the R package.
test:
    #!/usr/bin/env bash
    R CMD build .
    R CMD check --as-cran landscapetools_*.tar.gz
    rm -rf landscapetools_*.tar.gz landscapetools.Rcheck