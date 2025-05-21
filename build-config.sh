#!/bin/bash

# Download and install Quarto
QUARTO_VERSION=1.4.550
echo "Downloading Quarto v${QUARTO_VERSION}..."
wget -q https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz
tar -xzf quarto-${QUARTO_VERSION}-linux-amd64.tar.gz
export PATH=$PATH:$PWD/quarto-${QUARTO_VERSION}/bin

# Verify Quarto installation
quarto --version

# Build the site
echo "Building Quarto site..."
quarto render

# Move output to the Cloudflare Pages expected directory if needed
# If your Quarto output is already in the _site directory, this may not be needed
if [ -d "_output" ] && [ ! -d "_site" ]; then
  mv _output _site
fi
