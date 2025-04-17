#!/bin/bash

## Setup minimal environment for bspm
apt update -qq && apt install --yes --no-install-recommends \\
  python3-dbus python3-gi python3-apt \\
  make \\
  wget \\
  ca-certificates \\
  gnupg

## Add keys for r2u
wget -qO- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | \\
    tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc

## Add r2u repository
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu jammy main" \\
    > /etc/apt/sources.list.d/cranapt.list

## Update apt
apt update -qq

## Install bspm through apt to avoid conflicts
apt install --yes --no-install-recommends r-cran-bspm

## Enable bspm globally in R
RHOME=$(R RHOME)
echo "suppressMessages(bspm::enable())" >> ${RHOME}/etc/Rprofile.site
echo "options(bspm.version.check=FALSE)" >> ${RHOME}/etc/Rprofile.site