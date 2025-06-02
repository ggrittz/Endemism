#!/bin/bash

# Download files directly to existing directory
wget --directory-prefix="d/Doutorado/SynTreeSys/Endemism/data/raw-data/chelsa" \
     --no-host-directories \
     --cut-dirs=3 \
     --input-file=envidatS3paths.txt

# Verify downloads
ls -l "d/Doutorado/SynTreeSys/Endemism/data/raw-data/chelsa"