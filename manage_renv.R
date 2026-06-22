# R script for:
# Renv environment manager
# Script for automated package management and project reproducibility
# 
# For paper:
#
# 
# Authors of the paper:
# Puguh Prasetyoputra
# Yovita Isnasari
# Ari Purwanto Sarwo Prasojo
# Iwan Hermawan
# 
# Code by:
# Ari Purwanto Sarwo Prasojo
# 
# Date of this version:
# 2026/05/31

# ==============================================================================
# ENVIRONMENT MANAGER
# ==============================================================================

# 1. Ensure the 'renv' package is installed on the system
if (!requireNamespace("renv", quietly = TRUE)) {
  message("[-] 'renv' package not found. Installing it now...")
  install.packages("renv")
}

# 2. Automated Repository Setup
if (!file.exists("renv.lock")) {
  # Condition A: New repository or missing renv.lock
  message("[!] renv.lock file not found. Initializing a new renv environment for the project...")
  renv::init(bare = FALSE) 
  # bare = FALSE automatically scans your scripts to register discovered packages to the lockfile
  
} else {
  # Condition B: Cloning a repository that already contains renv.lock
  message("[+] renv.lock file found. Synchronizing local library with the lockfile...")
  renv::activate()   # Ensures renv is activated for this project environment
  renv::restore(prompt = FALSE) # Restores/installs the exact package versions without manual prompts
  message("[+] Synchronization complete! Your local environment is ready for use.")
}

# ==============================================================================
# PERIODIC MANAGEMENT ----
# (Run the lines below manually as needed)
# ==============================================================================

# WHEN SHOULD YOU RUN THE LINE BELOW?
# -> Run this after adding, removing, or updating packages in your project scripts
#    to record the changes in the renv.lock file before making a git commit.

# renv::snapshot()


# WHEN SHOULD YOU RUN THE LINE BELOW?
# -> Run this to inspect discrepancies between packages declared in scripts, 
#    the lockfile, and your current local project library.

# renv::status()