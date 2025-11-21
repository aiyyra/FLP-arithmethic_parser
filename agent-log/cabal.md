# Cabal Build System Guide

This document provides a comprehensive guide to using Cabal for building the Arithmetic Expression Evaluator project.

## What is Cabal?

**Cabal** (Common Architecture for Building Applications and Libraries) is the standard build system and package manager for Haskell projects. It handles:

- **Dependency management**: Automatically downloads and manages Haskell libraries
- **Building**: Compiles your Haskell code with proper flags
- **Testing**: Runs test suites
- **Packaging**: Creates distributable packages
- **Installation**: Installs executables to your system

## Prerequisites

### Check if Cabal is Installed

```bash
cabal --version
```

**Expected output:**

```
cabal-install version 3.12.1.0
compiled using version 3.12.1.0 of the Cabal library
```

### Check GHC Version

```bash
ghc --version
```

**Expected output:**

```
The Glorious Glasgow Haskell Compilation System, version 9.6.7
```

### Installation (if needed)

If Cabal is not installed, install it via GHCup:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## Project Structure

Our Cabal project is located in the `solution/` directory:

```
solution/
├── evaluator.cabal    # Cabal configuration file
├── Main.hs            # Entry point
├── Tokenizer.hs       # Module
├── Parser.hs          # Module
├── Evaluator.hs       # Module
└── Tests.hs           # Test suite
```

## Understanding the .cabal File

The `evaluator.cabal` file describes our project:

```cabal
cabal-version:      3.0                    # Cabal format version
name:               evaluator              # Project name
version:            1.0.0                  # Project version
synopsis:           Arithmetic Expression Evaluator CLI
description:        Full description of the project
license:            NONE                   # No license (coursework)
author:             WID3001 Student
maintainer:         student@example.com
category:           Education
build-type:         Simple                 # Standard build
extra-source-files: README.md              # Extra files to include

common warnings
    ghc-options: -Wall                     # Enable all warnings

executable evaluator                       # Defines an executable
    import:           warnings              # Import common settings
    main-is:          Main.hs              # Entry point file
    other-modules:    Tokenizer            # Other modules to compile
                    , Parser
                    , Evaluator
    build-depends:    base ^>=4.18.2.1     # Dependencies (only base)
    hs-source-dirs:   .                    # Source directory
    default-language: Haskell2010          # Haskell standard
```

### Key Sections Explained

**1. Metadata**

- `name`: The package name (used for the executable)
- `version`: Semantic versioning (major.minor.patch)
- `synopsis`: One-line description
- `description`: Detailed description

**2. Common Settings**

- `common warnings`: Reusable configuration block
- `ghc-options: -Wall`: Enable all compiler warnings

**3. Executable**

- `executable evaluator`: Defines a binary named "evaluator"
- `main-is`: The file containing the `main` function
- `other-modules`: Additional modules to compile
- `build-depends`: External libraries (we only use `base`)
- `hs-source-dirs`: Where to find source files

## Basic Cabal Commands

### 1. Update Package Index

Before first use, update the package list:

```bash
cd solution
cabal update
```

This downloads the latest package index from Hackage (Haskell package repository).

### 2. Build the Project

Compile the project:

```bash
cabal build
```

**What happens:**

- Cabal resolves dependencies
- Downloads any missing packages
- Compiles all modules
- Links the executable
- Stores build artifacts in `dist-newstyle/`

**Output example:**

```
Resolving dependencies...
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built:
 - evaluator-1.0.0 (exe:evaluator) (first run)
Configuring executable 'evaluator' for evaluator-1.0.0..
Preprocessing executable 'evaluator' for evaluator-1.0.0..
Building executable 'evaluator' for evaluator-1.0.0..
[1 of 4] Compiling Tokenizer
[2 of 4] Compiling Parser
[3 of 4] Compiling Evaluator
[4 of 4] Compiling Main
Linking dist-newstyle/build/.../evaluator-1.0.0/x/evaluator/build/evaluator/evaluator ...
```

### 3. Run the Executable

Run without installing:

```bash
cabal run evaluator -- "2+3"
```

**Breakdown:**

- `cabal run`: Run the executable
- `evaluator`: Name of the executable (from .cabal file)
- `--`: Separator between Cabal args and program args
- `"2+3"`: Argument passed to the evaluator program

**Examples:**

```bash
cabal run evaluator -- "2^3"
cabal run evaluator -- "sin(0)+abs(-5)"
cabal run evaluator -- "sqrt(16)/2"
```

### 4. Clean Build Artifacts

Remove compiled files:

```bash
cabal clean
```

This deletes the `dist-newstyle/` directory.

### 5. Install the Executable

Install to your local bin directory:

```bash
cabal install
```

**What happens:**

- Builds the project
- Copies the executable to `~/.cabal/bin/` (or `~/.local/bin/`)
- Makes it available system-wide (if bin is in PATH)

**After installation, you can run:**

```bash
evaluator "2+3"
```

(without `cabal run`)

## Advanced Cabal Commands

### Build with Optimizations

Build with optimization level 2:

```bash
cabal build --ghc-options="-O2"
```

### Build in Verbose Mode

See detailed compilation output:

```bash
cabal build -v
```

### Rebuild from Scratch

Force a complete rebuild:

```bash
cabal clean
cabal build
```

### Check for Issues

Run without building:

```bash
cabal check
```

This validates your `.cabal` file for common issues.

## Cabal Workflow

### Typical Development Workflow

1. **Initial Setup** (once):

   ```bash
   cd solution
   cabal update
   ```

2. **Development Cycle** (repeat):

   ```bash
   # Edit code
   vim Main.hs

   # Build and run
   cabal run evaluator -- "test expression"

   # Or just build
   cabal build
   ```

3. **Testing**:

   ```bash
   # Run specific tests
   cabal run evaluator -- "2+3"
   cabal run evaluator -- "sqrt(-1)"  # Test error handling
   ```

4. **Final Build**:
   ```bash
   cabal clean
   cabal build
   cabal install  # Optional: install system-wide
   ```

## Understanding Build Output

### Build Directory Structure

After running `cabal build`, you'll see:

```
solution/
├── dist-newstyle/
│   ├── build/
│   │   └── x86_64-linux/
│   │       └── ghc-9.6.7/
│   │           └── evaluator-1.0.0/
│   │               └── x/
│   │                   └── evaluator/
│   │                       ├── build/
│   │                       │   └── evaluator/
│   │                       │       └── evaluator  # The executable!
│   │                       └── ...
│   └── cache/
└── ...
```

### Finding the Executable

The built executable is deeply nested. Use `cabal run` instead of navigating to it manually.

Or find it with:

```bash
find dist-newstyle -name evaluator -type f
```

## Comparison: Manual GHC vs Cabal

### Manual GHC Compilation

```bash
ghc --make -o evaluator Main.hs
./evaluator "2+3"
```

**Pros:**

- Simple for small projects
- Direct control
- Fast for single files

**Cons:**

- No dependency management
- Manual flag management
- No standardized structure
- Hard to share/distribute

### Cabal Build

```bash
cabal build
cabal run evaluator -- "2+3"
```

**Pros:**

- Automatic dependency management
- Standardized project structure
- Easy to share (just share .cabal file)
- Reproducible builds
- Professional standard

**Cons:**

- More complex for beginners
- Nested build directories
- Requires understanding of .cabal format

## Common Issues and Solutions

### Issue 1: "Could not resolve dependencies"

**Problem:** Cabal can't find compatible package versions

**Solution:**

```bash
cabal update
cabal build
```

### Issue 2: "No executables found"

**Problem:** Trying to run before building

**Solution:**

```bash
cabal build
cabal run evaluator -- "expression"
```

### Issue 3: "Module not found"

**Problem:** Module not listed in `other-modules`

**Solution:** Edit `evaluator.cabal` and add the module to `other-modules`

### Issue 4: Build cache issues

**Problem:** Strange compilation errors after code changes

**Solution:**

```bash
cabal clean
cabal build
```

## Best Practices

### 1. Always Use Cabal for Projects

Even small projects benefit from Cabal's structure.

### 2. Keep .cabal File Updated

When adding new modules, update `other-modules` section.

### 3. Use `cabal run` During Development

Faster than building and running separately.

### 4. Clean Before Final Build

```bash
cabal clean && cabal build
```

Ensures a fresh, reproducible build.

### 5. Version Your Dependencies

Use specific version ranges in `build-depends`:

```cabal
build-depends: base ^>=4.18.2.1
```

## Quick Reference

| Task                 | Command                         |
| -------------------- | ------------------------------- |
| Update package index | `cabal update`                  |
| Build project        | `cabal build`                   |
| Run executable       | `cabal run evaluator -- "expr"` |
| Clean build files    | `cabal clean`                   |
| Install executable   | `cabal install`                 |
| Check .cabal file    | `cabal check`                   |
| Verbose build        | `cabal build -v`                |
| Rebuild from scratch | `cabal clean && cabal build`    |

## Summary

Cabal provides a professional, standardized way to build Haskell projects. For our evaluator:

1. **Created** `evaluator.cabal` configuration file
2. **Use** `cabal build` to compile
3. **Use** `cabal run evaluator -- "expression"` to test
4. **Benefit** from automatic dependency management and reproducible builds

This makes the project easier to share, build, and maintain!

```

```
