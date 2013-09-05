pipes-platform
===============

[![Build Status](https://travis-ci.org/Gabriel439/pipes-platform.png)](https://travis-ci.org/Gabriel439/pipes-platform)

The haskell pipes platform

## Quick Start

To quickly get the latest non-haddock suite of pipes-4.0 you have two options:

### via cabal-meta

A sources.txt is maintained so all packages can be installed with:

    cabal-meta install -j

Or if you are using `cabal-dev`:

    cabal-meta --dev install -j

If you are using an old version of `Cabal`, then the `-j` switch (which enables
installing packages in parallel) might not be available. Just remove it.

### via submodules

1. clone and update

    ```
    git clone https://github.com/Gabriel439/pipes-platform.git
    cd pipes-platform
    git submodule update --init --remote (--rebase)
    ```

   The submodule command is somewhat dependent on your git version, or otherwise reliant on the git gods so your mileage may vary.  If the above doesn't work try just:

   ```
   git submodule update --init
   ```
   
   as --remote is a more modern git convention.

   This grabs each of the libraries as submodules, and inserts them into the deps directory.  

2. install

You then just have to `cabal install` within each of the deps directories (using the order in .travis.yml).


## Quick Develop

### adding a library

To add a library, 

1. Starting with a fresh fork of this repo:

     ```
     git checkout -b new-lib
     git submodule add -b master https://github.com/bgamari/pipes-vector.git deps/pipes-vector
     git submodule update --remote --rebase
     ```

2. Add the repository to `cabal-meta`'s `sources.txt` file:

   ```
   echo "https://github.com/bgamari/pipes-vector.git master" >> sources.txt
   ```

4. Add the package to `pipes-platform.cabal`, and to `.travis.yml`

5. push the new commit and send a pull request

     ```
     git push origin new-lib
     ```

### changing branches (or entire libraries)

The approximate work-flow goes something like this:

1. Start with a fresh fork and new branch
  
2. swap out the submodule reference by editing .submodule like so:
     ```
        [submodule "deps/pipes-vector"]
      	path = deps/pipes-vector
     -	url = https://github.com/bgamari/pipes-vector.git
     -	branch = master
     +	url = https://github.com/tonyday567/pipes-vector.git
     +	branch = dep-fix  
     ```
3. sync this change

     ```
     git checkout -b lib-change
     git submodule sync -- deps/pipes-vector
     git submodule update --remote --rebase
     git push origin new-lib
     ```

4. send a pull request

## hot fix

To quickly test and fix a travis-ci fail

a. log issue with upstream (and wait), or 
b. attempt a `hot-fix` branch:

  1. fork the offending upstream library
  
     ```
     git clone https://github.com/bgamari/pipes-vector.git master
     cd pipes-vector
     git checkout -b hot-fix
     ```
     
  2. debug and hack the fix
  
     ```
     git push origin hot-fix
     ```
  3. to test start a new branch of pipes-platform and swap libraries in the same way as described in changng branches.
  
  4. (optionally), push the hot-fix branch so travis can test it.
  
  5. If it looks good, pull request the fix in step 2. to the upstream library
  
  
