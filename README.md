pipes-ecosystem
===============

testing the pipes suite of libraries

https://secure.travis-ci.org/tonyday567/pipes-ecosystem.png

A trello board is also available that (attempts) to track commits across the ecosystem.

https://trello.com/b/18zh1T6U/pipes

## travis

To add a library to this repo:

0. With a fresh fork ...

1. Add as a submodule tracking the right pipes 4 branch
   - putting in in deps/packagename
   - the best way is usually to track a branch rather than the old git submodule way (it's changed recently)

     ```
     git submodule add -b master https://github.com/bgamari/pipes-vector.git deps/pipes-vector
     ```

2. Update the (whole) repo

   ```
   git submodule update --remote
   ```

   or perhaps, depending on your git version:

   ```
   git submodule update --init
   ```

3. Add the repository to `cabal-meta`'s `sources.txt` file:

   ```
   echo "https://github.com/bgamari/pipes-vector.git master" >> sources.txt
   ```

4. Add the package to `pipes-ecosystem.cabal`, and to `.travis.yml`

   At this point, your shiny new pipes ecosystem can be tested locally.  Just go through all the deps folders (respecting order as in the travis file) and `cabal(-dev) install`

5. push and send a pull request

On committing a pull request, travis will then install all the non-standard non-hackage libraries and either give the green light or barf an email.


Feel free to add anything else in the `test` directory - crumbs, snippets that look at interactions between libraries - whatever you think might be helpful.

## cabal-meta

A sources.txt is maintained so all packages can be installed with:

    cabal-meta install -j

Or if you are using `cabal-dev`:

    cabal-meta --dev install -j

If you are using an old version of `Cabal`, then the `-j` switch (which enables
installing packages in parallel) might not be available. Just remove it.



