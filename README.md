This package integrates [hspec](http://hspec.github.io) with
[hs-webdriver](https://hackage.haskell.org/package/webdriver), allowing you to write webdriver tests
using hspec.  This package contains no code testing the `hspec-webdriver` package itself.  The
`webdriver-angular` package contains some test code which test both the Angular webdriver commands
in `webdriver-angular` and some tests of functions in `hspec-webdriver`. 

Note about `hspec-webdriver-clone` and `hspec-webdriver`:

The [`hspec-webdriver`](https://hackage.haskell.org/package/hspec-webdriver) v1.2.1 was written and published by the author John Lenz to the bitbucket.org repository, but was not uploaded to Hackage.
The bitbucket.org mercurial repo stopped working in 2020, and the original source repository became inaccesible.
Moreover attempts to contact the author to update the Hackage did not succeed, therefore this package was created to push the v1.2.1 and provide maintainance for future hspec releases.
The code in this package was obtained from local copy of the source-code stored in the `/nix/store`.
