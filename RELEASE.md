# Release process

This package uses
[`semantic-release`](https://github.com/semantic-release/semantic-release) to
manage versions. In order to release, you need to follow these steps:

1. Push your commit(s) to the default branch, or merge your pull request
2. In the CI pipeline, there will be a workflow called `Semantic release`: you
   should wait until this workflow has succeeded; it will produce a new tag and
   a release commit with the correct changelog
3. On your local machine, pull the commit and tag and run the command `stack
   upload .`; if you want a preview without publishing a definitive version to
   Hackage, you can use `stack upload --candidate .` to upload a
   [candidate](https://hackage.haskell.org/upload#candidates)
