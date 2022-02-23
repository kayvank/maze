maze
---
Draw a maze

# What this is
A quick tutorial on [cairo](https://hackage.haskell.org/package/cairo) and [fgl](https://hackage.haskell.org/package/fgl) to generate a maze based on [Tikhon Jelvis blog](https://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs/)

## usage

``` sh
git remote add origin git@github.com:kayvank/maze.git
cd maze
nix-shell
cabal build
cabal run maze-exe ## check output file at: /tmp/maze.png
```

### todo
- unit tests
- more efficient random maze generator

`
