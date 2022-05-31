# life_is_long_no_python_please-lab2-variant3

## Description

### Objectives

Use development tools: Haskell, IDE/source code editor, git
, Github Actions, and laboratory work process.

Design algorithms and data structures in mutable styles.

Develop unit and property-based tests Students should meet with tools and
typical development workflow on the classical development task in the first
laboratory work: developing a library for a specific data structure.

Students should implement the selected by the variants data structure as a
mutable object (interaction with an object should modify it if applicable). All
 changes should be performed in place, by changing the initial structure.

## Variant

- No.3 Set based on binary-tree

- Simulate `mutable` by State Monad or Free Monad

## Struct of project

- The main.hs in app/ is just a file init by cabal. It doesn't matter.

- The implementation of Binary Search Tree is in src/BST.hs.

- The test runs by the cabal, and the test cases are in test/*.hs.

## Key features

- The use of Haskell.

- Achieve a generator of random tree struct.

- Use the `State Monad`

## Design note

Fix a bug in `remove`.

## Answer for questions

- Compare mutable and immutable implementation

    For what I do with Haskell, the simulate of `mutable` makes it possible
    to write code in sequence, just like in other languages.
    More normal, mutable implementation encapsulates functions in a
    real object, which called methods. It shows a natural way to call them.
    At the same time, it usually becomes redundant when we only use part of
    the methods.
    And the immutable implementation requires developers to have better
    grasp of the library. And it is good for parallel computing because
    there is no limitation of mutexes.

- Note implementation restriction

    So abstract the `State` is that it's hard to understanduse it and use it.

## Contribution

Zhao, Tianhao is responsible for the implementation of the
data structure and some test cases.

Ge, Binghang edit the Readme and contribute some test cases,
also the action.