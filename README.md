# OCaml Playground

This project serves as a playground for exploring the [OCaml](https://ocaml.org/)
programming language.

## Prerequisites

Ensure the following requirements are met prior to usage:

- OCaml [installed](https://ocaml.org/docs/up-and-running)

## First Time Setup

Initialize Opam:

```sh
opam init --enable-shell-hook
```

> This should only be performed once on your machine.

Clone the repository:

```sh
git clone git@github.com:tgillus/ocaml-playground.git
```

Navigate into the project directory:

```sh
cd ocaml-playground
```

Create an OCaml switch with the required dependencies:

```sh
opam switch create . 5.3.0 --deps-only
```

Install development dependencies:

```sh
opam install ocaml-lsp-server ocamlformat odoc utop
```

## Run Code

```sh
dune exec playground
```

## Format Source Code

```sh
dune build @fmt --auto-promote
```

## Exercises

The OCmal [exercises](https://ocaml.org/problems) are used as a means to explore
OCaml syntax, semantics, and idioms.

## Textbook

Cornell University published an online OCaml textbook for their functional
programming course named CS 3110. It has accompanying YouTube lectures that
serve as a tremendous resource for learning OCaml and functional programming.

- [Textbook](https://cs3110.github.io/textbook/cover.html)
- [YouTube Lectures](https://www.youtube.com/playlist?list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU)

