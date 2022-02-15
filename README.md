# circuit-streams

This repo contains code for generating Mealy machines from certain classes of *stream functions*.

At a high level, the class of stream functions we are interested in have three properties:

* They are **causal**: the *n*th element of the output depends only on the first *n+1* inputs;
* They are **monotone**: the initial output is a monotone function, and the stream derivative produces another monotone stream function; and
* They have **finite stream derivatives**: from a given stream function, there always exists at least one finite word *w* such that the stream derivatives obtained by inputting *w* contains at least one duplicate.

## Example

To run the example, execute the following command:

```sh
dune exec bin/streams/main.exe
```

This will create an output file `mealy.dot`.
You can then generate the corresponding svg with

```sh
dot -Tsvg mealy.dot -O
```

## Background

This program is based on work on my paper with Dan Ghica and David Sprunger, '[Full abstraction for digital circuits](https://arxiv.org/abs/2201.10456)', in particular section 5.
