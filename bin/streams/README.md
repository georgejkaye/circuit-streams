# `streams`

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
