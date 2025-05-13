# $\lambda$-Compiler

A Primitive Compiler from $\lambda$-calculus + $\alpha$ (e.g. primitive operations, recursion, and effects) to C, based on Call-By-Push-Value (CBPV)

## Usage

```
$ lamc [EXAMPLE_ID] [(-c|--direct-c) (-o|--output OUTPUT_FILE) | [--am]]
```

For details of each option, please check `lamc --help`.

### Autocompletion

$\lambda$-compiler supports bash, zsh, and fish completion provided by
[`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative).
To use that, users can check [their documentation about autocompletion](https://github.com/pcapriotti/optparse-applicative?tab=readme-ov-file#bash-zsh-and-fish-completions).

## Current Status
Currently, this repository implements a primitive compiler from Call-By-Value $\lambda$-calculus to C based on Call-By-Push-Value (CBPV) and
a primitive compiler from CBV $\lambda$-calculus to abstract machine (AM) for CBPV. This repository also comes with an interpreter for AM.

- [x] Frontend Parser for $\lambda$-calculus
- [x] Simple bidirectional type-checker for $\lambda$-calculus
- [ ] Constraint-based bidirectional type checker for $\lambda$-calculus
- [x] Call-By-Value $\lambda$-calculus-To-CBPV pass
- [ ] Call-By-Name $\lambda$-calculus-To-CBPV pass
- [ ] Call-By-Need $\lambda$-calculus-To-CBPV pass
- [x] Simple bidirectional type-checker for CBPV
- [ ] Constraint-based bidirectional type checker for CBPV
- [x] Simple CBPV optimization pass
- [ ] Tail call optimization pass (or lowering pass)
- [ ] CBPV interpreter
- [x] CBPV-to-C pass without GC
- [x] CBPV-to-AM pass without GC
- [x] AM interpreter
- [ ] AM-to-C pass without GC
- [ ] GC instruction for AM
- [ ] GC for CBPV-to-C
- [ ] GC for CBPV-to-AM
- [ ] GC for AM-to-C
- [x] PrintInt effect
- [x] PrintDouble effect
- [ ] PrintString effect
- [ ] Read effect
- [ ] Exception-handling effects
- [ ] General I/O effect
