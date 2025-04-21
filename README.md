# Lambda Comp

A Primitive Compiler from $\lambda$-calculus

## Current Status
Currently, this repository implements a primitive compiler from Call-By-Value $\lambda$-calculus to C based on Call-By-Push-Value (CBPV) and
a primitive compiler from CBV $\lambda$-calculus to abstract machine (AM) for CBPV. This repository also comes with an interpreter for AM.

- [x] Call-By-Value $\lambda$-calculus-To-CBPV pass
- [ ] Call-By-Name $\lambda$-calculus-To-CBPV pass
- [ ] Call-By-Need $\lambda$-calculus-To-CBPV pass
- [x] Simple CBPV optimization pass
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
- [ ] PrintString effect
- [ ] Read effect
- [ ] Exception-handling effects
- [ ] General I/O effect
