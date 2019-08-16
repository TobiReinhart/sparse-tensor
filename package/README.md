# The `sparse-tensor` Package

sparse-tensor is a Haskell tensor algebra library. It defines the usual tensor algebra functions such as

- addition
```
result = t1 &+ t2
```
- scalar multiplication
```
result = s &. t1
```
- tensor product
```
result = t1 &* t2
```
- or symmetrizations
```
result = symTens (0,1) t -- symmetrization in first two indices
```