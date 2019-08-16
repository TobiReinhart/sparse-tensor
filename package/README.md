# The `sparse-tensor` Package

sparse-tensor is a Haskell tensor algebra library. It defines the usual tensor algebra functions such as

- addition
```haskell
result = t1 &+ t2
```
- scalar multiplication
```haskell
result = s &. t1
```
- tensor product
```haskell
result = t1 &* t2
```
- or symmetrizations
```haskell
result = symTens (0,1) t -- symmetrization in first two indices
```
## The `Tensor` type

Tensor types can be defined with any value type and index types. For example, a tensor type with `n` contravariant and `m` covariant 4-d spacetime indices ranging from 0 to 3 and `Rational` values can be defined as
```haskell
type MyTensor n m  = AbsTensor2 n m Ind3 (SField Rational)
```

These operations on tensors are **type-safe**, for example it is not possible to add two tensors of different rank,
```haskell
>>> :set -XDataKinds
>>> (undefined :: MyTensor 0 1) &+ (undefined :: MyTensor 0 2)
```
```
<interactive>:3:33: error:
    • Couldn't match type ‘2’ with ‘1’
      [...]
```
as this causes a type error at **compile time**.

The package comes with pre-defined tensor types and basic tensors that cover many applications in mathematical physics. It is of course possible to define further custom tensor types and tensors.

Furthermore, `sparse-tensor` provides the `LorentzGenerator` module which can be used to generate a basis for the space of Lorentz-invariant tensors of certain rank which obey certain symmetries.

## Automatic differentiation
`sparse-tensor` also supports tensors with **functions** as values. For such tensors, the package also provides the `partial` function for automatic differentiation.

## Symbolic calculations
The package can also handle **symbolic** tensor values. All manipulations, including differentiation, are then performed on strings which may be passed to a computer algebra engine. `sparse-tensor` itself cannot yet process these symbolic values.