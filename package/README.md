# The `sparse-tensor` Package

sparse-tensor is a Haskell tensor algebra library. It defines the usual tensor algebra functions such as

- addition
```haskell
result = t1 &+ t2
```
- scalar multiplication
```haskell
result = s &. t
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

## Predefined tensors
The package comes with pre-defined tensor types. Basic tensors of these types for applications in mathematical physics are exported by `Math.Tensor.Examples.Gravity`. It is of course possible to define further custom tensor types and tensors.

`Math.Tensor.LorentzGenerator` exports functionality to generate a basis for the space of Lorentz-invariant tensors of certain rank which obey certain symmetries.

## Automatic differentiation
`sparse-tensor` also supports tensors with **functions** as values. For such tensors, the package also provides the `partial` function for automatic differentiation. `Math.Tensor.Examples.Gravity.Schwarzschild` exports the Einstein tensor for a Schwarzschild spacetime, calculated from the Schwarzschild metric:
```haskell
>>> let e = einstein 2 -- einstein tensor for Schwarzschild metric with r_s = 2
>>> e `evalSec` [1.2, 3.1, 1.3, 2.2] -- evaluate at spacetime point
ZeroTensor
```

## Symbolic calculations
The package can also handle **symbolic** tensor values. All manipulations, including differentiation, are then performed on strings which may be passed to a computer algebra engine. `sparse-tensor` itself cannot yet process these symbolic values. `Math.Tensor.Examples.Gravity.SchwarzschildSymbolic` exports the Schwarzschild metric with symbolic entries and methods to calculate derived geometric entities:
```haskell
>>> let g  = schwarzschildS
>>> let g' = schwarzschildS'
>>> let gamma = christoffelS g g'
>>> let comps = toListT2 gamma     -- get list of components
>>> print $ snd $ comps !! 1       -- component gamma^t_tr
SSymbolic "(1 % 2)*((1/(1 - rs/r))*(diff(1 - rs / r, r)))"
```