
namespace FsAlg.Generic

/// Linear algebra operations module (automatically opened)
[<AutoOpen>]
module Ops =
    /// Converts array, list, or sequence `v` into a Vector
    let inline vector v = Vector.ofSeq v
    /// Converts array, list, or sequence `v` into a Vector, first passing the elements through a conversion function `f`
    let inline vectorBy f v = Vector.map f (Vector.ofSeq v)
    /// Converts 2d array `m` into a Matrix
    let inline matrix m = Matrix.ofSeq m
    /// Converts 2d array `m` into a Matrix, first passing the elements through a conversion function `f`
    let inline matrixBy f m = Matrix.map f (Matrix.ofSeq m)
