//
// This file is part of
// FsAlg: Generic Linear Algebra Library
//
// Copyright (c) 2015, National University of Ireland Maynooth (Atilim Gunes Baydin, Barak A. Pearlmutter)
//
// FsAlg is released under the BSD license.
// (See accompanying LICENSE file.)
//
// Written by:
//
//   Atilim Gunes Baydin
//   atilimgunes.baydin@nuim.ie
//
//   Barak A. Pearlmutter
//   barak@cs.nuim.ie
//
//   Brain and Computation Lab
//   Hamilton Institute & Department of Computer Science
//   National University of Ireland Maynooth
//   Maynooth, Co. Kildare
//   Ireland
//
//   www.bcl.hamilton.ie
//

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
