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

open FsAlg.Generic.Util

/// Generic vector type
[<NoEquality; NoComparison>]
type Vector<'T when 'T : (static member Zero : 'T)
                and 'T : (static member One : 'T)
                and 'T : (static member (+) : 'T * 'T -> 'T)
                and 'T : (static member (-) : 'T * 'T -> 'T)
                and 'T : (static member (*) : 'T * 'T -> 'T)
                and 'T : (static member (/) : 'T * 'T -> 'T)
                and 'T : (static member (~-) : 'T -> 'T)
                and 'T : (static member Abs : 'T -> 'T)
                and 'T : (static member Pow : 'T * 'T -> 'T)
                and 'T : (static member Sqrt : 'T -> 'T)
                and 'T : (static member op_Explicit : 'T -> float)
                and 'T : comparison> =
    | ZeroVector of 'T
    | Vector of 'T[]
    /// ZeroVector
    static member inline Zero = ZeroVector LanguagePrimitives.GenericZero<'T>
    /// Converts Vector `v` to float[]
    static member inline op_Explicit(v:Vector<'T>) =
        match v with
        | Vector v -> Array.map float v
        | ZeroVector _ -> [||]
    /// Gets the element of this Vector at the given position `i`
    member inline v.Item
        with get i =
            match v with
            | Vector v -> v.[i]
            | ZeroVector z -> z
    /// Gets the first element of this Vector
    member inline v.FirstItem =
        match v with
        | Vector v -> v.[0]
        | ZeroVector z -> z
    /// Gets the total number of elements of this Vector
    member inline v.Length =
        match v with
        | Vector v -> v.Length
        | ZeroVector _ -> 0
    /// Gets the L1 (Manhattan) norm of this Vector
    member inline v.GetL1Norm() =
        match v with
        | Vector v -> Array.sumBy abs v
        | ZeroVector z -> z
    /// Gets the L2 (Euclidean) norm of this Vector
    member inline v.GetL2Norm() =
        match v with
        | Vector v -> sqrt (Array.sumBy (fun x -> x * x) v)
        | ZeroVector z -> z
    /// Gets the squared L2 (Euclidean) norm of this Vector
    member inline v.GetL2NormSq() =
        match v with
        | Vector v -> Array.sumBy (fun x -> x * x) v
        | ZeroVector z -> z
    /// Gets the Lp norm (or p-norm) of this Vector, with the given `p`
    member inline v.GetLPNorm(p:'T):'T =
        match v with
        | Vector v -> (Array.sumBy (fun x -> (abs x) ** p) v) ** (LanguagePrimitives.GenericOne<'T> / p)
        | ZeroVector z -> z
    /// Gets the minimum element of this Vector
    member inline v.GetMin() =
        match v with
        | Vector v -> Array.min v
        | ZeroVector z -> z
    /// Gets the minimum element of this Vector, compared by using Operators.min on the result of function `f`
    member inline v.GetMinBy(f) =
        match v with
        | Vector v -> Array.minBy f v
        | ZeroVector z -> z
    /// Gets the maximum element of this Vector
    member inline v.GetMax() =
        match v with
        | Vector v -> Array.max v
        | ZeroVector z -> z
    /// Gets the maximum element of this Vector, compared by using Operators.max on the result of function `f`
    member inline v.GetMaxBy(f) =
        match v with
        | Vector v -> Array.maxBy f v
        | ZeroVector z -> z
    /// Gets the unit Vector codirectional with this Vector
    member inline v.GetUnitVector() =
        match v with
        | Vector vv -> let n = v.GetL2Norm() in Vector (Array.map (fun x -> x / n) vv)
        | ZeroVector z -> ZeroVector z
    /// Returns a sequence of Vectors that are obtained by splitting this Vector into `n` subvectors of equal length. The length of this Vector must be an integer multiple of `n`, otherwise ArgumentException is raised.
    member inline v.Split(n:int) =
        match v with
        | Vector v ->
            if n <= 0 then invalidArg "" "For splitting this Vector, n should be a positive integer."
            let l = (float v.Length) / (float n)
            if not (isInteger l) then invalidArg "" "Cannot split Vector into n equal pieces when length of Vector is not an integer multiple of n."
            seq {for i in 0 .. (int l) .. (v.Length - 1) do yield Vector (Array.sub v i (int l))}
        | ZeroVector _ -> seq {yield v}
    /// Gets a string representation of this Vector that can be pasted into a Mathematica notebook
    member inline v.ToMathematicaString() = 
        let sb = System.Text.StringBuilder()
        sb.Append("{") |> ignore
        for i = 0 to v.Length - 1 do
            sb.Append(sprintf "%.2f" (float v.[i])) |> ignore
            if i < v.Length - 1 then sb.Append(", ") |> ignore
        sb.Append("}") |> ignore
        sb.ToString()
    /// Gets a string representation of this Vector that can be pasted into MATLAB
    member inline v.ToMatlabString() =
        let sb = System.Text.StringBuilder()
        sb.Append("[") |> ignore
        for i = 0 to v.Length - 1 do
            sb.Append(sprintf "%.2f" (float v.[i])) |> ignore
            if i < v.Length - 1 then sb.Append(" ") |> ignore
        sb.Append("]") |> ignore
        sb.ToString()
    /// Converts the elements of this Vector to another type, using the given conversion function `f`
    member inline v.Convert(f:'T->'a):Vector<'a> =
        match v with
        | Vector v -> Vector (Array.map f v)
        | ZeroVector _ -> ZeroVector LanguagePrimitives.GenericZero<'a>
    /// Creates a copy of this Vector
    member inline v.Copy() =
        match v with
        | Vector v -> Vector (Array.copy v)
        | ZeroVector z -> ZeroVector z
    /// Converts this Vector to an array
    member inline v.ToArray() =
        match v with
        | Vector v -> v
        | ZeroVector _ -> [||]
    /// Converts this Vector to a sequence
    member inline v.ToSeq() =
        match v with
        | Vector v -> Array.toSeq v
        | ZeroVector _ -> Seq.empty
    /// Creates a new Vector that contains the given subrange of elements, specified by start index `s` and count `c`
    member inline v.GetSubVector(s, c) =
        match v with
        | Vector v -> Vector (Array.sub v s c)
        | ZeroVector _ -> Vector.Zero
    /// Adds Vector `a` to Vector `b`
    static member inline (+) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (+) a b) with | _ -> invalidArg "" "Cannot add two Vectors of different dimensions."
        | Vector _, ZeroVector _ -> a
        | ZeroVector _, Vector _ -> b
        | ZeroVector _, ZeroVector _ -> Vector.Zero
    /// Subtracts Vector `b` from Vector `a`
    static member inline (-) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (-) a b) with | _ -> invalidArg "" "Cannot subtract two Vectors of different dimensions."
        | Vector _, ZeroVector _ -> a
        | ZeroVector _, Vector b -> Vector (Array.map (~-) b)
        | ZeroVector _, ZeroVector _ -> Vector.Zero
    /// Computes the inner product (dot / scalar product) of Vector `a` and Vector `b`
    static member inline (*) (a:Vector<'T>, b:Vector<'T>):'T =
        match a, b with
        | Vector a, Vector b -> try Array.map2 (*) a b |> Array.sum with | _ -> invalidArg "" "Cannot multiply two Vectors of different dimensions."
        | Vector _, ZeroVector _ -> LanguagePrimitives.GenericZero<'T>
        | ZeroVector _, Vector _ -> LanguagePrimitives.GenericZero<'T>
        | ZeroVector _, ZeroVector _ -> LanguagePrimitives.GenericZero<'T>
    /// Computes the cross product of Vector `a` and Vector `b` (three-dimensional)
    static member inline (%*) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector va, Vector vb ->
            if (a.Length <> 3) || (b.Length <> 3) then invalidArg "" "The cross product is only defined for three-dimensional vectors."
            Vector [|va.[1] * vb.[2] - va.[2] * vb.[1]; va.[2] * vb.[0] - va.[0] * vb.[2]; va.[0] * vb.[1] - va.[1] * vb.[0]|]
        | Vector _, ZeroVector _ -> Vector.Zero
        | ZeroVector _, Vector _ -> Vector.Zero
        | ZeroVector _, ZeroVector _ -> Vector.Zero
    /// Multiplies Vector `a` and Vector `b` element-wise (Hadamard product)
    static member inline (.*) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (*) a b) with | _ -> invalidArg "" "Cannot multiply two Vectors of different dimensions."
        | Vector _, ZeroVector _ -> Vector.Zero
        | ZeroVector _, Vector _ -> Vector.Zero
        | ZeroVector _, ZeroVector _ -> Vector.Zero
    /// Divides Vector `a` by Vector `b` element-wise (Hadamard division)
    static member inline (./) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (/) a b) with | _ -> invalidArg "" "Cannot divide two Vectors of different dimensions."
        | Vector _, ZeroVector _-> raise (new System.DivideByZeroException("Attempted to divide a Vector by a ZeroVector."))
        | ZeroVector _, Vector _ -> Vector.Zero
        | ZeroVector _, ZeroVector _ -> raise (new System.DivideByZeroException("Attempted to divide a ZeroVector by a ZeroVector."))
    /// Adds scalar `b` to each element of Vector `a`
    static member inline (+) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map ((+) b) a)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot add a scalar to a ZeroVector."
    /// Adds scalar `a` to each element of Vector `b`
    static member inline (+) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((+) a) b)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot add a scalar to a ZeroVector."
    /// Subtracts scalar `b` from each element of Vector `a`
    static member inline (-) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map (fun x -> x - b) a)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot subtract a scalar from a ZeroVector."
    /// Subtracts each element of Vector `b` from scalar `a`
    static member inline (-) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((-) a) b)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot add subtract a ZeroVector from a scalar."
    /// Multiplies each element of Vector `a` by scalar `b`
    static member inline (*) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map ((*) b) a)
        | ZeroVector _ -> Vector.Zero
    /// Multiplies each element of Vector `b` by scalar `a`
    static member inline (*) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((*) a) b)
        | ZeroVector _ -> Vector.Zero
    /// Divides each element of Vector `a` by scalar `b`
    static member inline (/) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map (fun x -> x / b) a)
        | ZeroVector _ -> Vector.Zero
    /// Divides scalar `a` by each element of Vector `b`
    static member inline (/) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((/) a) b)
        | ZeroVector _ -> raise (new System.DivideByZeroException("Attempted division by a ZeroVector."))
    /// Gets the negative of Vector `a`
    static member inline (~-) (a:Vector<'T>):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map (~-) a)
        | ZeroVector _ -> Vector.Zero


/// Provides basic operations on Vector types. (Implementing functionality similar to Microsoft.FSharp.Collections.Array)
[<RequireQualifiedAccess>]
module Vector =
    /// Creates a Vector from sequence `s`
    let inline ofSeq s = Vector (Array.ofSeq s)
    /// Converts Vector `v` to an array
    let inline toArray (v:Vector<_>) = v.ToArray()
    /// Returns Vector `v` as a sequence
    let inline toSeq (v:Vector<_>) = v.ToSeq()
    /// Builds a new Vector that contains the elements of each of the given sequence of Vectors `v`
    let inline concat (v:seq<Vector<_>>) = Seq.map toArray v |> Array.concat |> ofSeq
    /// Creates a copy of Vector `v`
    let inline copy (v:Vector<_>) = v.Copy()
    /// Creates a Vector with `n` elements, all having value `v`
    let inline create n v = Vector (Array.create n v)
    /// Creates a Vector with `n` elements, where the element with index `i` has value `v` and the rest of the elements have value 0
    let inline createBasis n i v = Vector (Array.init n (fun j -> if j = i then v else LanguagePrimitives.GenericZero))
    /// Tests if any element of Vector `v` satisfies predicate `p`
    let inline exists p (v:Vector<_>) = v |> toArray |> Array.exists p
    /// Returns the first element of Vector `v` for which predicate `p` is true
    let inline find p (v:Vector<_>) = v |> toArray |> Array.find p
    /// Returns the index of the first element of Vector `v` for which predicate `p` is true
    let inline findIndex p (v:Vector<_>) = v |> toArray |> Array.findIndex p
    /// Applies function `f` to each element of Vector `v`, threading an accumulator (with initial state `s`) through the computation. If the input function is f and the elements are i0...iN then computes f (... (f s i0)...) iN.
    let inline fold f s (v:Vector<_>) = v |> toArray |> Array.fold f s
    /// Applies function `f` to each element of Vector `v`, threading an accumulator (with initial state `s`) through the computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
    let inline foldBack f s (v:Vector<_>) = v |> toArray |> Array.foldBack f s
    /// Tests if all elements of Vector `v` satisfy predicate `p`
    let inline forall p (v:Vector<_>) = v |> toArray |> Array.forall p
    /// Returns the value of the element with the given index `i`
    let inline get i (v:Vector<_>) = v.[i]
    /// Creates a Vector with dimension `n` and a generator function `f` to compute the elements
    let inline init n f = Vector (Array.init n f)
    /// Applies function `f` to each element of Vector `v`
    let inline iter f (v:Vector<_>) = v |> toArray |> Array.iter f
    /// Applies function `f` to each element of Vector `v`. The integer passed to function `f` indicates the index of element.
    let inline iteri f (v:Vector<_>) = v |> toArray |> Array.iteri f
    /// Gets the L1 (Manhattan) norm of Vector `v`
    let inline l1norm (v:Vector<_>) = v.GetL1Norm()
    /// Gets the L2 (Euclidean) norm of Vector `v`. This is the same with `Vector.norm`.
    let inline l2norm (v:Vector<_>) = v.GetL2Norm()
    /// Gets the squared L2 (Euclidean) norm of Vector `v`. This is the same with `Vector.normSq`.
    let inline l2normSq (v:Vector<_>) = v.GetL2NormSq()
    /// Returns the length of Vector `v`
    let inline length (v:Vector<_>) = v.Length
    /// Gets the Lp norm (or p-norm) of Vector `v`, with the given `p`
    let inline lpnorm p (v:Vector<_>) = v.GetLPNorm(p)
    /// Creates a Vector whose elements are the results of applying function `f` to each element of Vector `v`
    let inline map f (v:Vector<_>) = v |> toArray |> Array.map f |> Vector
    /// Creates a Vector whose elements are the results of applying function `f` to each element of Vector `v`. An element index is also supplied to function `f`.
    let inline mapi f (v:Vector<_>) = v |> toArray |> Array.mapi f |> Vector
    /// Returns the maximum of all elements of Vector `v`
    let inline max (v:Vector<_>) = v.GetMax()
    /// Returns the maximum of all elements of Vector `v`, compared by using Operators.max on the result of function `f`
    let inline maxBy f (v:Vector<_>) = v.GetMaxBy(f)
    /// Returns the minimum of all elements of Vector `v`
    let inline min (v:Vector<_>) = v.GetMin()
    /// Returns the minimum of all elements of Vector `v`, compared by using Operators.min on the result of function `f`
    let inline minBy f (v:Vector<_>) = v.GetMinBy(f)
    /// Gets the L2 (Euclidean) norm of Vector `v`. This is the same with `Vector.l2norm`.
    let inline norm v = l2norm v
    /// Gets the squared L2 (Euclidean) norm of Vector `v`. This is the same with `Vector.l2normSq`.
    let inline normSq v = l2normSq v
    /// Applies function `f` to each element of Vector `v`, threading an accumulator argument through the computation. If the input function is f and the elements are i0...iN, then computes f (... (f i0 i1)...) iN.
    let inline reduce f (v:Vector<_>) = v |> toArray |> Array.reduce f
    /// Applies function `f` to each element of Vector `v`, threading an accumulator argument through the computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN-1 iN)).
    let inline reduceBack f (v:Vector<_>) = v |> toArray |> Array.reduceBack f
    /// Like Vector.fold, but returns the intermediate and final results
    let inline scan f s (v:Vector<_>) = v |> toArray |> Array.scan f s
    /// Like Vector.foldBack, but returns both the intermediate and final results
    let inline scanBack f s (v:Vector<_>) = v |> toArray |> Array.scanBack f s
    /// Returns a sequence of Vectors that are obtained by splitting Vector `v` into `n` subvectors of equal length. The length of Vector `v` must be an integer multiple of `n`, otherwise ArgumentException is raised.
    let inline split n (v:Vector<_>) = v.Split(n)
    /// Creates a Vector with `n` elements, where the `i`-th element is 1 and the rest of the elements are 0
    let inline standardBasis n i = createBasis n i LanguagePrimitives.GenericOne
    /// Creates a new Vector that contains the given subrange of Vector `v`, specified by start index `s` and count `c`
    let inline sub (v:Vector<_>) s c = v.GetSubVector(s, c)
    /// Returns the sum of all the elements in Vector `v`
    let inline sum (v:Vector<_>) = v |> toArray |> Array.sum
    /// Returns the sum of the results generated by applying function `f` to each element of Vector `v`
    let inline sumBy f (v:Vector<_>) = v |> toArray |> Array.sumBy f
    /// Gets the unit vector codirectional with Vector `v`
    let inline unitVector (v:Vector<_>) = v.GetUnitVector()
