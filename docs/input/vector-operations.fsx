(*** hide ***)
#r "../../src/FsAlg/bin/Debug/FsAlg.dll"

(**
Vector Operations
=================

This page lists only a selection of commonly used vector operations provided by FsAlg.

Please refer to [API Reference](reference/index.html) for a full list of supported operations.

The **FsAlg.Generic.Vector** module provides functionality similar to the F# [Collecitons.Array](https://msdn.microsoft.com/en-us/library/ee370273.aspx) module, for creating and manipulating vectors.

Creating Vectors
----------------
*)

open FsAlg.Generic

let v1 = vector [1.; 2.; 3.]
let v2 = Vector.create 3 1.
let v3 = Vector.init 3 (fun i -> exp (float i))

(*** hide, define-output: o ***)
printf "val v1 : Vector<float> = Vector [|1.0; 2.0; 3.0|]
val v2 : Vector<float> = Vector [|1.0; 1.0; 1.0|]
val v3 : Vector<float> = Vector [|1.0; 2.718281828; 7.389056099|]"
(*** include-output: o ***)

(**
Basic Operations
----------------   
*)

let v4  = v1 + v2  // Vector addition
let v5  = v1 - v2  // Vector substraction
let v6  = v1 * v2  // Vector inner product (dot / scalar product)
let v7  = v1 %* v2 // Vector cross product
let v8  = v1 .* v2 // Vector element-wise (Hadamard) product
let v9  = v1 ./ v2 // Vector element-wise (Hadamard) division
let v10 = v1 + 2.  // Add scalar to vector
let v11 = v1 - 2.  // Subtract scalar from vector
let v12 = 2. - v1  // Subtract each element of vector from scalar
let v13 = v1 * 2.  // Vector-scalar multiplication
let v14 = v1 / 2.  // Vector-scalar division
let v15 = 2. / v1  // Divides each element of vector by scalar
let v16 = -v1      // Unary negation

(*** hide, define-output: o2 ***)
printf "val v4 : Vector<float> = Vector [|2.0; 3.0; 4.0|]
val v5 : Vector<float> = Vector [|0.0; 1.0; 2.0|]
val v6 : float = 6.0
val v7 : Vector<float> = Vector [|-1.0; 2.0; -1.0|]
val v8 : Vector<float> = Vector [|1.0; 2.0; 3.0|]
val v9 : Vector<float> = Vector [|1.0; 2.0; 3.0|]
val v10 : Vector<float> = Vector [|3.0; 4.0; 5.0|]
val v11 : Vector<float> = Vector [|-1.0; 0.0; 1.0|]
val v12 : Vector<float> = Vector [|1.0; 0.0; -1.0|]
val v13 : Vector<float> = Vector [|2.0; 4.0; 6.0|]
val v14 : Vector<float> = Vector [|0.5; 1.0; 1.5|]
val v15 : Vector<float> = Vector [|2.0; 1.0; 0.6666666667|]
val v16 : Vector<float> = Vector [|-1.0; -2.0; -3.0|]"
(*** include-output: o2 ***)

(**
Vector Norms
------------   
*)

let n1 = Vector.l1norm v1    // L1 norm
let n2 = Vector.l2norm v1    // L2 norm
let n3 = Vector.l2normSq v1  // Squared L2 norm
let n4 = Vector.lpnorm 3. v1 // Lp norm

(*** hide, define-output: o3 ***)
printf "val n1 : float = 6.0
val n2 : float = 3.741657387
val n3 : float = 14.0
val n4 : float = 3.301927249"
(*** include-output: o3 ***)

(**
Accessing Elements & Conversions
--------------------------------
*)

let e1 = v1.[0]          // 1st element of v1
let e2 = Vector.get v1 1 // 2nd element of v1
let e3 = v1.[..1]        // Slice, until 2nd element
let e4 = v1.[1..2]       // Slice, between 2nd and 3rd elements
let v20 = Vector.ofSeq [1.; 2.; 3.] // Convert sequence to vector
let v21 = vector [1.; 2.; 3.]       // Same with above, supports lists,
let v22 = vector [|1.; 2.; 3.|]     // arrays,
let v23 = vector (seq {yield 1.})   // and sequences.
let aa1 = Vector.toArray v1 // Convert vector to array
let sq1 = Vector.toSeq v1   // Convert vector to sequence

(*** hide, define-output: o4 ***)
printf "val e1 : float = 1.0
val e2 : float = 2.0
val e3 : Vector<float> = Vector [|1.0; 2.0|]
val e4 : Vector<float> = Vector [|2.0; 3.0|]
val v20 : Vector<float> = Vector [|1.0; 2.0; 3.0|]
val v21 : Vector<float> = Vector [|1.0; 2.0; 3.0|]
val v22 : Vector<float> = Vector [|1.0; 2.0; 3.0|]
val v23 : Vector<float> = Vector [|1.0|]
val aa1 : float [] = [|1.0; 2.0; 3.0|]
val sq1 : seq<float>"
(*** include-output: o4 ***)

(**
Mutating/Replacing Elements
------------------
*)
v1.[0] <- 4. // Mutate an element
Vector.replace (fun x -> x + 2.) v1 // Replace by mapping a function, mutating in place
Vector.replacei (fun i x -> x + float i) v1 // Replace in place, with index
Vector.replace2 (fun x y -> x - y) v1 v2 // Replace v1 in place, using a function of v1 and v2
Vector.replacei2 (fun i x y -> x - y + float i) v1 v2 // Replace v1 in place, with index
Vector.replaceWith v1 v2 // Replace elements of v1 with v2, mutating in place

(**
Splitting and Concatenating
---------------------------   
*)

let ss1 = Vector.splitEqual 3 (vector [1.; 2.; 3.; 4.; 5.; 6.]) // Split into 3 vectors of equal length
let ss2 = Vector.split [2; 4] (vector [1.; 2.; 3.; 4.; 5.; 6.]) // Split into vectors of given lengths
let cc1 = Vector.concat ss1 // Concatenate vectors into one

(*** hide, define-output: o5 ***)
printf "val ss1 : seq<Vector<float>>
val ss2 : seq<Vector<float>>
val cc1 : Vector<float> = Vector [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0|]"
(*** include-output: o5 ***)

(**
Mathematica and MATLAB Strings
------------------------------

You can generate string representations of vectors that you can copy and paste into Mathematica notebooks or MATLAB.   
*)

let s1 = v1.ToMathematicaString()
let s2 = v1.ToMatlabString()

(*** hide, define-output: o6 ***)
printf "val s1 : string = \"{1.00, 2.00, 3.00}\"
val s2 : string = \"[1.00 2.00 3.00]\""
(*** include-output: o6 ***)

(**
Other Operations
---------------------
The following are just a selection of other operations. Please refer to [API Reference](reference/index.html) for a full list of supported operations.
*)

let len = Vector.length v1 // Length of vector
let min = Vector.min v1  // Minimum element of vector
let max = Vector.max v1  // Maximum element of vector
let sum = Vector.sum v1  // Sum elements of vector
let v17 = Vector.unitVector v1 // Get unit vector codirectional with v1
let v18 = Vector.map (fun x -> sin x) v1 // Map function to vector
let v19 = Vector.fold (fun s x -> s + sin x) 0. v1 // Fold a vector

(*** hide, define-output: o7 ***)
printf "val len : int = 3
val min : float = 1.0
val max : float = 3.0
val sum : float = 6.0
val v17 : Vector<float> = Vector [|0.2672612419; 0.5345224838; 0.8017837257|]
val v18 : Vector<float> = Vector [|0.8414709848; 0.9092974268; 0.1411200081|]
val v19 : float = 1.89188842"
(*** include-output: o7 ***)