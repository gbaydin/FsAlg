(*** hide ***)
#r "../../src/FsAlg/bin/Debug/FsAlg.dll"

(**
Matrix Operations
=================

This page lists only a selection of commonly used matrix operations provided by FsAlg.

Please refer to [API Reference](reference/index.html) for a full list of supported operations.

The **FsAlg.Generic.Matrix** module provides functionality similar to the F# [Collecitons.Array2D](https://msdn.microsoft.com/en-us/library/ee353794.aspx) module, for creating and manipulating matrices.

Creating Matrices
-----------------
*)

open FsAlg.Generic

let m1 = matrix [[1.; 2.]; [3.; 4.]]
let m2 = Matrix.create 2 2 1.
let m3 = Matrix.init 2 2 (fun i j -> exp (float (i + j)))

(*** hide, define-output: o ***)
printf "val m1 : Matrix<float> = Matrix [[1.0; 1.0]
                                 [1.0; 1.0]]
val m2 : Matrix<float> = Matrix [[1.0; 1.0]
                                 [1.0; 1.0]]
val m3 : Matrix<float> = Matrix [[1.0; 2.718281828]
                                 [2.718281828; 7.389056099]]"
(*** include-output: o ***)

(**
Basic Operations
----------------   
*)

let m4 = m1 + m2  // Matrix addition
let m5 = m1 - m2  // Matrix subtraction
let m6 = m1 * m2  // Matrix product
let m7 = m1 .* m2 // Matrix element-wise (Hadamard) product
let m8 = m1 ./ m2 // Matrix element-wise (Hadamard) division

let v = vector [1.; 2.]
let m9 = m1 * v   // Matrix-vector product
let m10 = v * m1  // Vector-matrix product

let m11 = m1 + 2. // Add scalar to matrix
let m12 = m1 - 2. // Subtract scalar from matrix
let m13 = 2. - m1 // Subtract each element of matrix from scalar
let m14 = m1 * 2. // Matrix-scalar multiplication
let m15 = m1 / 2. // Matrix-scalar division
let m16 = 2. / m1 // Divides each element of matrix by scalar
let m17 = -m1     // Unary negation

(*** hide, define-output: o2 ***)
printf "val m4 : Matrix<float> = Matrix [[2.0; 3.0]
                                 [4.0; 5.0]]
val m5 : Matrix<float> = Matrix [[0.0; 1.0]
                                 [2.0; 3.0]]
val m6 : Matrix<float> = Matrix [[3.0; 3.0]
                                 [7.0; 7.0]]
val m7 : Matrix<float> = Matrix [[1.0; 2.0]
                                 [3.0; 4.0]]
val m8 : Matrix<float> = Matrix [[1.0; 2.0]
                                 [3.0; 4.0]]
val v : Vector<float> = Vector [|1.0; 2.0|]
val m9 : Vector<float> = Vector [|5.0; 11.0|]
val m10 : Vector<float> = Vector [|7.0; 10.0|]
val m11 : Matrix<float> = Matrix [[3.0; 4.0]
                                  [5.0; 6.0]]
val m12 : Matrix<float> = Matrix [[-1.0; 0.0]
                                  [1.0; 2.0]]
val m13 : Matrix<float> = Matrix [[1.0; 0.0]
                                  [-1.0; -2.0]]
val m14 : Matrix<float> = Matrix [[2.0; 4.0]
                                  [6.0; 8.0]]
val m15 : Matrix<float> = Matrix [[0.5; 1.0]
                                  [1.5; 2.0]]
val m16 : Matrix<float> = Matrix [[2.0; 1.0]
                                  [0.6666666667; 0.5]]
val m17 : Matrix<float> = Matrix [[-1.0; -2.0]
                                  [-3.0; -4.0]]"
(*** include-output: o2 ***)

(**
Matrix Operations
-----------------   
*)

let m1det = Matrix.det m1         // Determinant
let m1inv = Matrix.inverse m1     // Inverse
let m1eig = Matrix.eigenvalues m1 // Eigenvalues
let m1t   = Matrix.transpose m1   // Transpose
let m1tr  = Matrix.trace m1       // Trace
let m1dia = Matrix.diagonal m1    // Diagonal

(*** hide, define-output: o3 ***)
printf "val m1det : float = -2.0
val m1inv : Matrix<float> = Matrix [[-2.0; 1.0]
                                    [1.5; -0.5]]
val m1eig : Vector<float> = Vector [|5.372281312; -0.3722813117|]
val m1t : Matrix<float> = Matrix [[1.0; 3.0]
                                  [2.0; 4.0]]
val m1tr : float = 5.0
val m1dia : Vector<float> = Vector [|1.0; 4.0|]"
(*** include-output: o3 ***)

(**
Solving Systems of Linear Equations
-----------------------------------
We can solve a [system of linear equations](http://en.wikipedia.org/wiki/System_of_linear_equations) represented in matrix form $\mathbf{A} \mathbf{x} = \mathbf{b} \;,$ which has solution $\mathbf{x} = \mathbf{A}^{-1} \mathbf{b}\;$.

For example, the system

$$$
  \begin{eqnarray*}
  3x + 2y - z &=& 1\\
  2x - 2y +4z &=& -2\\
  -x + 0.5y -z &=& 0\\
  \end{eqnarray*}

can be represented as

$$$
    \begin{bmatrix}
    3 & 2 & -1\\
    2 & -2 & 4\\
    -1& 0.5 &-1\\
    \end{bmatrix} \begin{bmatrix}
    x\\
    y\\
    z\\
    \end{bmatrix} = \begin{bmatrix}
    1\\
    -2\\
    0\\
    \end{bmatrix}

and has solution

$$$
  \begin{eqnarray*}
  x &=& 1\\
  y &=& -2\\
  z &=& -2\\
  \end{eqnarray*}\; .
*)

let A = matrix [[3.; 2.; -1.]; [2.; -2.; 4.]; [-1.; 0.5; -1.]]
let b = vector [1.; -2.; 0.]

let x = Matrix.solve A b

(*** hide, define-output: o4 ***)
printf "val A : Matrix<float> = Matrix [[3.0; 2.0; -1.0]
                                [2.0; -2.0; 4.0]
                                [-1.0; 0.5; -1.0]]
val b : Vector<float> = Vector [|1.0; -2.0; 0.0|]
val x : Vector<float> = Vector [|1.0; -2.0; -2.0|]"
(*** include-output: o4 ***)

(**
Matrix Decompositions
--------------------- 
*)

let lu, piv, togg = Matrix.decomposeLU m1 // LU decomposition
let q, r = Matrix.decomposeQR m1 // QR decomposition

(*** hide, define-output: o5 ***)
printf "val togg : float = -1.0
val piv : int [] = [|1; 0|]
val lu : Matrix<float> = Matrix [[3.0; 4.0]
                                 [0.3333333333; 0.6666666667]]
val r : Matrix<float> = Matrix [[3.16227766; 4.427188724]
                                [4.440892099e-16; 0.632455532]]
val q : Matrix<float> = Matrix [[0.316227766; 0.9486832981]
                                [0.9486832981; -0.316227766]]"
(*** include-output: o5 ***)

(**
Accessing Elements & Conversions
--------------------------------
*)

let el1 = m1.[0, 0]         // Element at 0, 0
let el2 = Matrix.get m1 1 1 // Element at 1, 1
let sl1 = m1.[0..1, 1..]    // Slice, between rows 0 and 1, columns 1 and beyond
let sl2 = m1.[*, 0..1]      // Slice, all rows, between columns 0 and 1
let sl3 = m1.[0, *]         // Return row 0 as a vector
let sl4 = Matrix.row 0 m1   // Return row 0 as a vector
let sl5 = m1.[*, 1]         // Return column 1 as a vector
let sl6 = Matrix.col 1 m1   // Return column 1 as a vector
let sl7 = Matrix.toRows m1  // Return rows as a sequence of vectors
let m18 = Matrix.ofRows [vector [1.;2.]; vector [3.;4.]] // Create matrix from row vectors
let sl8 = Matrix.toCols m1  // Return columns as a sequence of vectors
let m19 = Matrix.ofCols [vector [1.;2.]; vector [3.;4.]] // Create matrix from column vectors
let m20 = Matrix.ofSeqSeq [[1.; 2.]; [3.; 4.]]  // Convert sequence of sequences to matrix
let m21 = matrix [[1.; 2.]; [3.; 4.]]        // Same with above, supports lists,
let m22 = matrix [|[|1.; 2.|]; [|3.; 4.|]|]  // arrays,
let m23 = matrix (seq {yield seq {yield 1.}}) // and sequences.
let aa1 = Matrix.toArrayArray m1   // Convert matrix to jagged array
let aa2 = Matrix.toArray2D m1   // Convert matrix to 2d array
let aa3 = Matrix.toArray m1     // Convert matrix to 1d array, scanning left to right, top to bottom
let ma3 = Matrix.ofArray 2 aa3  // Convert 1d array to matrix, populating two rows
let vv1 = Matrix.toVector m1    // Convert matrix to vector, scanning left to right, top to bottom
let mv1 = Matrix.ofVector 2 vv1 // Convert vector to matrix, populating two rows

(*** hide, define-output: o6 ***)
printf "val el1 : float = 1.0
val el2 : float = 4.0
val sl1 : Matrix<float> = Matrix [[2.0]
                                  [4.0]]
val sl2 : Matrix<float> = Matrix [[1.0; 2.0]
                                  [3.0; 4.0]]
val sl3 : Vector<float> = Vector [|1.0; 2.0|]
val sl4 : Vector<float> = Vector [|1.0; 2.0|]
val sl5 : Vector<float> = Vector [|2.0; 4.0|]
val sl6 : Vector<float> = Vector [|2.0; 4.0|]
val sl7 : seq<Vector<float>>
val m18 : Matrix<float> = Matrix [[1.0; 2.0]
                                  [3.0; 4.0]]
val sl8 : seq<Vector<float>>
val m19 : Matrix<float> = Matrix [[1.0; 3.0]
                                  [2.0; 4.0]]
val m20 : Matrix<float> = Matrix [[1.0; 2.0]
                                  [3.0; 4.0]]
val m21 : Matrix<float> = Matrix [[1.0; 2.0]
                                  [3.0; 4.0]]
val m22 : Matrix<float> = Matrix [[1.0; 2.0]
                                  [3.0; 4.0]]
val m23 : Matrix<float> = Matrix [[1.0]]
val aa1 : float [] [] = [|[|1.0; 2.0|]; [|3.0; 4.0|]|]
val aa2 : float [,] = [[1.0; 1.0]
                       [1.0; 1.0]]
val aa3 : float [] = [|1.0; 2.0; 3.0; 4.0|]
val ma3 : Matrix<float> = Matrix [[1.0; 2.0]
                                  [3.0; 4.0]]
val vv1 : Vector<float> = Vector [|1.0; 2.0; 3.0; 4.0|]
val mv1 : Matrix<float> = Matrix [[1.0; 2.0]
                                  [3.0; 4.0]]"
(*** include-output: o6 ***)


(**
Mutating/Replacing Elements
------------------
*)

m1.[0, 0] <- 4. // Mutate an element
Matrix.replace (fun x -> x + 2.) m1 // Replace by mapping a function, mutating in place
Matrix.replacei (fun i j x -> x + float (i * j)) m1 // Replace in place, with index
Matrix.replace2 (fun x y -> x - y) m1 m2 // Replace m1 in place, using a function of m1 and m2
Matrix.replacei2 (fun i j x y -> x - y + float (i * j)) m1 m2 // Replace m1 in place, with index
Matrix.replaceWith m1 m2  // Replace elements of m1 with m2, mutating in place
Matrix.appendRow v m1     // Append a vector to a matrix as a new row
Matrix.appendCol v m1  // Append a vector to a matrix as a new column
Matrix.prependRow v m1    // Prepend a vector to a matrix as a new row
Matrix.prependCol v m1 // Prepend a vector to a matrix as a new column

(**
Mathematica and MATLAB Strings
------------------------------
You can generate string representations of matrices that you can copy and paste into Mathematica notebooks or MATLAB.   
*)

let s1 = m1.ToMathematicaString()
let s2 = m2.ToMatlabString()

(*** hide, define-output: o7 ***)
printf "val s1 : string = \"{{1.00, 2.00}, {3.00, 4.00}}\"
val s2 : string = \"[1.00 1.00; 1.00 1.00]\""
(*** include-output: o7 ***)

(**
Other Operations
---------------------   
The following are just a selection of other operations. Please refer to [API Reference](reference/index.html) for a full list of supported operations.
*)

let l1  = Matrix.rows m1 // Number of rows
let l2  = Matrix.cols m1 // Number of columns
let m24 = Matrix.map (fun x -> sin x) m1 // Map function to matrix

(*** hide, define-output: o8 ***)
printf "val l1 : int = 2
val l2 : int = 2
val m24 : Matrix<float> = Matrix [[0.8414709848; 0.8414709848]
                                  [0.8414709848; 0.8414709848]]"
(*** include-output: o8 ***)
