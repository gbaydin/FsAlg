(*** hide ***)
#r "../../src/FsAlg/bin/Debug/FsAlg.dll"

(**
FsAlg: Generic Linear Algebra Library
=====================================

FsAlg is a [linear algebra](http://en.wikipedia.org/wiki/Linear_algebra) library that supports [generic types](http://en.wikipedia.org/wiki/Generic_programming). It is implemented in the F# language.

The library provides generic Vector and Matrix types that support most of the commonly used linear algebra operations, including matrix–vector operations, matrix inverse, determinants, eigenvalues, LU and QR decompositions. Its intended use is to enable writing generic linear algebra code with [custom numeric types](http://tomasp.net/blog/fsharp-custom-numeric.aspx/). It can also be used as a lightweight library for prototyping and scripting with [primitive floating point types](https://msdn.microsoft.com/en-us/library/dd233210.aspx).

FsAlg is developed by [Atılım Güneş Baydin](http://www.cs.nuim.ie/~gunes/) and [Barak A. Pearlmutter](http://bcl.hamilton.ie/~barak/) as part of their work at the [Brain and Computation Lab](http://www.bcl.hamilton.ie/), Hamilton Institute, National University of Ireland Maynooth.

How to Get
----------

You can install the library via NuGet. You can also download the source code or the binaries of the latest release <a href="https://github.com/gbaydin/FsAlg/releases">on GitHub</a>.

<div class="row">
    <div class="span1"></div>
    <div class="span6">
    <div class="well well-small" id="nuget">
        The FsAlg library <a href="https://www.nuget.org/packages/FsAlg">is available on NuGet</a>. To install, run the following command in the <a href="http://docs.nuget.org/docs/start-here/using-the-package-manager-console">Package Manager Console</a>:
        <pre>PM> Install-Package FsAlg</pre>
    </div>
    </div>
    <div class="span1"></div>
</div>

Quick Usage Example
-------------------
*)

open FsAlg.Generic

let A = matrix [[3.; 1.]; [2.; -1.]]
let b = vector [5.; 0.]

// Solve a matrix equation
let x = Matrix.solve A b

// Find inverse of A
let Ainv = Matrix.inverse A

// Initialize a vector
let w = Vector.init 2 (fun i -> float (i * i))

// Map a function to a vector
let y = Vector.map (fun x -> sin (exp x)) w

// Matrix-vector operations
let z = A * (x - y)