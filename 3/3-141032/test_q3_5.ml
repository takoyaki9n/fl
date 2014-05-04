#use "q3_5.ml";;

module IntRing =
  struct
    type t = int
    let zero = 0
    let one = 1
    let add a b = a + b
    let mul a b = a * b
  end

module BoolRing =
  struct
    type t = bool
    let zero = false
    let one = true
    let add a b = a || b
    let mul a b = a && b
  end

module IntMatrix = Matrix(IntRing);;
module IntVector = Vector(IntRing);;

let a = IntMatrix.list2mat [[1; 2]; [3; 4]; [5; 6]];;
let b = IntMatrix.list2mat [[-1; 3]; [1; 2]];;
let v = IntVector.list2vec [1; 2];;
IntVector.vec2list v;;
IntVector.matmul a v;;
IntMatrix.mul a b;;
IntMatrix.trans b;;
IntMatrix.mul a v;;
IntMatrix.scale 2 v;;
