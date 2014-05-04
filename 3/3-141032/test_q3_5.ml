#use "q3_5.ml";;

module IntRig =
  struct
    type t = int
    let zero = 0
    let one = 1
    let add a b = a + b
    let mul a b = a * b
  end

module BoolRig =
  struct
    type t = bool
    let zero = false
    let one = true
    let add a b = a || b
    let mul a b = a && b
  end

module IntMatrix = Matrix(IntRig);;
module IntVector = Vector(IntRig);;

let a = IntMatrix.list2mat [[1; 2]; [3; 4]; [5; 6]];;
let b = IntMatrix.list2mat [[1; 3; 5]; [2; 4; 6]];;
let v = IntVector.list2vec [1; 2];;
let u = IntVector.list2vec [-1; 4];;
IntVector.vec2list (IntVector.add u v);;
IntVector.vec2list (IntVector.scale 2 v);;
IntVector.vec2list (IntVector.matmul a v);;
IntMatrix.mat2list (IntMatrix.scale 2 a);;
IntMatrix.mat2list (IntMatrix.add a (IntMatrix.trans b));;
IntMatrix.mat2list (IntMatrix.mul a b);;
