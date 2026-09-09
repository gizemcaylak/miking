include "map.mc"
include "ocaml/ast.mc"

let impl = lam arg : { expr : String, ty : use Ast in Type }.
  { expr = arg.expr, ty = arg.ty, libraries = ["owl"], cLibraries = [] }

let owlDenseMatrixGenericInplaceUnop = lam name. lam shape1. lam shape2.
  join [
    "(fun m n a b -> Owl_dense_matrix.Generic.",
    name,
    " ~out:(Bigarray.genarray_of_array2 (Bigarray.reshape_2 (Bigarray.genarray_of_array1 b) ",
    shape2,
    ")) (Bigarray.genarray_of_array2 (Bigarray.reshape_2 (Bigarray.genarray_of_array1 a) ",
    shape1,
    ")))"]

let owlDenseMatrixGenericInplaceUnopTy = tyarrows_ [
  tyint_, tyint_, otyopaque_, otyopaque_, otyunit_]

let owlDenseMatrixGenericInplaceBinop = lam name.
  join [
    "(fun m n a b c -> Owl_dense_matrix.Generic.",
    name,
    " ~out:(Bigarray.genarray_of_array2 (Bigarray.reshape_2 (Bigarray.genarray_of_array1 c) m n)) (Bigarray.genarray_of_array2 (Bigarray.reshape_2 (Bigarray.genarray_of_array1 a) m n)) (Bigarray.genarray_of_array2 (Bigarray.reshape_2 (Bigarray.genarray_of_array1 b) m n)))"]

let owlDenseMatrixGenericInplaceBinopTy = tyarrows_ [
  tyint_, tyint_, otyopaque_, otyopaque_, otyopaque_, otyunit_]

let matExtMap =
  use OCamlTypeAst in
  mapFromSeq cmpString [
    ("externalMatTranspose", [
      impl {
        expr = owlDenseMatrixGenericInplaceUnop "transpose_" "m n" "n m",
        ty = owlDenseMatrixGenericInplaceUnopTy
      }
    ]),
    ("externalMatElemExp", [
      impl {
        expr = owlDenseMatrixGenericInplaceUnop "exp_" "m n" "m n",
        ty = owlDenseMatrixGenericInplaceUnopTy
      }
    ]),
    ("externalMatElemLog", [
      impl {
        expr = owlDenseMatrixGenericInplaceUnop "log_" "m n" "m n",
        ty = owlDenseMatrixGenericInplaceUnopTy
      }
    ]),
    ("externalMatElemMul", [
      impl {
        expr = owlDenseMatrixGenericInplaceBinop "mul_",
        ty = owlDenseMatrixGenericInplaceBinopTy
      }
    ]),
    ("externalMatExp", [
      impl {
        expr = "(fun m n a -> Bigarray.reshape_1 (Owl_linalg_generic.expm (Bigarray.genarray_of_array2 (Bigarray.reshape_2 (Bigarray.genarray_of_array1 a) m n))) (m * n))",
        ty = tyarrows_ [tyint_,  tyint_, otyopaque_, otyopaque_]
      }
    ]),
    ("externalMsgMergeLogLike", [
      impl {
        expr = "(fun (n : int) (a : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) (b : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) (pa : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) (pb : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) (w : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) (o : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) -> let q00 = Bigarray.Array1.unsafe_get pa 0 in let q01 = Bigarray.Array1.unsafe_get pa 1 in let q02 = Bigarray.Array1.unsafe_get pa 2 in let q03 = Bigarray.Array1.unsafe_get pa 3 in let q10 = Bigarray.Array1.unsafe_get pa 4 in let q11 = Bigarray.Array1.unsafe_get pa 5 in let q12 = Bigarray.Array1.unsafe_get pa 6 in let q13 = Bigarray.Array1.unsafe_get pa 7 in let q20 = Bigarray.Array1.unsafe_get pa 8 in let q21 = Bigarray.Array1.unsafe_get pa 9 in let q22 = Bigarray.Array1.unsafe_get pa 10 in let q23 = Bigarray.Array1.unsafe_get pa 11 in let q30 = Bigarray.Array1.unsafe_get pa 12 in let q31 = Bigarray.Array1.unsafe_get pa 13 in let q32 = Bigarray.Array1.unsafe_get pa 14 in let q33 = Bigarray.Array1.unsafe_get pa 15 in let r00 = Bigarray.Array1.unsafe_get pb 0 in let r01 = Bigarray.Array1.unsafe_get pb 1 in let r02 = Bigarray.Array1.unsafe_get pb 2 in let r03 = Bigarray.Array1.unsafe_get pb 3 in let r10 = Bigarray.Array1.unsafe_get pb 4 in let r11 = Bigarray.Array1.unsafe_get pb 5 in let r12 = Bigarray.Array1.unsafe_get pb 6 in let r13 = Bigarray.Array1.unsafe_get pb 7 in let r20 = Bigarray.Array1.unsafe_get pb 8 in let r21 = Bigarray.Array1.unsafe_get pb 9 in let r22 = Bigarray.Array1.unsafe_get pb 10 in let r23 = Bigarray.Array1.unsafe_get pb 11 in let r30 = Bigarray.Array1.unsafe_get pb 12 in let r31 = Bigarray.Array1.unsafe_get pb 13 in let r32 = Bigarray.Array1.unsafe_get pb 14 in let r33 = Bigarray.Array1.unsafe_get pb 15 in let acc = ref 0.0 in for i = 0 to n - 1 do let k = i * 4 in let a0 = Bigarray.Array1.unsafe_get a k in let a1 = Bigarray.Array1.unsafe_get a (k+1) in let a2 = Bigarray.Array1.unsafe_get a (k+2) in let a3 = Bigarray.Array1.unsafe_get a (k+3) in let b0 = Bigarray.Array1.unsafe_get b k in let b1 = Bigarray.Array1.unsafe_get b (k+1) in let b2 = Bigarray.Array1.unsafe_get b (k+2) in let b3 = Bigarray.Array1.unsafe_get b (k+3) in let v0 = (a0 *. q00 +. a1 *. q10 +. a2 *. q20 +. a3 *. q30) *. (b0 *. r00 +. b1 *. r10 +. b2 *. r20 +. b3 *. r30) in Bigarray.Array1.unsafe_set o (k+0) v0; let v1 = (a0 *. q01 +. a1 *. q11 +. a2 *. q21 +. a3 *. q31) *. (b0 *. r01 +. b1 *. r11 +. b2 *. r21 +. b3 *. r31) in Bigarray.Array1.unsafe_set o (k+1) v1; let v2 = (a0 *. q02 +. a1 *. q12 +. a2 *. q22 +. a3 *. q32) *. (b0 *. r02 +. b1 *. r12 +. b2 *. r22 +. b3 *. r32) in Bigarray.Array1.unsafe_set o (k+2) v2; let v3 = (a0 *. q03 +. a1 *. q13 +. a2 *. q23 +. a3 *. q33) *. (b0 *. r03 +. b1 *. r13 +. b2 *. r23 +. b3 *. r33) in Bigarray.Array1.unsafe_set o (k+3) v3; acc := !acc +. Bigarray.Array1.unsafe_get w i *. Float.log (0.25 *. (v0 +. v1 +. v2 +. v3)) done; !acc)",
        ty = tyarrows_ [tyint_, otyopaque_, otyopaque_, otyopaque_, otyopaque_,
                        otyopaque_, otyopaque_, tyfloat_]
      }
    ])
  ]
