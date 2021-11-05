module Matrix :
  sig
    exception DifferentMatrixTypes
    exception NotMultiplicableMatrices

    (* public build functions *)
    val zeroes : int -> int -> int array array
    val identity : int -> int array array
    val init : int -> int array array

    (* public operations *)
    val sum : int array array -> int array array -> int array array
    val mult : int array array -> int array array -> int array array
    val transpose : 'a array array -> 'a array array
  end =
  struct

    exception DifferentMatrixTypes
    exception NotMultiplicableMatrices

    type matrix_dimension = int * int

    (* builds a matrix of the given size filled with zeroes *)
    let zeroes n m = Array.make_matrix n m 0;;

    (* builds an identity matrix of the given size *)

    let rec build_identity_row dim row_n step = match step with
      _ when step == dim -> [| |]
      | _ -> 
        if step == row_n then Array.concat [ [| 1 |]; build_identity_row dim row_n (step+1) ] 
        else Array.concat [ [| 0 |]; build_identity_row dim row_n (step+1) ];;

    let rec make_identity dim step = match step with
      _ when step == dim -> [| |]
      | _ -> Array.concat [ [| build_identity_row dim step 0 |]; make_identity dim (step+1) ];;

    let identity n = make_identity n 0;;

    (* builds a square matrix of the given size filled the first nxn numbers *)

    let rec build_init_row dim row_n step = match step with 
      _ when step == dim -> [| |]
      | _ -> Array.concat [ [| (dim*row_n)+step |]; build_init_row dim row_n (step+1)];;

    let rec make_init dim step = match step with
      _ when step == dim -> [| |]
      | _ -> Array.concat [ [| build_init_row dim step 0 |]; make_init dim (step+1) ];;

    let init n = make_init n 0;;

    (* sums two matrices *)

    let rec row_sum row1 row2 length step =
      match step with 
        _ when length == step -> [| |]
        | _ -> Array.concat [ [| row1.(step) + row2.(step) |]; row_sum row1 row2 length (step+1) ];;

    let rec nested_sum matrix1 matrix2 dim step =
      match step with
        _ when step == (fst dim) -> [| |]
        | _ -> Array.concat [ [| row_sum matrix1.(step) matrix2.(step) (snd dim) 0 |]; nested_sum matrix1 matrix2 dim (step+1) ];;

    let sum matrix1 matrix2 = 
      match matrix1, matrix2 with
        [| |], [| |] -> [| |]
        | [| |], _ -> raise DifferentMatrixTypes
        | _, [| |] -> raise DifferentMatrixTypes
        | _, _ -> 
          let m1_dim : matrix_dimension = ((Array.length matrix1), (Array.length matrix1.(0)))
            and m2_dim : matrix_dimension = ((Array.length matrix2), (Array.length matrix2.(0)))
          in 
            if (fst m1_dim) != (fst m2_dim) || (snd m1_dim) != (snd m2_dim) then raise DifferentMatrixTypes 
            else nested_sum matrix1 matrix2 m1_dim 0;;

    (* extracts a column from the given matrix *)
    let rec extract_column matrix col_n rows step = 
      match step with
        _ when step == rows -> [| |]
        | _ -> Array.concat [ [| matrix.(step).(col_n) |]; extract_column matrix col_n rows (step+1)];;

    (* multiplies two matrices *)

    let rec calc_mult_element i j matrix1 matrix2 length step =
      match step with 
        _ when step == length -> 0
        | _ -> matrix1.(i).(step) * matrix2.(step).(j) + calc_mult_element i j matrix1 matrix2 length (step+1);;

    let rec calc_mult_row row_n matrix1 matrix2 dim1 dim2 step =
      match step with
        _ when step == (snd dim2) -> [| |]
        | _ -> Array.concat [ 
          [| calc_mult_element row_n step matrix1 matrix2 (snd dim1) 0 |];
          calc_mult_row row_n matrix1 matrix2 dim1 dim2 (step+1) 
        ];;

    let rec nested_mult matrix1 matrix2 dim1 dim2 step = 
      match step with
        _ when step == (snd dim1) -> [| |]
        | _ -> Array.concat [ 
          [| calc_mult_row step matrix1 matrix2 dim1 dim2 0 |]; 
          nested_mult matrix1 matrix2 dim1 dim2 (step+1) 
        ];;

    let mult matrix1 matrix2 =
      match matrix1, matrix2 with
        [| |], [| |] -> [| |]
        | [| |], _ -> raise NotMultiplicableMatrices
        | _, [| |] -> raise NotMultiplicableMatrices
        | _, _ -> 
          let m1_dim : matrix_dimension = ((Array.length matrix1), (Array.length matrix1.(0)))
            and m2_dim : matrix_dimension = ((Array.length matrix2), (Array.length matrix2.(0)))
          in 
            if (snd m1_dim) != (fst m2_dim) then raise NotMultiplicableMatrices 
            else nested_mult matrix1 matrix2 m1_dim m2_dim 0;;

    (* transposes the given matrix *)

    let rec nested_transpose matrix dim step = 
      match step with
        _ when step == (snd dim) -> [| |]
        | _ -> Array.concat [ [| extract_column matrix step (fst dim) 0 |]; nested_transpose matrix dim (step+1)];;

    let transpose matrix = 
      match matrix with
        [| |] -> [| |]
        | _ -> nested_transpose matrix (Array.length matrix, Array.length matrix.(0)) 0;;

  end;;