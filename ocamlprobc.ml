type color = W | B
type image = L of color | N of image * image * image * image

(* Função para dividir a matriz em 4 partes *)
let dividirmatriz matriz =
  let size = Array.length matriz in
  let half_size = size / 2 in
  let nw = Array.sub matriz 0 half_size |> Array.map (fun row -> Array.sub row 0 half_size) in
  let ne = Array.sub matriz 0 half_size |> Array.map (fun row -> Array.sub row half_size half_size) in
  let sw = Array.sub matriz half_size half_size |> Array.map (fun row -> Array.sub row 0 half_size) in
  let se = Array.sub matriz half_size half_size |> Array.map (fun row -> Array.sub row half_size half_size) in
  (nw, ne, sw, se);;

(* Função para criar o quadtree *)
let rec definir_quadtree matrix =
  let is_single_color matrix =
    let color = matrix.(0).(0) in
    let size = Array.length matrix in
    let rec check_color i j =
      if i >= size then true
      else if j >= size then check_color (i + 1) 0
      else matrix.(i).(j) = color && check_color i (j + 1)
    in
    check_color 0 0
  in
  if is_single_color matrix then
    let color = if matrix.(0).(0) = 0 then W else B in
    L color
  else
    let (nw, ne, sw, se) = dividirmatriz matrix in
    let nw_node = definir_quadtree nw in
    let ne_node = definir_quadtree ne in
    let sw_node = definir_quadtree sw in
    let se_node = definir_quadtree se in
    N (nw_node, ne_node, sw_node, se_node);;  (* Para recursividade criar nodo com 4 childs*)

(*Funcao para mostrar matriz*)
let mostrar_matriz matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      print_int matrix.(i).(j);
      print_string " "
    done;
    print_newline ()
  done;;


(* Função para contar o número de leafs e nodes *)
let rec contar_quadtree quadtree =
  match quadtree with
  | L _ -> (1, 0, 1, 1)  (* Leaf node with branch size 1 *)
  | N (nw, ne, sw, se) ->
    let count_nw_leaves, count_nw_nodes, min_nw_branch, max_nw_branch = contar_quadtree nw in
    let count_ne_leaves, count_ne_nodes, min_ne_branch, max_ne_branch = contar_quadtree ne in
    let count_sw_leaves, count_sw_nodes, min_sw_branch, max_sw_branch = contar_quadtree sw in
    let count_se_leaves, count_se_nodes, min_se_branch, max_se_branch = contar_quadtree se in
    let leaf_count = count_nw_leaves + count_ne_leaves + count_sw_leaves + count_se_leaves in
    let node_count = 1 + count_nw_nodes + count_ne_nodes + count_sw_nodes + count_se_nodes in
    let min_branch_size = min min_nw_branch (min min_ne_branch (min min_sw_branch min_se_branch)) in
    let max_branch_size = max max_nw_branch (max max_ne_branch (max max_sw_branch max_se_branch)) in
    (leaf_count, node_count, min_branch_size + 1, max_branch_size + 1);;

(*Rodar matriz 90 graus para a esquerda*)
let rodar90 matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let rotated_matrix = Array.make_matrix cols rows 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      rotated_matrix.(cols - j - 1).(i) <- matrix.(i).(j)
    done
  done;
  rotated_matrix;;

  (*Inverter 1 - 0 e 0-1*)
let invertermatriz matrix =
  let inverted = Array.map (Array.map (fun x -> if x = 0 then 1 else 0)) matrix in
  inverted;;

(*Rodar 180 graus para a direita*)
let rodar180 matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let rotated_matrix = Array.make_matrix rows cols 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      rotated_matrix.(i).(j) <- matrix.(rows - 1 - i).(cols - 1 - j)
    done;
  done;
  rotated_matrix;;


(* Ler o nome do ficheiro da entrada do usuário *)
let ficheiroinput = read_line() in
let nomedficheiro = ficheiroinput ^ ".ppm" in


(* Abrir o ficheiro *)
let handle_ficheiro = open_in nomedficheiro in

(* Saltar a primeira linha *)
let _ = input_line handle_ficheiro in

(* Ler o tamanho da matriz *)
let linha = input_line handle_ficheiro in
let linha_tratada = String.trim linha in
let (tamanho, _) = Scanf.sscanf linha_tratada "%d %d" (fun w h -> (w, h)) in

(* Criar a matriz *)
let matriz = Array.make_matrix tamanho tamanho 0 in

(* Ler os valores da matriz do ficheiro *)
for i = 0 to tamanho - 1 do
  let linha = input_line handle_ficheiro in
  let linha_tratada = String.trim linha in
  let scanner = Scanf.Scanning.from_string linha_tratada in
  for j = 0 to tamanho - 1 do
    matriz.(i).(j) <- Scanf.bscanf scanner "%d " (fun value -> value)
  done;
done;

(* Fechar o ficheiro *)
close_in handle_ficheiro;


(* Criar o quadtree *)
let quadtree = definir_quadtree matriz in

(* Imprimir o quadtree *)
let folhas, nodos, min_ramo, max_ramo = contar_quadtree quadtree in

(* Imprimir os contadores *)
print_endline (string_of_int folhas ^ " " ^ string_of_int nodos);
print_endline (string_of_int (min_ramo - 1) ^ " " ^ string_of_int (max_ramo - 1));

let matriz90 = rodar90 matriz in
  print_endline("P1");
  print_endline (string_of_int tamanho ^ " " ^ string_of_int tamanho);
  mostrar_matriz matriz90;

let invertida = invertermatriz matriz90 in
  print_endline("P1");
  print_endline (string_of_int tamanho ^ " " ^ string_of_int tamanho);
  mostrar_matriz invertida;

let matriz180 = rodar180 invertida in
  print_endline("P1");
  print_endline (string_of_int tamanho ^ " " ^ string_of_int tamanho);
  mostrar_matriz matriz180;;
