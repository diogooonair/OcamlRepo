(* Perguntar ao utilizador o tamanho do pao(n) , quantidade de entradas na tabela de precos(m) *)
print_string "Insere o tamanho do quatre quarts:\n";
flush stdout; (* flush the output buffer *)
let n = read_int () in
print_string "Insere a quantidade de entradas na tabela de precos:\n";
flush stdout; (* flush para a mensagem ser enviada *)
let m = read_int () in

(* criar uma array com o tamanho m, para a tabela de preços *)
let tabela = Array.make m (0, 0) in

(* Pedir e definir os precos na array *)
for i = 0 to Array.length tabela - 1 do
  print_string "Insere o tamanho e o preco no formato (t p):\n";
  flush stdout; (* flush para a mensagem ser enviada *)
  let (c, d) = Scanf.scanf " %d %d" (fun a b -> (a, b)) in
  tabela.(i) <- (c, d)
done;

for i = 0 to Array.length tabela - 1 do
  let (c, d) = tabela.(i) in
  Printf.printf "Entrada %d: %d %d\n" i c d;
  flush stdout; (* flush para a mensagem ser enviada *)
done;


let dp = Array.make (n+1) 0 in
  for i = 1 to n do
    let max_profit = ref 0 in
    for j = 1 to m do
      let (size_j, price_j) = tabela.(j-1) in
      if size_j <= i then
        max_profit := max !max_profit (dp.(i - size_j) + price_j)
    done;
    dp.(i) <- !max_profit
  done;



Printf.printf "O lucro maximo e %i\n" dp.(n);;