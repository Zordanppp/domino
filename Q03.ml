(* Questão 3 - Daniel Lopes Zordan Filho *)

type Pedra = (int * int)
type Estado = Pedra list list
(* A Função "validar" resgata as pontas das pedras de uma lista, soma e confere se o resultado é multiplo de 5. Se for, então a pontuação é válida, se não, é "inválida" *)
 fun validar(L) :(int * string) =
 let
 	(* A função "pegaSoma" resgata o primeiro naipe de cada pedra e os soma*)
       fun pegaSoma(nil) = 0  
       |pegaSoma((x1 : int, x2 : int) :: xs)  = if(x1 = x2) then x1 + x2 + pegaSoma(xs) else x1 + pegaSoma(xs);

       (*A função "resposta" confere se o resultado da função "pegaSoma" é múltiplo de 5 e retorna o resultado com a classificação "valido/invalido" *)
       fun resposta(x) = if x mod 5 = 0 then (x, "vale")
                                        else (x, "nao vale");
 in
       resposta(pegaSoma(L))
 end;

(*A função "validarMesa" recebe um estado de jogo 
e retorna um par dizendo se os pontos são válidos *)

fun pertence(_, nil) = false
| pertence(x: Pedra list, g::h : Pedra list list) = if (x = g) then true 
					  else pertence(x, h);

fun count(el, nil) = 0 
| count(el, x::xs) = if(el = x) then 1 + count(el, xs) else count(el, xs);

fun validarMesa((e1, e2, e3, e4 :  Pedra list)) = 
let
	(*A função "mapLista" mapeia um estado de jogo para uma lista com as pontas de cada ramo *)
	val lista: Estado = [e1, e2, e3, e4];

	fun mapLista(nil) = nil
	| mapLista(x::xs) = [hd(x)]::mapLista(xs) 

	val pontas = mapLista(lista)

	fun carrocaSena(nil) = 0
	| carrocaSena(pontas) = if(hd(pontas) = [(6,6)]) then 1 + carrocaSena(tl(pontas)) else carrocaSena(tl(pontas)) 

	fun nocarrocaSena(nil) = nil
	| nocarrocaSena(pontas) = if(hd(pontas) = [(6,6)]) then nocarrocaSena(tl(pontas)) else hd(pontas) @ nocarrocaSena(tl(pontas))

	fun pontasValidas(nil) = nil
	| pontasValidas(lista) = 
		if(carrocaSena(lista) = 4) then [(6,6)]
		else if (carrocaSena(lista) = 3) then (6,6) :: nocarrocaSena(pontas)
		else nocarrocaSena(pontas)
in
	(* mapLista(lista) *)
	(* lista *)
	(* tl(hd(lista))  *)
	(* nocarrocaSena(pontas) *)
	(* pontasValidas(pontas) *)
	validar(pontasValidas(pontas))
end;

(* count([(6,6)], [[(6,6)], [(4,6)], [(1,6)], [(6,6)]]); *)

validarMesa([(1, 6),(6, 6)], [(4,4), (4,6),  (6,6)], [(1, 1), (1, 5), (5, 6), (6,6)], [(6,6)]);

(* ([(6, 6)], [(4,6), (6,6)], [(1,6), (6,6)], [(6,6)]) *)