Questão 3 - Daniel Lopes Zordan Filho

(* A Função "validar" resgata as pontas das pedras de uma lista, soma e confere se o resultado é multiplo de 5. Se for, então a pontuação é válida, se não, é "inválida" *)
 fun validar(L) :(int * string) =
 let
 	(* A função "pegaSoma" resgata o primeiro naipe de cada pedra e os soma*)
       fun pegaSoma(nil) = 0  
       |pegaSoma((x1 : int, x2 : int) :: xs)  = x1 + pegaSoma(xs);

       (*A função "resposta" confere se o resultado da função "pegaSoma" é múltiplo de 5 e retorna o resultado com a classificação "valido/invalido" *)
       fun resposta(x) = if x mod 5 = 0 then (x, "vale")
                                        else (x, "nao vale");
 in
       resposta(pegaSoma(L))
 end;

(*A função "validarMesa" recebe um estado de jogo 
e retorna um par dizendo se os pontos são válidos *)

fun validarMesa((e1, e2, e3, e4 :  (int * int) list)):(int * string) = 
let
	(*A função "mapLista" mapeia um estado de jogo para uma lista com as pontas de cada ramo *)
	fun mapLista([]) = []
	| mapLista(x::xs) = if x = nil then [(6,6)]@mapLista(xs) 
								   else hd(x)::mapLista(xs);

	val lista = e1 :: e2 :: e3 :: e4 :: [];

in
	validar(mapLista(lista))
end;