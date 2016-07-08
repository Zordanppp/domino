Questão 4 - Leonardo Augusto Picanço Barreto e Daniel Lopes Zordan Filho

(*A função "podeJogar" mapeia uma pedra e um estado do jogo, em quatro ramos, para "true" se for possivel jogar a pedra e "false" se não*)
fun podeJogar((a: int, b: int),(e1, e2, e3, e4 : (int * int) list)) = 
let
   (*A função "mapLista" mapeia um estado de jogo para uma lista com as pontas de cada ramo 
   	e se um ramo for nil então adiciona [(6,6)] nesse ramo*)
	fun mapLista([]) = []
	| mapLista(x::xs) = if x = nil then [(6,6)]@mapLista(xs)  
								   else hd(x)::mapLista(xs);

	val lista = e1 :: e2 :: e3 :: e4 :: []; (*Transforma os quatro ramos em uma lista*)

(*A função somaParList mapeia uma pedra e uma lista de pares
 para "true" se a pedra puder ser jogada e "false" se não puder *)	
fun somaParList(_,nil) = false
       |somaParList(Y as (z,zs) : int*int, (x1,x2)::xs)  = 
         if (z = x1) orelse (zs = x1) then true  
   						            	else somaParList(Y,xs)

in
	somaParList((a,b), g(lista))
end;




