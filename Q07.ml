(* Questão 7 - Leonardo Augusto Picanço Barreto e Daniel Lopes Zordan Filho *)

(*A função "podeJogar" mapeia uma lista e um estado, a uma lista de pedras que podem ser jogadas naquele estado*)
fun podeJogar((e1, e2, e3, e4 : (int * int) list), lista_pedras: (int * int) list) = 
let
     (*A função "mapLista" mapeia um estado de jogo para uma lista com as pontas de cada ramo 
    e se um ramo for nil então adiciona [(6,6)] nesse ramo*)
    fun mapLista([]) = []
    | mapLista(x::xs) = if x = nil then [(6,6)]@mapLista(xs)  
                                   else hd(x)::mapLista(xs);

   val lista = e1 :: e2 :: e3 :: e4 :: [];  (*Transforma os quatro ramos em uma lista*)

(*A função somaParList mapeia uma pedra e uma lista de pares
 para "true" se a pedra puder ser jogada e "false" se não puder *)  
fun somaParList(_,nil) = false
       |somaParList(Y as (z,zs) : int*int, (x1,x2)::xs)  = 
         if (z = x1) orelse (zs = x1) then true  
                                        else somaParList(Y,xs)

(*A função "comparar" recebe uma lista de pedra e o resultado da  função "mapLista"
Compara qual pedra pode ser jogada e depois retorna *)                                        
fun comparar((c, d)::xs: (int * int) list) = 
        if (somaParList((c, d), mapLista(lista))) then (c, d) :: comparar(xs) 
                                                  else comparar(xs)
    | comparar(nil) = nil

in
    comparar(lista_pedras)
end;

podeJogar(([(6,6)], [(4,6),(6,6)], [(3,6),(6,6)], [(2,6),(6,6)]), [(1, 6), (6,0), (3,2), (0,0), (5, 1)]);
