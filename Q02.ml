 (* Questão 2 - Daniel Lopes Zordan Filho *)

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
