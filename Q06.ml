Questão 6 - Daniel Lopes Zordan Filho

(*A função "mapNaipeList" mapeia um naipe e uma lista de pedras e retorna "true" se o naipe estiver em alguma pedra, ou "false" se não estiver *)
fun soma(_,nil) = false
       |soma( Y : int, (x1,x2)::xs)  = 
         if (Y = x1) orelse (Y = x2) then true 
         							 else soma(Y,xs);