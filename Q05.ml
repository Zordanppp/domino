Questão 5 - Leonardo Augusto Picanço Barreto

(*A função "novoEstado" mapeia um estado do jogo e uma pedra, joga a pedra onde puder, no estado, e retorna o novo estado *)

fun novoEstado((e1, e2, e3, e4: (int * int) list), (a: int, b: int)) = 
let
     (*A função "mapLista" mapeia um estado de jogo para uma lista com as pontas de cada ramo 
    e se um ramo for nil então adiciona [(6,6)] nesse ramo*)
    fun mapLista([]) = []
    | mapLista(x::xs) = if x = nil then [(6,6)]@mapLista(xs)  
                                   else hd(x)::mapLista(xs);

    (* A fução "resgata1" pega o primeiro naipe de lista de pares *)
    fun resgata1(nil) = nil
    |resgata1((x1 : int, x2 : int) :: xs)  = x1::resgata1(xs);

   val lista = e1 :: e2 :: e3 :: e4 :: [];  (*Transforma os quatro ramos em uma lista*)

    val l = resgata1(mapLista(lista)); (*Armazena o resultado das funções "resgata1" e "mapLista" na variavel "l" *)

    (*a função "novaPonta" inverte a pedra e retorna o ramo em que deve ser colocada*)
    fun novaPonta((x1: int, x2: int), x::xs, cont: int) = 
        if x1 = x then ((x2, x1)::nil, cont) 
                  else if x2 = x then ((x1, x2)::nil, cont) 
                           else novaPonta((x1, x2), xs, cont+1)
    | novaPonta(_, nil, cont) = (nil, cont);

    val teste = novaPonta((a, b), l, 1);

    val c = #2 teste;
    val p = #1 teste;

    (*A função "returnNewState" mapeia os quatro ramos e substituio pela a pedra onde foi indicada pela função "novaPonta"*)
    fun returnNewState(x1, x2, x3, x4: (int * int) list, nil) = (x1, x2, x3, x4)
    | returnNewState(x1, x2, x3, x4, k) = 
        if(c = 1) then (hd(k)::x1, x2, x3, x4)
                  else if(c = 2) then (x1, hd(k)::x2, x3, x4)
                                 else if(c = 3) then (x1, x2, hd(k)::x3, x4)
                                                else (x1, x2, x3, hd(k)::x4)
in
    returnNewState(e1, e2, e3, e4, p)    
end;
