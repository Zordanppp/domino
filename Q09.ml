(* Questão 9 - Daniel Lopes Zordan Filho e  Leonardo Augusto Picanço Barreto *)
(*Para resolver a questão 9 foi necessario usar a solução quase igual a da 8, mudando apenas algumas funções a mais, que se segue abaixo*)
fun g([]) = []
    | g(x::xs) = 
          if x = nil then g(xs) 
                            else hd(x)::g(xs);
(*função que pega o primeiro parâmetro de cada pedra*)
fun h(nil) = nil
|h((x1 : int, x2 : int) :: xs)  = x1::h(xs);

(*função que recebe um estado e uma pedra, mapeia se pode jogar e retorna a pedra no lugar jogado*)
fun novoEstado((e1, e2, e3, e4: (int * int) list), (a: int, b: int)) = 
let

    val lista = e1 :: e2 :: e3 :: e4 :: [];
    val l = h(g(lista));

    (*função que compara se pode jogar uma pedra no ramo, e se puder, retorna o ramo com a pedra invertida como ponta *)
    fun novaPonta((x1: int, x2: int), x::xs, cont: int) = 
                                                  if x1 = x then ((x2, x1)::nil, cont) 
                                                            else if x2 = x then ((x1, x2)::nil, cont) 
                                                                           else novaPonta((x1, x2), xs, cont+1)
    | novaPonta(_, nil, cont) = (nil, cont);

    val teste = novaPonta((a, b), l, 1);

    val c = #2 teste;
    val p = #1 teste;

    (*função retorna um novo estado do jogo com a pedra jogada na posição correta*)
    fun returnNewState(x1, x2, x3, x4: (int * int) list, nil) = (x1, x2, x3, x4)
    | returnNewState(x1, x2, x3, x4, k) = 
                                   if(c = 1) then (hd(k)::x1, x2, x3, x4)
                                             else if(c = 2) then (x1, hd(k)::x2, x3, x4)
                                                            else if(c = 3) then (x1, x2, hd(k)::x3, x4)
                                                                           else (x1, x2, x3, hd(k)::x4);

    val r = returnNewState(e1, e2, e3, e4, p)
    val la = #1 r :: #2 r :: #3 r :: #4 r :: nil
in
    la
end;

(*função que soma as pontas dos ramos, se for carroça, soma os dois naipes*)
fun pegaSoma(nil) = 0  
       |pegaSoma((x1 : int, x2 : int) :: xs)  = 
            if(x1 = x2) then x1 + x2 + pegaSoma(xs) 
                        else x1 + pegaSoma(xs);
(*função que, usa as outras funções e detecta a pedra que marca mais pontos, e retorna essa*)
fun pedrasPont(e1, e2, e3, e4 : (int * int) list, listaPedras: (int * int) list) =
let

    (*função que pegaas pontas do resultado da função "g" de lista*)
    fun pegarPontas(nil) = nil
    | pegarPontas(lista) = g(lista);

    (*função que anota os pontos de cada pedra*)
    fun pontosMarcados(x1, x2, x3, x4, c) = pegaSoma(pegarPontas(novoEstado((x1, x2, x3, x4), c)))
    (*função que lista quais pedras pontuam ao serem jogadas*)
    fun listar(nil)    = nil
    | listar(cab::cau) = 
    if(pontosMarcados(e1, e2, e3, e4, cab) mod 5 = 0) then (pontosMarcados(e1, e2, e3, e4, cab), cab):: listar(cau) 
                                                                           else listar(cau)

in
    listar(listaPedras) 
end; 

(*Aqui é onde a solução da questão  9 começa, de fato.
Esta função receberá os pontos*)
fun nine((listao : (int * (int*int)) list)) = 
let
    (*função que forma uma lista com os valores das somas de cada pedra*)
    fun h(nil) = nil
    |h((x1 : int, (x2,x3) : (int*int)) :: xs)  = [x1]@(h(xs));

    (*função que mapeia uma lista de inteiros e retorna o maior valor*)
    fun maior(x, nil) = x
    | maior(x: int, a::b) = if (x > a) then maior(x, b) 
                          else maior(a, b);

    val r = h(listao);

    val tt = maior(hd(r), tl(r));

    (*função que compara o primeiro elemento da cabeça de  uma lista (int * (int*int)) com o primeiro da cauda, recurssivamente  *)
    fun chora(_,nil) = nil
    |chora(D,lis::liss : (int * (int*int)) list) =
        if (D = #1(lis)) then ([lis])
                             else chora(D,liss)
in
    chora(tt,listao)
end;