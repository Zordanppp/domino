fun somar(nil) = 0
	| somar(x::xs) = x + somar(xs);

fun podeJogar((e1, e2, e3, e4 : (int * int) list), lista_pedras: (int * int) list) = 
let
    fun g([]) = []
	| g(x::xs) = if x = nil then [(6,6)]@g(xs) else hd(x)::g(xs);

	val lista = e1 :: e2 :: e3 :: e4 :: [];

	fun soma(_,nil) = false
    | soma(Y as (z,zs) : int*int, (x1,x2)::xs)  = 
         if (z = x1) orelse (zs = x1) then true  
         	else soma(Y,xs);

	fun comparar((c, d)::xs: (int * int) list) = 
		if (soma((c, d), g(lista))) then (c, d) :: comparar(xs) 
		else comparar(xs)
	| comparar(nil) = nil

in
	comparar(lista_pedras)
end;

fun g([]) = []
	| g(x::xs) = if x = nil then g(xs) else hd(x)::g(xs);

fun h(nil) = nil
|h((x1 : int, x2 : int) :: xs)  = x1::h(xs);

fun novoEstado((e1, e2, e3, e4: (int * int) list), (a: int, b: int)) = 
let

	val lista = e1 :: e2 :: e3 :: e4 :: [];

	val l = h(g(lista));

	fun novaPonta((x1: int, x2: int), x::xs, cont: int) = 
		if x1 = x then ((x2, x1)::nil, cont) 
		else if x2 = x then ((x1, x2)::nil, cont) 
		else novaPonta((x1, x2), xs, cont+1)
	| novaPonta(_, nil, cont) = (nil, cont);

	val teste = novaPonta((a, b), l, 1);

	val c = #2 teste;
	val p = #1 teste;

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

fun pedrasPont(e1, e2, e3, e4 : (int * int) list, listaPedras: (int * int) list) =
let
	val possiveis = podeJogar((e1, e2, e3, e4), listaPedras);

	fun pegarPontas(nil) = nil
	| pegarPontas(lista) = somar(h(g(lista)));

	fun listar(nil) = nil
	| listar(cab::cau) = 
		if(pegarPontas(novoEstado((e1, e2, e3, e4), cab)) < pegarPontas(novoEstado((e1, e2, e3, e4), hd(cau)))) mod 5 = 0) 
		then cab :: listar(cau) 
		else listar(cau)

in
	listar(possiveis)
end; 

(* Caso de teste *)
pedrasPont([(6,6)],[(2,1)], [(4,5),(5,6)],[(3,6)], [(6,1), (2,2)]); 