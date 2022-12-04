(*inicjalizacja typu drzewa leniwego, gdzie zadna wartosc nie jest obliczana*)
type 'a lmbt = LEmpty | LNode of (unit -> 'a) * (unit -> 'a lmbt) * (unit -> 'a lmbt) | Node of 'a * (unit -> 'a lmbt) * (unit -> 'a lmbt) | NodeL of 'a * 'a lmbt * (unit -> 'a lmbt) | NodeR of 'a * (unit -> 'a lmbt) * 'a lmbt | NodeC of 'a * 'a lmbt * 'a lmbt;;


(*typ listy z kierunkiem*)
type direction = Left | Right;;


(*typ listy leniwej*)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;


(*metoda konwertacji listy zwyklej do listy leniwej*)
let rec toLazyList xs =
    match xs with
          [] -> LNil
        | h::t -> LCons (h, function () -> toLazyList t)
;;



(*metoda generowania drzewa z kolejnoscia odpowiedajaca nastepnym liczbam calkowitym*)
let rec ltfrom k = LNode ((function() -> k), (function()-> (ltfrom((k*2)+1))), (function() ->  (ltfrom((k*2)+2))));;




(*ONLY TESTS*)
(*metoda do pobrania n poziomow drzewa leniwego; DO TESTOWANIA DRZEWA*)
let rec lmbtgetlevels (n, lt) =
    match (n, lt) with
          (0, _)                                 ->  []
        | (_, LEmpty)                            ->  []
        | (n, LNode( v,  lt,  rt))   ->  v()::lmbtgetlevels(n-1, lt())@lmbtgetlevels(n-1, rt())
        | (n, Node(v,  lt,  rt))         ->  v::lmbtgetlevels(n-1, lt())@lmbtgetlevels(n-1, rt())
        | (n, NodeL(v, lt,  rt))             ->  v::lmbtgetlevels(n-1, lt)@lmbtgetlevels(n-1, rt())
        | (n, NodeR(v,  lt, rt))             ->  v::lmbtgetlevels(n-1, lt())@lmbtgetlevels(n-1, rt)
        | (n, NodeC(v, lt, rt))                  ->  v::lmbtgetlevels(n-1, lt)@lmbtgetlevels(n-1, rt)
;;
(*ONLY TESTS*)




(*Metoda aktualizacji odwiedzonego drzewa*)
let rec actTree (tree, direction) =
    match (tree, direction) with
        (LNode(v, lt, rt), LNil)                     -> Node (v(), lt, rt)
        | (NodeC(v, lt, rt), LNil)                        -> NodeC(v, lt, rt)
        | (NodeL(v, lt, rt), LNil)                        -> NodeL(v, lt, rt)
        | (NodeR(v, lt, rt), LNil)                        -> NodeR(v, lt, rt)
        | (Node (v, lt, rt), LNil)                        -> Node(v, lt, rt)
        | (LEmpty, _) -> LEmpty
        | (LNode( v,  lt, rt), LCons(Left, dt))        -> NodeL(v(), (actTree(lt(), dt())),  rt)
        | (LNode( v,  lt,  rt), LCons(Right, dt))      -> NodeR(v(),  lt, (actTree(rt(), dt())))
        | (Node(v, lt, rt), LCons(Left, dt))               -> NodeL (v, actTree(lt(), dt()), rt)
        | (Node(v, lt,  rt), LCons(Right, dt))             -> NodeR (v, lt, actTree(rt(), dt()))
        | (NodeL(v, lt, rt), LCons(Left, dt))                  -> NodeL(v, actTree(lt, dt()), rt)
        | (NodeL(v, lt,  rt), LCons(Right, dt))            -> NodeC(v, lt, actTree(rt(), dt()))
        | (NodeR(v,  lt, rt), LCons(Left, dt))             -> NodeC(v, actTree(lt(), dt()), rt)
        | (NodeR(v, lt, rt), LCons(Right, dt))                 -> NodeR(v, lt, actTree(rt, dt()))
        | (NodeC(v, lt, rt), LCons(Left, dt))                  -> NodeC(v, actTree(lt, dt()), rt)
        | (NodeC(v, lt, rt), LCons(Right, dt))                 -> NodeC(v, lt, actTree(rt, dt()))
;;



(*Metoda pobrania elemntow na sciezce wskazanej w liscie*)
let rec take (tree, direction) =
    match (tree, direction) with
          (LEmpty, _)                               -> []
        | (LNode( v, _, _), LNil)                 -> [v()]
        | (Node(v, _, _), LNil)                       -> [v]
        | (NodeL(v, _, _), LNil)                      -> [v]
        | (NodeR(v, _, _), LNil)                      -> [v]
        | (NodeC(v, _, _), LNil)                      -> [v]
        | (LNode(v, _, rt), LCons(Right, dt))    -> v()::take(rt(), dt())
        | (LNode( v,  lt, _), LCons(Left, dt))     -> v()::take(lt(), dt())
        | (Node(v, _,  rt), LCons(Right, dt))          -> v::take(rt(), dt())
        | (Node(v,  lt, _), LCons(Left, dt))           -> v::take(lt(), dt())
        | (NodeL(v, _,  rt), LCons(Right, dt))         -> v::take(rt(), dt())
        | (NodeL(v, lt, _), LCons(Left, dt))               -> v::take(lt, dt())
        | (NodeR(v, _, rt), LCons(Right, dt))              -> v::take(rt, dt())
        | (NodeR(v,  lt, _), LCons(Left, dt))          -> v::take(lt(), dt())
        | (NodeC(v, _, rt), LCons(Right, dt))              -> v::take(rt, dt())
        | (NodeC(v, lt, _), LCons(Left, dt))               -> v::take(lt, dt())
;;






(*

let rec lttakeV2(tree, direction) =
    match (tree, direction) with
          (LEmpty, _) -> (LEmpty, [])
        | (LNode(v, lt, rt), LNil) -> (Node(v(), lt, rt), [v()])
        | (LNode(v, lt, _), LCons(Left, dt)) ->
            let (nextNode, xs) = lttakeV2(lt(), dt())
            in (nextNode, v()::xs)
        | (LNode(v, _, rt), LCons(Right, dt)) ->
            let (nextNode, xs) = lttakeV2(rt(), dt())
            in (nextNode, v()::xs)
;;
*)


let rec ltmap f ltree =
    match ltree with
          LEmpty -> LEmpty
        | NodeL(v, lt, rt) -> NodeL(f v, ltmap f (lt), function () -> ltmap f (rt()))
        | LNode(v, lt, rt) -> LNode((function () -> f (v())), (function() -> ltmap f (lt())), (function()-> ltmap f (rt())))
        | NodeR(v, lt, rt) -> NodeR(f v, (function() -> ltmap f (lt())), ltmap f (rt))
        | Node(v, lt, rt) -> Node(f v, (function() -> ltmap f (lt())), function () -> ltmap f (rt()))
        | NodeC(v, lt, rt) -> NodeC(f v, ltmap f (lt), ltmap f (rt))


(*Metoda pobrania elementow we wskazanym kierunku oraz aktualizacji drzewa leniwego*)
let lttake (tree, direction) = (take(tree, direction), actTree(tree, direction));;



(*Przyklad drzewa na ktorym pracujemy*)
lmbtgetlevels (4, ltfrom 0);;



(*default test*)
let dirllist = toLazyList [Left;Right; Right];;
let (elemList, newTree) = lttake ((ltfrom 0), dirllist);;


(*let (q1, q2) = lttakeV2((ltfrom 0), dirllist);;*)

(*test na empty*)
lttake(LEmpty, LNil);;


(*test na nie wiem co*)
let (p1, p2) = lttake(ltfrom 0, LNil);;



let testdirllsit = toLazyList [Left; Right; Right; Right];;

let (r1, r2) = lttake(newTree, testdirllsit);;



(*modyfikacja tests*)
let sqrNodes = ltmap (function x -> x*x);;
lmbtgetlevels (4, sqrNodes (ltfrom 0));;
lmbtgetlevels(4, sqrNodes(newTree));;