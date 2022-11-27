(*w mojej konwencji, dwukropek : oznacza to, że liczba jest zespolonej, bo ma dwa argumenty, jak dwie kropki (-_o) *)

(*dodawanie*)
let (+:) (x1,x2) (y1,y2) = (x1 +. y1, x2 +. y2);;

(*odejmowanie*)
let (-:) (x1,x2) (y1,y2) = (x1 -. y1, x2 -. y2);;

(*mnozenie*)
let ( *: ) (x1,x2) (y1,y2) = ((x1 *. y1) -. (x2 *. y2) , (x1 *. y2) +. (x2 *. y1));;

(*negacja*)
let (~:) (x1,x2) = (x1, ~-. x2);;

(*modul*)
let (!:) (x1,x2) = (sqrt (x1 *. x1 +. x2 *. x2));;

(*argument*)
(*algorytm wzięty z wikipedium, print screen umieszczony w wordzie*)
let (?:) (a,b) =
    if (a > 0.) then atan (b /. a)
    else if (a < 0.) then (atan (b /. a) +. Float.pi)
    else if (a = 0.) then
        if (b > 0.) then Float.pi /. 2.
        else if (b < 0.) then Float.pi /. -2.
        else raise (Failure "niokreslony argument")
    else raise (Failure "niokreslony argument");;




(*funkcja obliczania pierwistka n-tego stopnia liczby zespolonej*)
(*jedna z postaci wzora de Moivre, wykorzystująca rekurencje ogonową dla obliczenia ilosci iteracji i odpowiedzi*)
let roots (x, y) n =
    let rec rootsTAIL (x, y, n, k) =
        match k with
               -1.             ->                  []
            |   _              ->     (((!: (x,y)) ** (1. /. n)) *. cos(((?: (x, y)) +. 2. *. Float.pi *. k) /. n),  ((!: (x,y)) ** (1. /. n)) *. sin(((?: (x, y)) +. 2. *. Float.pi *. k) /. n))::rootsTAIL(x, y, n, k -. 1.)
    in rootsTAIL(x, y, n, n -. 1.)
;;





(*MODYFIKACJA*)
let (+*) (x, y) n =
    ((!: (x,y)) ** n) *. cos(n *. (?: (x, y))),  ((!: (x,y)) ** n) *. sin(n *. (?: (x, y)));;




let add = (5.,3.) +: (1.,6.);;
let add0 = (0.,0.) +: (0., 1.);;



let min = (5.,3.) -: (1.,6.);;
let min0 = (1.,1.) -: (2.,-2.);;


let multiply = (5., 2.) *: (3.,-7.);;
let mult0 = (0.,0.) *: (5.,10.);;


let neg = ~: (5.,-3.);;

let modul = !: (3., 4.);;
let modul0 = !: (1.5, 15.8);;


let arg = ?: (1.,1.);;
let arg0 = ?: (0.,0.);;


roots (1., -27.) (3.);;
(*MOD*)
let abcd = (1.,1.) +* 3.;;













