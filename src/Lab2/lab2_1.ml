 let rec find list elem =

    match (list, elem) with
         ([], _ ) -> 0
        | (h::t, x) when h = x -> 1 + find t x
        | (h::t, x) -> find t x
;;


find [] 'a';;
find [] 1;;
find [] false;;
find [] 2.;;

let find123 = find [1;2;3];;
find123 0;;
find123 1;;
find123 3;;


(* 2-a, 2-b, 1-c *)
let findABCAB = find ['a';'b';'c';'a';'b';' '];;
findABCAB 'a';;
findABCAB 'b';;
findABCAB 'c';;
findABCAB 'q';;
findABCAB ' ';;



let findSTRING = find ["abc"; " a "];;
findSTRING " a ";;
findSTRING "abc";;
findSTRING "  a  ";;







find [1.;2.;3.;3.] 3.;;


(* 4 true || 3 false *)
let findBOOL = find [true;false;true;true;false;true;false];;
findBOOL true;;
findBOOL false;;


let find1231231  = find [1;2;3;1;2;3;1];;
find1231231 1;;
find1231231 3;;


