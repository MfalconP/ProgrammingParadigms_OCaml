let split3Tail list =
    let rec split3TailRec (list, l1, l2, l3) =
        match list with
                     []         ->   (l1,l2,l3)
            | (e1::e2::e3::t)   ->   split3TailRec(t, e1::l1, e2::l2, e3::l3)
            |         _         ->   (l1, l2, l3)
    in split3TailRec (list, [], [], [])
;;


split3Tail [];;

split3Tail [1;2;3;4;5;6;7;8;9];;

split3Tail [true;false;false];;

split3Tail ["Ala"; "ma"; "kota"; "dużego"];;

split3Tail ['a';'b';'c'];;

split3Tail [1.;2.;3.;4.;5.;6.;7.;8.;];;

split3Tail [1];;

split3Tail ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'];;




let x = 5;;