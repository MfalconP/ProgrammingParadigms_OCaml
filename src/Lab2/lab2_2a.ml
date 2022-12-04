let rec split3Rec list =
    match list with
                 []         ->    ([], [], [])
        |   e1::e2::e3::t   ->    let (l1,l2,l3) = split3Rec t in (e1::l1, e2::l2, e3::l3)
        |         _         ->    ([], [], [])
;;



split3Rec [];;

split3Rec [1;2;3;4;5;6;7;8;9];;

split3Rec [true;false;false];;

split3Rec ["Ala"; "ma"; "kota"; "dużego"];;

split3Rec ['a';'b';'c'];;

split3Rec [1.;2.;3.;4.;5.;6.;7.;8.;];;

split3Rec [1];;

split3Rec ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'];;