(*Inicjalizacja typu fabryki*)
type factory = Final_Machine of int | Machine of int * factory list;;





(*let getTime machine =*)
(*    match machine with*)
(*          Machine(t,_) -> t*)
(*        | Final_Machine (ft) -> ft*)
(*;;*)

(*let max lst = List.fold_left max 0 lst*)

(*let timesFromDifRoutes fact =*)
(*    let rec calc (time, mList, listRest, q, firstMachineTime) =*)
(*        match mList with*)
(*              [] -> []*)
(*            | Machine(t, ms)::r ->  calc(q*time+t, ms, r, q, firstMachineTime)*)
(*            | Final_Machine(t)::r -> time+t::calc(firstMachineTime, listRest, r, q+1, firstMachineTime)*)
(*    in calc (0, fact, [], 1, getTime(List.hd (fact)))*)
(*;;*)


(*let productionTime fact = max (timesFromDifRoutes fact);;*)






(*Metoda, obliczajaca czasy wszystkich drog do koncowej maszyny*)
let rec timesFromDifRoutes (factL, time) =
    match factL with
          [] -> []
        | Machine(t, ms)::r -> timesFromDifRoutes(ms, time+t) @ timesFromDifRoutes(r,time)
        | Final_Machine(t)::r -> time+t::timesFromDifRoutes(r, time)
;;


(*pomocnicza metoda wyboru najwiekszego elementu z listy*)
let max lst = List.fold_left max 0 lst


(*metoda, wybierajaca najdluzsza droge do koncowej maszyny. Najdluzsza, bo maszyna musi zaczekac cale jej wejscie*)
let productionTime fact = max (timesFromDifRoutes (fact, 0));;




(*Przyklad test1.png*)
let test1 = [Machine (5, [Machine (8, [Final_Machine(10)]); Machine (12, [Final_Machine(10)]);Machine (9, [Final_Machine(10)])])];;

(*Przyklad test2.png*)
let test2 = [Machine(3, [Machine(4,[Final_Machine(8)]); Machine(6,[Final_Machine(8)])])];;

(*Przyklad test3.png*)
let test3 = [Machine(3,[Machine(4,[Machine(3,[Final_Machine(10)])]); Machine(5, [Machine(3,[Final_Machine(10)]); Machine(8,[Final_Machine(10)])]); Machine(6,[Machine(8,[Final_Machine(10)])])])];;

(*Tylko jedna maszyna*)
let test4 = [Final_Machine(15)];;

(*Maszyna (20) -> Maszyna (3)*)
let test5 = [Machine(20, [Final_Machine(3)])];;




productionTime test1;;

productionTime test2;;

productionTime test3;;

productionTime test4;;

productionTime test5;;


