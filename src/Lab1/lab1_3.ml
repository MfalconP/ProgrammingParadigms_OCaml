let rec insert (l1, x, n) =
if n<0 then x::l1
else if l1 = [] then x::[]
else if n = 0 then x::l1
else List.hd l1::insert(List.tl l1, x, n-1);;


let list = [1;2;3;4;5];;

insert(list, 6, 0);;
insert(list, 6, -10);;
insert(list, 6, 20);;
insert(list, 6, 3);;
insert(list, 6, 5);;


insert([],1,10);;

insert(["b";"c"], "a", 0);;

insert(["b";"c"], "d", 10);;

insert([true;false;true], false, 4);;

insert([1.;2.;], 0., -10);;