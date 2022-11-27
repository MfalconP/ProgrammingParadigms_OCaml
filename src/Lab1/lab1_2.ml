
let rec differences (l1,l2) =
if l1 = []  && l2 = [] then 0
else if l1 = [] then 1+differences(l1, List.tl l2)
else if l2 = [] then 1+differences(List.tl l1, l2)
else if List.hd l1 = List.hd l2 then differences(List.tl l1, List.tl l2)
else 1 + differences(List.tl l1, List.tl l2);;

let list1 = 1::1::2::4::[];;
let list2 = 1::1::2::3::8::9::9::[];;

let list3 = 1::1::1::1::[];;
let list4 = 1::1::1::1::[];;


let list5 = "a"::"b"::"c"::"d"::[];;
let list6 = "a"::"a"::"c"::"a"::[];;

let list7 = 1.0::2.0::3.0::4.0::[];;
let list8 = 1.0::1.0::2.0::3.0::8.0::9.0::9.0::[];;

let list9 = true::true::false::true::[];;
let list10 = true::true::false::false::[];;


differences (list1, list2);;
differences (list2, list1);;
differences (list3, list4);;
differences (list5, list6);;
differences (list7, list8);;
differences (list9, list10);;