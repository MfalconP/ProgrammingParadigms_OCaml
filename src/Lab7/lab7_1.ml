(* Comparator signature (task 1a) *)
module type COMPARATOR =
sig
    type compResult = LT | EQ | GT
    type t
    val compare: t -> t -> compResult
end;;


(* integer comparator structure (task 1b) *)
module IntComparator: COMPARATOR with type t = int =
struct
    type compResult = LT | EQ | GT
    type t = int
    let compare n1 n2 =  if n1<n2 then LT else
                            if n1>n2 then GT else EQ
end;;


(* string comparator structure (task  1b) *)
module StringComparator: COMPARATOR with type t = string  =
struct
    type compResult = LT | EQ | GT
    type t = string
    let compare n1 n2 =  if n1<n2 then LT else
                            if n1>n2 then GT else EQ
end;;



(* parametrized module OrderedListImpl (task 1c) *)
module OrderedListImpl (Comp: COMPARATOR) =
struct
    type listType = Comp.t
    type t = listType list

    exception Empty of string

    let create = []

     let rec insert list e =
        match list with
              [] -> [e]
            | h::tl ->
                (match Comp.compare e h with
                      Comp.LT -> e::h::tl
                     | Comp.EQ -> e::h::tl
                     | Comp.GT -> h::(insert tl e)
                )
     ;;

     let retrive list =
        match list with
              [] -> raise (Empty "module Ordered List: retrive")
            | h::tl -> (h, tl)
     ;;

end;;



(* OrderedList signature (task 1d) *)
module type ORDERED_LIST =
sig
    type listType
    type t
    exception Empty of string
    val create: t
    val insert: t -> listType -> t
    val retrive: t -> listType* t
end;;

(* OrderedList module using abstraction (task 1d) *)
module OrderedList (Comp: COMPARATOR) : ORDERED_LIST with type listType = Comp.t = OrderedListImpl (Comp: COMPARATOR);;



(* |||||||||||||___TESTS___||||||||||||| *)


(* comparator tests *)
let x = IntComparator.compare 5 2;;
let y = StringComparator.compare "abcd" "bcde";;



(* module OrderedListImpl test (Integer example) *)
module IntOrderedList = OrderedListImpl(IntComparator);;


let list = IntOrderedList.create;;

let list = IntOrderedList.insert list 5;;
let list = IntOrderedList.insert list 4;;
let list = IntOrderedList.insert list 6;;
let list = IntOrderedList.insert list 3;;
let list = IntOrderedList.insert list 5;;

let (elem1, list) = IntOrderedList.retrive list;;

let list = IntOrderedList.insert list 2;;
let (elem2, list) = IntOrderedList.retrive list;;
let (elem3, list) = IntOrderedList.retrive list;;
let (elem4, list) = IntOrderedList.retrive list;;
let (elem5, list) = IntOrderedList.retrive list;;
let (elem6, list) = IntOrderedList.retrive list;;

let (elem7, list) = IntOrderedList.retrive list;;


(* module OrderedListImpl test (String example) *)
module StrOrderedList = OrderedListImpl(StringComparator);;


let listSTR = StrOrderedList.create;;

let listSTR = StrOrderedList.insert listSTR "e";;
let listSTR = StrOrderedList.insert listSTR "d";;
let listSTR = StrOrderedList.insert listSTR "f";;
let listSTR = StrOrderedList.insert listSTR "b";;
let listSTR = StrOrderedList.insert listSTR "c";;

let (el1, listSTR) = StrOrderedList.retrive listSTR;;

let listSTR = StrOrderedList.insert listSTR "a";;
let (el2, listSTR) = StrOrderedList.retrive listSTR;;
let (el3, listSTR) = StrOrderedList.retrive listSTR;;
let (el4, listSTR) = StrOrderedList.retrive listSTR;;
let (el5, listSTR) = StrOrderedList.retrive listSTR;;
let (el6, listSTR) = StrOrderedList.retrive listSTR;;

let (el7, listSTR) = StrOrderedList.retrive listSTR;;





(* OrderedList module using abstraction tests (Integer example) *)
module IntOrderedListABS = OrderedList(IntComparator);;


let listABS = IntOrderedListABS.create;;

let listABS = IntOrderedListABS.insert listABS 5;;
let listABS = IntOrderedListABS.insert listABS 4;;
let listABS = IntOrderedListABS.insert listABS 6;;
let listABS = IntOrderedListABS.insert listABS 3;;
let listABS = IntOrderedListABS.insert listABS 5;;

let (e1, listABS) = IntOrderedListABS.retrive listABS;;

let listABS = IntOrderedListABS.insert listABS 2;;

let (e2, listABS) = IntOrderedListABS.retrive listABS;;
let (e3, listABS) = IntOrderedListABS.retrive listABS;;
let (e4, listABS) = IntOrderedListABS.retrive listABS;;
let (e5, listABS) = IntOrderedListABS.retrive listABS;;
let (e6, listABS) = IntOrderedListABS.retrive listABS;;

let (e7, listABS) = IntOrderedListABS.retrive listABS;;



module StrOrderedListABS = OrderedList(StringComparator);;


let listSTR_ABS = StrOrderedListABS.create;;

let listSTR_ABS = StrOrderedListABS.insert listSTR_ABS "e";;
let listSTR_ABS = StrOrderedListABS.insert listSTR_ABS "d";;
let listSTR_ABS = StrOrderedListABS.insert listSTR_ABS "f";;
let listSTR_ABS = StrOrderedListABS.insert listSTR_ABS "b";;
let listSTR_ABS = StrOrderedListABS.insert listSTR_ABS "c";;

let (el_1, listSTR_ABS) = StrOrderedListABS.retrive listSTR_ABS;;

let listSTR_ABS = StrOrderedListABS.insert listSTR_ABS "a";;
let (el_2, listSTR_ABS) = StrOrderedListABS.retrive listSTR_ABS;;
let (el_3, listSTR_ABS) = StrOrderedListABS.retrive listSTR_ABS;;
let (el_4, listSTR_ABS) = StrOrderedListABS.retrive listSTR_ABS;;
let (el_5, listSTR_ABS) = StrOrderedListABS.retrive listSTR_ABS;;
let (el_6, listSTR_ABS) = StrOrderedListABS.retrive listSTR_ABS;;

let (el_7, listSTR_ABS) = StrOrderedListABS.retrive listSTR_ABS;;

