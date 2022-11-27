let polishTime (para) =
    if fst para < 0 || fst para > 23 || snd para < 0 || snd para > 59 then raise (Failure "Nieprawidlowy czas")
    else if fst para = 0 then string_of_int(fst para + 12) ^ ":" ^ string_of_int (snd para) ^ " w nocy"
    else if fst para <= 4 && fst para > 0 then string_of_int (fst para) ^ ":" ^ string_of_int (snd para) ^ " w nocy."
    else if fst para <=12 then string_of_int (fst para) ^ ":" ^ string_of_int (snd para) ^ " rano"
    else if fst para <= 17 then string_of_int (fst para-12) ^ ":" ^ string_of_int (snd para) ^ " po poludniu."
    else string_of_int (fst para-12) ^ ":" ^ string_of_int (snd para) ^ " wieczorem.";;




polishTime((5,15));;

polishTime((13,20));;

polishTime((19,43));;

polishTime((1,18));;

polishTime((0,30));;



polishTime((24,00));;

polishTime((22,65));;

polishTime((24,65));;

