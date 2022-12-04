let pascalCascadeI n =
    if (n<1) then failwith("There is no such line in pascal cascade")
    else
        let i = ref 1 and arr1 = ref (Array.make 1 1) and arr2 = ref (Array.make 2 1);
        in begin
        	    while (!i < n) do
        	    arr2 := Array.make (!i + 1) 1;

        	    for l=1 to (!i - 1) do
                    !arr2.(l) <- (!arr2.(l-1) + !arr1.(l))
                    done;

                !arr2.(!i) <- !arr2.(!i-1);

                arr1 := Array.make (!i + 1) 1;
                arr1 := Array.copy !arr2;

                i := !i + 1
        	    done;
            !arr1
            end
;;


pascalCascadeI 1;;
pascalCascadeI 2;;
pascalCascadeI 3;;
pascalCascadeI 4;;
pascalCascadeI 5;;
pascalCascadeI 6;;

pascalCascadeI 10;;

pascalCascadeI 0;;
pascalCascadeI (-10);;

