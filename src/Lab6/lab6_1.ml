let sum2Lists (row) =
    let rec sum2ListsHELP (row, exElem, actElem, n) =
        match (row, exElem, actElem, n) with
            ([], exElem, actElem, _) -> [exElem+actElem; exElem+actElem]
            | (h::tl, _, _, 0) -> sum2ListsHELP(tl, 0, h, 1)
            | (h::tl, exElem, actElem, n) -> (exElem+actElem)::sum2ListsHELP(tl, exElem+actElem, h, n)
    in sum2ListsHELP (row, 0, 0, 0)
;;



let pascalCascadeF n =
    let rec cascadeLines (n, line)=
        match (n, line) with
              (1, line) -> line
            | (n, line) -> if (n<1) then failwith("There is no such line in pascal cascade")
                            else cascadeLines(n-1, sum2Lists(line))
    in cascadeLines (n, [1])
;;



pascalCascadeF 1;;
pascalCascadeF 2;;
pascalCascadeF 3;;
pascalCascadeF 4;;
pascalCascadeF 5;;
pascalCascadeF 6;;

pascalCascadeF 10;;

pascalCascadeF 0;;
