open System



module PigLatin = 
    let toPigLatin (word: string) =
        let isVowel (c: char) =
            match c with
            | 'a' | 'e' | 'i' |'o' |'u'
            | 'A' | 'E' | 'I' | 'O' | 'U' -> true
            |_ -> false
    
        if isVowel word[0] then
            word + "yay"
        else
            word[1..] + string(word[0]) + "ay"


let rec quicksort list = 
    match list with
    | [] ->
        []
    | firstElem::otherElements ->
        let smallerElements = 
            otherElements
            |> List.filter(fun e->e < firstElem)
            |> quicksort
        let largerElements =
            otherElements
            |> List.filter(fun e ->e >= firstElem)
            |> quicksort

        List.concat [smallerElements; [firstElem]; largerElements]



[<EntryPoint>]
let main args =
    for arg in args do
        let newArg = PigLatin.toPigLatin arg
        printfn "%s in Pig Latin is: %s" arg newArg
        printf "%A" (quicksort [1;5;23;18;9;1;3])

    0