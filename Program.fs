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

let rec qsort = function
|[]->[]
|pivot::rest->
        rest |> List.partition(fun i->i <pivot)
            ||> fun left right->(qsort left)@[pivot]@qsort right



let rec MergeSort (input: int list) = 
    
    let Merge (left: int list, right: int list) =
        let rec mergeLists (left: int list, right: int list, output: int list) =
            if left = [] then output@right
            else if right = [] then output@left
            else if left.Head < right.Head then mergeLists (left.Tail, right, output@[left.Head])
            else mergeLists (left, right.Tail, output@[right.Head])
        mergeLists (left, right, [])

    if input.Length = 0 then []
    else if input.Length = 1 then input
    else if input.Length = 2 then
        if input.[0] > input.[1] then [input.[1]; input.[0]]
        else input
    else
        let left = MergeSort (input |> Seq.take (input.Length / 2) |> Seq.toList)
        let right = MergeSort (input |> Seq.skip (input.Length / 2) |> Seq.toList)
        Merge (left, right)

[<EntryPoint>]
let main args =
    for arg in args do
        let newArg = PigLatin.toPigLatin arg
        printfn "%s in Pig Latin is: %s" arg newArg
        printf "%A" (MergeSort [-22;2;34;-2;0;9;-5;14;-55;74;13])
        

    0