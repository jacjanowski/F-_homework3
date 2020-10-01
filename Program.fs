#light

module hw03

//
// length:
//
// Computes the length of a list.
// Example: length [15;8;25;17;12] => 5
//
let rec length L =
    match L with
    | head :: tail -> 1 + length tail   //add 1 for every index until we reach the end of list.
    |[] -> 0

//
// sum
//
// Computes the sum of the values in a list of integers.
// Example: sum [15;8;25;17;12] => 77
//
let rec sum L =
    match L with
    | head :: tail -> head + sum tail   //add the first element be added to rest of list. Then second element + end of list, etc.
    |[] -> 0

//
// average
//
// Computes the floating point average of the values in a list of integers.
// You will need to convert ints to floats before dividing the sum by length to find the average.
// Example: average [15;8;25;17;12] => 15.4
//
let average L =
    let rec accumulate (sum, count : float) list =
        match list with
        | head::tail -> accumulate (sum + head, count + 1.0) tail
        | [] -> (sum, count)
    let sum, count = accumulate (0.0, 0.0) L
    let average = sum / count
    average
    



[<EntryPoint>]
let main argv =
    printf "filename> "
    let filename   = System.Console.ReadLine()
    let data_array = System.IO.File.ReadAllLines(filename)
    let data_list  = Array.toList data_array
    //
    // convert strings to integers:
    //
    let values = List.map (fun s -> int s) data_list
    printfn "%A" values
    //
    let len = length values
    printfn "%A" len
    //
    let total = sum values
    printfn "%A" total
    //
    let avg = average values
    printfn "%A" avg
    //
    0
