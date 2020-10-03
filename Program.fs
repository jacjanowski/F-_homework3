#light


//Jacob Janowski - 672941241
//jjanow7
//Homework 3: compute the sum, length of array, and average of the given values listed within data.txt.
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
    let rec add (sum, count : float) list =             //inner function call to 'add()' which will take the head of list every iteration and add it to 'sum'.
        match list with
        | head::tail -> add (sum + head, count + 1.0) tail
        | [] -> (sum, count)                            //if empty list, return sum ,count
    let sum, count = add (0.0, 0.0) L
    let average = sum / count       //average will equal our floated values of sum / count.
    average     //return
    



[<EntryPoint>]
let main argv =
    printf "filename> "
    let filename   = System.Console.ReadLine()
    let data_array = System.IO.File.ReadAllLines(filename)
    let data_list  = Array.toList data_array
    let data_list2  = Array.toList data_array       //convert data_array to flaoted values.
    //
    // convert strings to integers:
    //
    let values = List.map (fun s -> int s) data_list
    let values2 = List.map (fun s -> float s) data_list2            //initialize map of floated values.
    printfn "%A" values
    //
    let len = length values
    printfn "%A" len
    //
    let total = sum values
    printfn "%A" total
    //
    let avg = average values2       //use floated values.
    printfn "%A" avg
    //
    0                                  