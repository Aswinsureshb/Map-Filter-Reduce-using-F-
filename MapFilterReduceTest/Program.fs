// For more information see https://aka.ms/fsharp-console-apps

let salaryList = [75000; 48000; 120000; 190000; 300113; 92000; 36000]


let getFederalIncome income =
    match income with
    | i when i <= 49020 -> (i + 20000) * 15 / 100
    | i when i > 49020 && i <= 98040 -> (i - 49020) * 205 / 100 + ((i - 49020) * 205 / 100 + (49020 - 20000) * 15 / 100)
    | i when i > 98040 && i <= 151978 -> (i - 98040) * 26 / 100 + ((i - 98040) * 26 / 100 + (98040 - 49020) * 205 / 100 + (49020 - 20000) * 15 / 100)
    | i when i > 151978 && i <= 216511 -> (i - 151978) * 29 / 100 + ((i - 151978) * 29 / 100 + (151978 - 98040) * 26 / 100 + (98040 - 49020) * 205 / 100 + (49020 - 20000) * 15 / 100)
    | _ -> (income - 216511) * 33 / 100 + ((income - 216511) * 33 / 100 + (216511 - 151978) * 29 / 100 + (151978 - 98040) * 26 / 100 + (98040 - 49020) * 205 / 100 + (49020 - 20000) * 15 / 100)


let FilterHighIncome = 
    salaryList
    |> List.filter (fun i -> i > 100000)
    |> List.map getFederalIncome


let ReduceSalary =
    salaryList
    |> List.filter (fun i -> i >= 50000 && i <= 100000)
    |> List.fold (+) 0

printfn "Salaries with added tax: %A" FilterHighIncome
printfn "Sum of salaries in between $50,000 and $100,000: %d" ReduceSalary

// Tail Recursive function

let SumOfMultiples x =
    let rec SumFunction acc number =
        if number = 0 then
            acc
        else
            SumFunction (acc + number) (number - 3)
    SumFunction 0 x


let Result = SumOfMultiples 27


printfn "Sum of multiples of 3 up to 27: %d" Result