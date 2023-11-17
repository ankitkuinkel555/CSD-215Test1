//test1

//Question number 1

//a.Filter through the list to find high-income salaries.
// Create a list of salaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Display the original list of salaries
printfn "Original Salaries: %A" salaries

// Filter: Display high-income salaries (above $100,000)
let highIncomeSalaries = List.filter (fun salary -> salary > 100000) salaries
printfn "High-Income Salaries (above $100,000): %A" highIncomeSalaries


//b. Use the map function to calculate tax for all salaries based on the table provided.
// Create a list of salaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Display the original list of salaries
printfn "Original Salaries: %A" salaries

// Tax brackets and rates
let taxBrackets = [
    (49020, 0.15);
    (98040, 0.205);
    (151978, 0.26);
    (216511, 0.29);
    (System.Int32.MaxValue, 0.33)
]

// Function to calculate tax for a given salary
let calculateTax salary =
    let rec calculateTaxHelper remainingBrackets totalTax =
        match remainingBrackets with
        | [] -> totalTax
        | (limit, rate) :: rest ->
            if salary > limit then
                calculateTaxHelper rest (totalTax + (float limit * rate))
            else
                calculateTaxHelper rest (totalTax + (float salary * rate))

    calculateTaxHelper taxBrackets 0.0

// Map: Calculate tax for each salary
let taxes = List.map calculateTax salaries
printfn "The taxes for each of the salaries are: %A" taxes


//c. Filtering salaries less than $49,020 and add $20,000 to these salaries using the map function.
// Create a list of salaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Display the original list of salaries
printfn "Original Salaries: %A" salaries

// Filter salaries less than $49,020 and add $20,000
let adjustedSalaries =
    salaries
    |> List.filter (fun salary -> salary < 49020)
    |> List.map (fun salary -> salary + 20000)

// Display the adjusted salaries
printfn "The salaries less than 49,020 with adding 20,000 are: %A" adjustedSalaries



//d. Filter salaries between $50,000 and $100,000 and sum them all using the reduce/fold function. 
// Create a list of salaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Display the original list of salaries
printfn "Original Salaries: %A" salaries

// Filter salaries between $50,000 and $100,000 and sum them using List.fold
let sumOfFilteredSalaries =
    let isBetween50kAnd100k salary = salary >= 50000 && salary <= 100000
    let sumFunction acc salary = if isBetween50kAnd100k salary then acc + salary else acc

    List.fold sumFunction 0 salaries

// Display the sum of filtered salaries
printfn "The sum of Salaries between $50,000 and $100,000 are given below: %d" sumOfFilteredSalaries



//Question number 2
let rec sumMultiplesOf3TailRecursive currentNumber acc =
    match currentNumber with
    | 0 -> acc
    | _ -> sumMultiplesOf3TailRecursive (currentNumber - 3) (acc + currentNumber)

// Example usage:
let result = sumMultiplesOf3TailRecursive 27 0
printfn "%d" result
