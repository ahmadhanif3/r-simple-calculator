cat("Hello user! Welcome to the simple calculator. 
This are the things you can do:
* Do simple calculations with +, -, *, /, %%, ^
* In this version, the program runs calculations in order (not PEMDAS)
* Use the format -> num op num op num ...
* No other characters except operators mentioned and numbers are allowed
* Use 'ans' in the calculation to use the last calculation result, or to view the last answer
* Use 'hist' to view all the previous calculations
* Use 'clear' to clear the terminal
* Use 'help' to show on how to use the program
* Use 'exit' to quit the program
Enjoy!\n\n")

calc.state <- TRUE
calc.history <- data.frame(expression=character(), result=numeric())
calc.ans <- 0

calculate = function(a, b, op){
  a <- as.numeric(a)
  b <- as.numeric(b)
  
  ans = switch(op,
      "+" = a + b,
      "-" = a - b,
      "*" = a * b,
      "/" = a / b,
      "%%" = a %% b,
      "^" = a^b,
      0)
  
  return(ans)
}

while (calc.state) {
  input <- readline(prompt = "> ")
  
  if (tolower(input) == 'exit'){
    cat("Thank you for using the calculator!\n")
    calc.state <- FALSE
  }
  else if (tolower(input) == 'hist'){
    if (nrow(calc.history) == 0) {
      cat('No calculations was done, yet... ;)')
    }
    else {
      for (i in 1:nrow(calc.history)){
        cat(paste(i, ". ", calc.history[i, "expression"], " = ", calc.history[i, "result"], "\n"))
      }
    }
    next
  }
  else if (tolower(input) == "ans") {
    cat(calc.ans)
    next
  }
  else if (tolower(input) == "help") {
    cat("This are the things you can do:
* Do simple calculations with +, -, *, /, %%, ^
* In this version, the program runs calculations in order (not PEMDAS)
* Use the format -> num op num op num ...
* No other characters except operators mentioned and numbers are allowed
* Use 'ans' in the calculation to use the last calculation result, or to view the last answer
* Use 'hist' to view all the previous calculations
* Use 'help' to show on how to use the program
* Use 'exit' to quit the program
Enjoy!\n\n")
    next
  }
  else if (tolower(input) == "clear") {
    cat("\014")
  }
  else {
    split.inputs <- strsplit(input, " +")[[1]]
    
    if (length(split.inputs) %% 2 == 0) {
      cat("Unfinished Statement...")
    }
    else {
      calc.a <- split.inputs[1]
      if (calc.a == 'ans') {
        calc.a = calc.ans
      }
      calc.op.idx <- 2
      calc.b.idx <- 3
      
      while (calc.b.idx <= length(split.inputs)) {
        calc.b = split.inputs[calc.b.idx]
        if (calc.b == 'ans') {
          calc.b = calc.ans
        }
        calc.a <- calculate(calc.a, calc.b, split.inputs[calc.op.idx])
        calc.op.idx = calc.op.idx + 2
        calc.b.idx = calc.b.idx + 2
      }
      
      calc.ans <- calc.a
      calc.history <- rbind(calc.history, data.frame(expression=input, result=calc.ans))
      cat(calc.ans)
    }
  }
}

print('Program Successfully Ran')