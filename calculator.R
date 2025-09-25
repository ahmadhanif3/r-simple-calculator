cat("Hello user! Welcome to the simple calculator. 
This are the things you can do:
* Do simple calculations with +, -, *, /, %%, ^
* In this version, the program runs calculations in order (not PEMDAS)
* Use the format -> num op num op num ...
* No other characters except operators mentioned and numbers are allowed
* Commands are case sensitive
* Use 'ans' in the calculation to use the last calculation result, or to view the last answer
* Use 'hist' to view all the previous calculations
* Use 'clear' to clear the terminal
* Use 'help' to show on how to use the program
* Use 'exit' to quit the program
Enjoy!\n\n")

# --- GLOBAL VARIABLE ---
calc.history <- data.frame(expression=character(), result=numeric())
calc.ans <- 0

# --- CALCULATE FUNCTION ---
calculate = function(a, b, op){
  a <- as.numeric(a)
  b <- as.numeric(b)
  
  # -- ZERO DIVISION ERROR --
  if (op == "/" && b == 0) {
    stop("Division by zero is not allowed")
  }
  
  ans = switch(op,
      "+" = a + b,
      "-" = a - b,
      "*" = a * b,
      "/" = a / b,
      "%%" = a %% b,
      "^" = a^b,
      stop("Unknown operator:", op) # -- UNKNOWN OPERATOR --
  )
  
  return(ans)
}

# --- MAIN LOOP ---
while (TRUE) {
  input <- readline(prompt = "> ")
  
  # --- COMMAND HANDLING ---
  # -- EXIT ---
  if (input == 'exit'){
    cat("Thank you for using the calculator!\n")
    break
  }
  # -- HISTORY --
  else if (input == 'hist'){
    # - HISTORY IS EMPTY -
    if (nrow(calc.history) == 0) {
      cat('No calculations was done, yet... ;)')
    }
    else {
      for (i in 1:nrow(calc.history)){
        cat(paste(i, ":", calc.history[i, "expression"], "=", calc.history[i, "result"], "\n"))
      }
    }
    next
  }
  # -- ANSWER --
  else if (input == "ans") {
    cat(calc.ans)
    next
  }
  # -- HELP -- 
  else if (input == "help") {
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
  # -- CLEAR TERMINAL --
  else if (input == "clear") {
    cat("\014")
    next
  }
  
  tryCatch({
    split.inputs <- strsplit(input, " +")[[1]]
    
    # - TO BE DEVELOPPED, INPUT HANDLING -
    if (length(split.inputs) %% 2 == 0) {
      cat("Unfinished Statement...")
    }
    else {
      split.inputs <- ifelse(split.inputs == 'ans', calc.ans, split.inputs)
      
      calc.a <- split.inputs[1]
      calc.op.idx <- 2
      calc.b.idx <- 3
      
      # - CALCULATION ITERATION -
      while (calc.b.idx <= length(split.inputs)) {
        calc.b <- split.inputs[calc.b.idx]
        calc.a <- calculate(calc.a, calc.b, split.inputs[calc.op.idx])
        calc.op.idx = calc.op.idx + 2
        calc.b.idx = calc.b.idx + 2
      }
      
      calc.ans <- calc.a
      calc.history <- rbind(calc.history, data.frame(expression=input, result=calc.ans))
      cat(calc.ans)
    }
    
  # - ERROR HANDLING -
  }, error = function(e) {
    cat("Error:", e$message)
  })
}

print('Program Successfully Ran')