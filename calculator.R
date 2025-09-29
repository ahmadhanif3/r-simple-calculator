# --- GLOBAL VARIABLE ---
calc.state = TRUE
calc.history <- data.frame(expression=character(), result=numeric())
calc.ans <- 0
help.text = "This are the things you can do:
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
Enjoy!\n\n"

# --- CALCULATE FUNCTION ---
calc.calculation = function(a, b, op){
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

# --- HISTORY FUNCTION ---
calc.show.history = function(history){
  # - HISTORY IS EMPTY -
  if (nrow(history) == 0) {
    cat('No calculations was done, yet... ;)')
  }
  else {
    for (i in 1:nrow(history)){
      cat(paste(i, ":", history[i, "expression"], "=", history[i, "result"], "\n"))
    }
  }
}

# --- EXIT FUNCTION ---
calc.exit = function() {
  cat("Thank you for using the calculator!\n")
  return(FALSE)
}

# --- CALCULATION PRECEDENCE FUNCTION ---
calc.precedence = function(inp, op) {
  indexes <- which(inp == op)
  order <- 0
  
  for (idx in indexes) {
    idx <- idx - 2 * order
    
    # - CALCULATE, CHANGE a WITH RESULT -
    a <- inp[idx-1]
    b <- inp[idx+1]
    ans <- calc.calculation(a, b, op)
    inp[idx-1] = ans
    
    # - EDIT INPUT, CHANGE IDX MANIPULATION -
    order <- order + 1
    idx.to.remove <- c(idx, idx+1)
    inp <- inp[-idx.to.remove]
  }
  
  return(inp)
}

calc.calculate = function(inp, ans, history) {
  tryCatch({
    split.inputs <- strsplit(inp, " +")[[1]]
    
    # - TO BE DEVELOPPED, INPUT HANDLING -
    if (length(split.inputs) %% 2 == 0) {
      cat("Unfinished Statement...")
    }
    else {
      split.inputs <- ifelse(split.inputs == 'ans', ans, split.inputs)
      
      ops <- c('^', '*', '/', '%%', '+', '-')
      for (op in ops) {
        split.inputs <- calc.precedence(split.inputs, op)
      }
      
      ans <- split.inputs[1]
      history <- rbind(history, data.frame(expression=inp, result=ans))
      cat(ans)
      return(list(ans = ans, history = history))
    }
    
    # - ERROR HANDLING -
  }, error = function(e) {
    cat("Error:", e$message)
  })
}

cat("Hello user! Welcome to the simple calculator.\n", help.text)
# --- MAIN LOOP ---
while (calc.state) {
  input <- readline(prompt = "> ")
  
  # --- COMMAND HANDLING ---
  switch(input,
         "exit" = {calc.state <- calc.exit()},
         "hist" = calc.show.history(calc.history),
         "ans" = cat(calc.ans),
         "help" = cat(help.text),
         "clear" = cat("\014"),
         {
          result <- calc.calculate(input, calc.ans, calc.history)
          calc.ans <- result$ans
          calc.history <- result$history
         }
  )
}

print('Program Successfully Ran')