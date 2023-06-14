#assignment2 

#the user will be asked to enter a 3 digit positive number 
no_input <- readline(prompt = "Please enter a 3 digit positive number:")

#then the program will check if the number meets certain requirements 
#this is where the outer if statement will start 
if (is.numeric(as.numeric(no_input)) & #first it will check if the input is a number
    (as.numeric(no_input) %% 1 == 0) & #then it will check if it is a whole number without any decimals 
    (((as.numeric(no_input) %/% 10) %/% 10) %/% 10) == 0) { #then it is going to check if it is a 3 digit number by getting rid of the right most digit 3 times 
  
  #this is where the program will find out if the number is narcassistic, this is also where the inner if statement will be
  split_no <- unlist(strsplit(as.character(no_input), "")) #the number will be split into a list and then using the unlist() function, it will be put into a vector instead 
  print("yes")
} else { #this is the else condition for the outer if statement 
  print("Entry is not valid. Terminating...") #the program will terminate if the original conditions for a 3 digit positive number input are not met. 
}

