
#################
#first we are going to read the textfile from the working directory
word_file <- "words.txt"
words <- as.list(readLines(word_file))

#then we are going to take a sample from the list using the sample() function 
word_for_play <- sample(words, size = 1)
word_length <- nchar(word_for_play)
# We will create a vector to store the guessed letters
guessed_letters <- vector(mode = "character", length = 0)
# As for the rules, there are 8 guesses allowed so we will define the maximum number of incorrect guesses
incorrrect_limit <- 8

#then I will welcome the user to the game and let them know how long the secret word is.
print(paste("Welcome to Hangman! The secret word is", word_length, "letters long."))

#####list(secret_word = secret_word, guessed_letters = guessed_letters, incorrrect_limit = incorrrect_limit)
##############

#For this game, they have 8 chances to guess the wrong letter before they will lose. 

#we are going to create a while loop to ensure that the input is correct and we can proceed. 
valid_input <- FALSE # first we have to set valid input to false so that it will loop through the while loop
guess_count <- 0 
while (!valid_input){
  #First we will ask for them to input their first guess 
  user_input = readline(prompt = "Please enter your first guess now (one letter):")
  
  if (nchar(user_input) == 1) { #here is the outer most if statement to ensure that the input is valid 
    if (grepl("^[a-zA-Z]$", user_input)) { #here is the second if statement which will check to see if the input is either an upper case or lower case letter 
      valid_input <- TRUE #once the conditions are met then we can exit the loop 
      print("Valid input!")

      # Display the secret word with dashes for unguessed letters
      guessed_letters <- vector(mode = "character", length = 0)
      word_display <- ""
      for (user_input in strsplit(as.character(word_for_play), "")[[1]]) {
        if (user_input %in% guessed_letters) {
          word_display <- paste(word_display, user_input, sep = "")
        } else {
          word_display <- paste(word_display, "_", sep = "")
        }
      }
      print(word_display)
      
      # Display the guessed letters
      guessed_letters_str <- paste(guessed_letters, collapse = ", ")
      print(paste("Guessed letters:", guessed_letters_str))
      
      
      guess_count = guess_count + 1
    } else {
      print("Invalid input. You must enter a letter. Try again.")
    }
  } else { # here is the else statement for the outer most if statement which will promt the user to try again
    print("Invalid input. You must enter one letter only. Try again.")
  }
}



#at this point we have out first input and it is valid. 
while(guess_count <= 6) {
  if (user_input %in% strsplit(as.character(word_for_play), "")[[1]]) {
  print("Good job! The secret word has that letter.")
  print(paste("Used letters:", used_words))
  } else {
  print(FALSE)
  }
}

