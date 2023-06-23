##ASSIGNMENT 3

#First we are going to read the textfile from the working directory
word_list <- readLines("words.txt")
# We want to choose a random word from the list, and want to announce how long it is 
secret_word <- tolower(sample(word_list, 1))
word_length <- nchar(secret_word)
#We will create a vector to store the guessed letters
guessed_letters <- vector(mode = "character", length = 0)
#As for the rules, there are 8 guesses allowed so we will define the maximum number of incorrect guesses
incorrrect_limit <- 8
game_state <- list(secret_word = secret_word, guessed_letters = guessed_letters, incorrrect_limit = incorrrect_limit)
#Welcome the user to the game and inform them about the secret word's length
print(paste("Welcome to Hangman! The secret word is", word_length, "letters long."))

# I will use a while loop so that it continues to ask for a user input until either the word is guessed or there has been more than 8 incorrect guesses
while (TRUE) { 
  # Display the secret word with dashes for unguessed letters
  # Initialize an empty string to hold the word display
  word_display <- ""
  #Iterate over each letter in the secret word, separating by nothing so it splits the word by each letter individually
  for (letter in strsplit(secret_word, "")[[1]]) {
    #Check if the letter is already guessed from the list of previously guessed letters
    if (letter %in% guessed_letters) {
      #If the letter is guessed we will add it to the word display
      word_display <- paste(word_display, letter, sep = "")
    } else {
      #If the letter is not guessed we will add a placeholder underscore to the word display
      word_display <- paste(word_display, "_", sep = "")
    }
  }
  # Print the word display to show the current state of the guessed word
  print(word_display)
  
  
  # Display the guessed letters so that the user can keep count and this will ensure that the game is efficient
  guessed_letters_str <- paste(guessed_letters, collapse = ", ")
  print(paste("Guessed letters:", guessed_letters_str))
  # Ask the user for a guess
  guess <- readline("Enter a letter: ")
  # We need to make sure that the guess meets the requirements 
  if (nchar(guess) != 1) { #it must only be one character long
    print("Invalid input. You must enter one letter only. Try again.") #if it is longer then it will tell the user what their input error was
    next
  }
  # We will convert the guess to lowercase to make identification easier 
  guess <- tolower(guess)
  
  # We also want to check if the input is a letter, it shouldn't be a number or character. 
  if (!grepl("^[a-z]$", guess)) {
    print("Invalid input. You must enter a letter. Please try again.")
    next
  }
  
  #After a valid input has been entered, it will be added to the list of guessed letters form before to update as the while loop continues to loop
  guessed_letters <- c(guessed_letters, guess)
  
  #In each loop, we will check if the conditions of the game have been met and if the game can be over. 
  
  #We will check if the player has guessed the entire secret word correctly
  if (all(strsplit(secret_word, "")[[1]] %in% guessed_letters)) {
    print("WINNER! You guessed the secret word!")
    return(TRUE)  #Return TRUE to indicate the game is over
  }
  
  #Then we will calculate the number of incorrect guesses by counting letters in guessed_letters not present in the secret word
  incorrect_guesses <- sum(!(guessed_letters %in% strsplit(secret_word, "")[[1]]))
  #If the player has exceeded the maximum allowed incorrect guesses, the game will end 
  if (incorrect_guesses >= incorrrect_limit) {
    print("Game over! You lost):")
    print(paste("The secret word was:", secret_word))
    return(TRUE)  # Return TRUE to indicate the game is over
  }
  
}#closing the while loop 


