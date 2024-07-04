### ANGELA BAKAJ
### BTC1855 ASSIGNMENT 3

#### REVIEW BY CRYSTAL LEE - COMMENTS MARKED BY "CL"
#### I commented out some of the original code and replaced it with corrections
#' just to have the game continue running for the review - CL

# Read the word list from the file
# User: setwd!!!
# sw message: !!!

# SUGGESTION - Make sure dictionary.txt is in the repo, or else the user wouldn't
# have access to the list - CL
# I uploaded my own txt file to the review repo to continue the review - CL

dictionary <- suppressWarnings(readLines("assign3-wordlist.txt"))


# Selecting a random word from the dictionary:
secret_word <- sample(dictionary, 1)

# Specifying the number of characters that the secret word contains:
secret_word_length <- nchar(secret_word) 

cat("The secret word has", secret_word_length, "letters.\n")

# Initializing the number of attempts the user has:
max_attempts <- 6
cat("You have", max_attempts, "wrong attempts allowed.\n")

# Checking if the input is a single letter:
# Setting this check as a function itself:
is_valid_input <- function(input) {
  return(grepl("^[a-zA-Z]$", input))
}

# Setting up the game variables:
# Ensuring the specify the number of attempts:
attempts <- 0
correct_guesses <- rep("_", secret_word_length)
incorrect_guesses <- character()

while (attempts < max_attempts) {
  cat("Current word: ", paste(correct_guesses, collapse = " "), "\n")
  guess <- tolower(readline(prompt = "Enter a letter: "))
  
  if (!is_valid_input(guess)) {
    cat("Invalid input. Please enter a single letter.\n")
    next
  }
  
  if (guess %in% correct_guesses || guess %in% incorrect_guesses) {
    cat("You have already guessed that letter.\n")
    next
  }
  
  if (grepl(guess, secret_word)) {
    cat("Correct guess!\n")
    positions <- which(strsplit(secret_word, "")[[1]] == guess)
    correct_guesses[positions] <- guess
  } else {
    cat("Wrong guess.\n")
    incorrect_guesses <- c(incorrect_guesses, guess)
    attempts <- attempts + 1
  }
  
  if (all(correct_guesses != "_")) {
    cat("Congratulations! You've guessed the word:", secret_word, "\n")
    break
  }
  
  # SUGGESTION: There is no variable called "wrong_guesses", try "incorrect_guesses" - CL
  # cat("incorrect guesses:", paste(wrong_guesses, collapse = " "), "\n") # ORIGINAL CODE
  cat("incorrect guesses:", paste(incorrect_guesses, collapse = " "), "\n") # CORRECTED CODE
  cat("Remaining tries:", max_attempts - attempts, "\n")
}

# SUGGESTION: There is no variable called "tries", try "attempts" - CL
# if (tries == max_attempts) { # ORIGINAL CODE
if (attempts == max_attempts) { # CORRECTED CODE
  cat("You've exhausted your attempts. The secret word was:", secret_word, "\n")
}

#### CODE REVIEW SUMMARY - CL

# FUNCTIONALITY: Code generally runs well except for the sections where the wrong variable is used
# There is also a missing .txt file in your repo so ensure that all necessary files are included 
# or provide clear instructions on how to obtain them
# I suggest running your entire code at the end to check using "Source"
# Good job including a masked word as added functionality! 

# STRUCTURE/LOGIC: Good structure and logic. Starts with loading and reading .txt file, 
# initializing attempts, setting conditions, and ending game when attempts are exhausted
# Conditions are correctly set and attempts are decreased appropriately with wrong guesses

# DESIGN: Well-organized and easy to understand. I suggest encapsulating your game code into 
# a function so the internal computations/running code isn't seen in the console, just the output.
# It makes it especially easier to read the instructions at the beginning of the game.

# READABILITY: Include more comments within your game code that explains some of the decisions
# you made e.g., what is "positions <- which(strsplit(secret_word, "")[[1]] == guess)" and why
# did you do it this way?

