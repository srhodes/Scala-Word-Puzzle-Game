/*
* srhodes17@georgefox.edu
* Program 3
* 2019-3-16
* */

package ProgrammingAssignment


// I/O library used to allow the user to insert input
import scala.io.StdIn.readLine
// File library used for accessing the file or preventing file access error
import java.io.{File, FileNotFoundException, PrintWriter}
// I/O library used to read or write to the file
import scala.io.Source
// Utility library used to access random numbers
import scala.util.Random



/**
  * The single object game used to allow player to play the game
  */
object WordPuzzleGame
{

  /* This main function have a Unit type that is not really like a void type
  but it is a type that exist in scala specific to main. Sometime Unit type
  is used in other situation beside main */
  def main(args: Array[String]): Unit =
  {
    // High score variable
    var hsScore: Array[Int] = new Array[Int](45)

    // high score initial will be used after the game
    var hsInitial: Array[String] = new Array[String](45)

    // high score word is the word used by the player to get to the high score
    var hsWord: Array[String] = new Array[String](45)

    // counter should start at 1 otherwise it would access the wrong data in the file
    var counter = 1

    // location of array should start at Zero
    var arrayLoca: Int = 0

    try
    {


    /* The for-loop help access the file to get all of the information about the user
       getting the highest score. It would not record unless the score is high enough
     */
    for(line <- Source.fromFile("highscore.txt").getLines)
    {

      println("LINE should be 3: ")
      println(line)
      println(counter)


      if(counter == 1)
      {
        arrayLoca = line.length() - 1
        hsWord(arrayLoca) = new String
        hsWord(arrayLoca) = line
      }
      else if(counter == 2)
      {
        hsInitial(arrayLoca) = new String
        hsInitial(arrayLoca) = line
      }
      else
      {

        // this line variable is translated into integer
        hsScore(arrayLoca) = line.toInt
        // counter reset back to zero
        counter = 0
      }
      // counter incrementing so that we go through the file
       counter = counter + 1
    }

    }
    catch
    {
      // This try catch exception is used to catch if there is no file
      case ex: FileNotFoundException => {
        println("File not FOUND")
      }
    }

    // In a game or any simulation really while statement is used
while (true)
{
  try
  {
    println()
    println("Welcome to the Word Puzzle game in Scala!")
    println()
    println("*** Word Puzzle ***")
    println("1 - Play a game")
    println("2 - View high scores")
    println("3 - Quit")
    println()
    print("Enter your menu choice [1, 2, 3]:")

    // this function is used to read word from the user
    var input = readLine()



    var chosenWord= new String
    var guessWord = new String

    //insert c
    if (input == "1")
    {

      // The set collection of string is inserted in the set variable
      var set = scala.collection.mutable.Set[String]()
      // The set collection of char is inserted in the setChar variable
      var setChar = scala.collection.mutable.Set[Char]()

      //Source.fromFile("dictionary.txt").foreach{

      // the line variable
      var line ={}

      // The for-loop contains the command to go through the file line-by-line
      for(line <- Source.fromFile("dictionary.txt").getLines)
      {
        //  println(line)
        set.add(line)
      }

      // the function random get to the next integer
      var aNumber = new Random().nextInt(set.size)
      var counter = 0

      // The empty list get inside the word variable
      var word = {}



      // println(aNumber)
      for(word <- set)
      {
        //  println(word)
        if(counter == aNumber)
        {

          chosenWord = word
          // println("This is the chosen word---------------------")
          // println(word)
          // println(chosenWord)

        }
        // increasing the counter in order to insert more
        counter = counter + 1

      }
      //the print function print the chosen word before going to the next line.
      println(chosenWord)

      // go through the chosen
      for(i <- 1 to chosenWord.length())
      {
        guessWord = guessWord + '*'
      }

      var guessCounter = 0


      var charsLeft = chosenWord.length()
      while (charsLeft > 0)
      {

        println(guessWord)
        print("Enter your guess")

        // reading what the user input as guess
        var word = readLine()
        var letter = word(0)
        var duplicate = false

        // going through each letter within a set
        for(letterInSet <- setChar)
        {

          if(letter == letterInSet)
          {
            duplicate = true
            println("duplicate guess Letter---------------")

          }
        }
        if(duplicate == false)
        {
          guessCounter = guessCounter + 1
          setChar.add(letter)

          for(i <- 0 to chosenWord.length() - 1)
          {
            if(chosenWord.charAt(i) == letter){
              var newGuessWord = new String
              for(j <- 0 to guessWord.length() - 1)
              {
                if(j == i){
                  newGuessWord = newGuessWord + letter
                }
                else
                {
                  newGuessWord = newGuessWord + guessWord.charAt(j)
                }
              }
              guessWord = new String
              guessWord = newGuessWord
            }
          }
        }

        charsLeft = 0
        for(j <- 0 to guessWord.length() - 1)
        {
          if(guessWord.charAt(j) == '*')
          {
            charsLeft = charsLeft + 1
          }//end if
        }// end for
        println("This is the char left -------------------")
        println(charsLeft)
      }//

      println("You guessed the word, it was  "+ guessWord)


      if(hsScore(guessWord.length() - 1) == 0 || hsScore(guessWord.length() - 1) > guessCounter)
      {
        // prompt the user to enter its name
        println("You broke the high score for words with "+ guessWord.length + " letters!")
        print("Enter your initials: ")
        var playerInitials = readLine()

        hsWord(guessWord.length - 1) = new String
        hsWord(guessWord.length - 1) = guessWord
        hsInitial(guessWord.length - 1) = new String
        hsInitial(guessWord.length - 1) = playerInitials
        hsScore(guessWord.length - 1) = guessCounter
      }// end if
    }
    else if (input == "2")
    { println("*** High Scores ***")
      println("#  Player  Guesses   Word")
      for(i <- 0 to 44)
      {

        if(hsScore(i) > 0)
        {


          // the number of letter the guessed word have
          printf(s"%-2d ", (i+1))


          // printing the winner initial
          printf(s"%-6s  ", hsInitial(i))

          var guessStr = hsScore(i).toString()

          // printing the high score initial
          printf(s"%-7d   ", hsScore(i))

          println(hsWord(i))
        }

      }
    }
    else if (input == "3") // User selection is 3 meaning he wants to exit
    {
      val writer = new PrintWriter(new File("highscore.txt"))

      // for loop used for traversal
      for(i <- 0 to 44)
      {

        if (hsScore(i) > 0)
        {
          writer.println(hsWord(i))
          writer.println(hsInitial(i))
          writer.println(hsScore(i))
        }
      }

      // Close the file after writing in it
      writer.close()

      // The system function is used to exit the program
      sys.exit()
    }
    else
    {
      println("Invalid option,  Try again.")
    }
  }
  catch
  {
      // This try catch statement is used if the user inputted the wrong character
      case ex: NumberFormatException =>
      {
        println("Invalid option,  Try again.")

      }
  }
}


  }
}
