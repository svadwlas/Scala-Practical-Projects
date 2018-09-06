

import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn.readLine
import CoinFlipUtils._

case class GameState(numFlips:Int, numCorrect:Int)
object CoinFlipUtils {
def showPrompt(): Unit = {
  print("\n(h)eads, (t)ails, or (q)uit: ")
}


def getUserInput():String  = readLine.trim.toUpperCase


def printableFlipResult(flip: String): String = flip match {
  case "H" => "Heads"
  case "T" => "Tails"
}


def printGameState(printableFlipResult: String, gameState: GameState): Unit = {
  print(s"Flip was $printableFlipResult. ")
  printGameState(gameState)

}

def printGameState(gameState: GameState): Unit = {

  println(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect} ")

}

def printGameOver(): Unit = println("\n=== GAME OVER ===")


  def tossCoin(r: Random): String = {

    val i = r.nextInt(2)
    i match {

      case 0 => "H"
      case 1 => "T"
    }

  }
}




object CoinFlip extends App {

  val r = Random
  val s = GameState(0, 0)
  mainLoop(s, r)


  @tailrec
  def mainLoop(gameState: GameState, random: Random): Unit ={



    showPrompt()

    val userInput = getUserInput()


    userInput match {
      case "H" | "T" => {

        val coinTossResult = tossCoin(random)


        val newNumFlips = gameState.numFlips + 1

        if (userInput == coinTossResult){

          val newGameState = gameState.copy(numFlips =  newNumFlips, numCorrect = gameState.numCorrect + 1 )
          printGameState(printableFlipResult(coinTossResult), newGameState)
          mainLoop(newGameState, random)

        }
        else {
          val newGameState = gameState.copy(numFlips = newNumFlips)
          printGameState(printableFlipResult(coinTossResult), newGameState)
          mainLoop(newGameState, random)
        }


      }
      case _ => {
        printGameOver()
        printGameState(gameState)
      }







    }


  }




}




