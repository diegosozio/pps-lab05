package polyglot.a05b

import scala.util.Random

class LogicsImpl(size: Int) extends Logics {

  // Posizione iniziale di un elemento
  private var elementPosition: (Int, Int) = (Random.nextInt(size), Random.nextInt(size))

  // Flag per controllare se il gioco è finito
  private var gameOver = false

  // Metodo per avanzare la logica del gioco
  override def tick(): Unit = {
    // L'elemento si muove solo se il gioco non è finito
    if (!gameOver) {
      elementPosition = (Random.nextInt(size), Random.nextInt(size))
    }
  }

  // Metodo per verificare se il gioco è finito
  override def isOver(): Boolean = gameOver

  // Metodo per verificare se c'è un elemento nella posizione specificata
  override def hasElement(x: Int, y: Int): Boolean = {
    elementPosition == (x, y)
  }

  // Metodo per verificare se la cella selezionata è la stessa dell'elemento (vittoria)
  def selectCell(x: Int, y: Int): Unit = {
    // Controlla se la selezione coincide con la posizione dell'elemento
    if (elementPosition == (x, y)) {
      // Se la cella selezionata contiene l'elemento, hai vinto
      println("you win")
      gameOver = true
    } else {
      // Se non hai vinto, aggiorna la posizione dell'elemento
      tick()  // Muovi l'elemento solo se non hai vinto
    }
  }
}