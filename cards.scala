package info.cmlubinski.cards

import scala.util.Random

case class Card(suit:Char, face:String, points:Int)

class Deck {
  /**  We use a list as a stack for the cards; this allows for quickly grabbing
   *  the first element
   */
  private var cards = Deck allCards

  def shuffle() = {
    cards = Random.shuffle(cards)
    this
  }
  //  This will return None if the deck is empty, Some if there is a card
  def takeTop():Option[Card] = cards match {
    case head :: tail => {
      cards = tail
      Some(head)
    }
    case Nil => None
  }
}

object Deck {
  lazy val suits = List('H', 'D', 'C', 'S')
  lazy val allCards = 
    //  Aces
    (for (suit <- suits) yield Card(suit, "A", 15)) ++
    //  Face Cards
    (for (suit <- suits; card <- List("J", "Q", "K")) 
      yield Card(suit, card, 10)) ++
    //  Number cards
    (for (suit <- suits; card <- 2 to 10)
      yield Card(suit, card.toString, card))
}

class Hand {
  private var myCards = List[Card]()
  //  returns an option containing this if this was a success
  def takeCard(deck:Deck):Option[Hand] = {
    deck takeTop() match {
      case None => None
      case Some(card) => {
        myCards = card :: myCards
        Some(this)
      }
    }
  }
  def sort() = {
    myCards = myCards.sortWith((lhs, rhs) => {
      //  First compare suit
      if (lhs.suit != rhs.suit) {
        (lhs.suit, rhs.suit) match {
          //  Hearts before Diamonds before Clubs before Spades
          case ('H', _) | ('D', 'C') | ('D', 'S') | ('C', 'S') => true
          case _ => false
        }
      } else {  //  suit matches, so we must look at the point value
        lhs.points < rhs.points
      }
    })
    this
  }
  //  We permit read only access
  def cards = myCards
}
