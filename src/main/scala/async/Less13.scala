package async

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object Less13 extends App {

  /*
Make this work correctly a) first with synchronized blocks, b) then with AtomicReference
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val tasksCount     = 100
  val taskIterations = 1000
  val initialBalance = 10

  // PLACE TO FIX - START
  var balance1: Int = initialBalance
  var balance2: Int = initialBalance

  //synchronized blocks
  def doTaskIteration(): Unit = synchronized {
    val State(newBalance1, newBalance2) = transfer(State(balance1, balance2))
    balance1 = newBalance1
    balance2 = newBalance2
  }

  //AtomicReference
  val atomRefState = new AtomicReference[State]()
  atomRefState.set(State(balance1, balance2))

  def doTaskIteration2(): Unit = {
    atomRefState.getAndUpdate(transfer(_))
  }

  def printBalancesSum(): Unit = {
    println(balance1 + balance2)
  }
  // PLACE TO FIX - FINISH

  def transfer(state: State): State = {
    if (state.balance1 >= state.balance2) {
      State(state.balance1 - 1, state.balance2 + 1)
    } else {
      State(state.balance1 + 1, state.balance2 - 1)
    }
  }

  val tasks = (1 to tasksCount).toVector.map(_ =>
    Future {
      (1 to taskIterations).foreach(_ => doTaskIteration2()) //switch to test
    }
  )
  val tasksResultFuture: Future[Vector[Unit]] = Future.sequence(tasks)
  Await.ready(tasksResultFuture, 5.seconds)

  printBalancesSum() //should print 20

  final case class State(balance1: Int, balance2: Int)
}

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

/** Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object AsyncHomework {

  import cats.syntax.traverse._

  implicit private val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def main(args: Array[String]): Unit = {
    val res = for {
      body     <- fetchPageBody(args.head)
      links    <- findLinkUrls(body)
      initList <- links.map(fetchServerName).sequence
      resList =
        initList
          .filter(_.isDefined)
          .map(_.get)
          .distinct
          .sorted
    } yield resList
    res.onComplete {
      case Failure(exception) => exception.printStackTrace()
      case Success(value)     => println(value)
    }
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
