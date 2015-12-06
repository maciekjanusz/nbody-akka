import akka.actor.{Props, ActorRef, ActorLogging, Actor}

class BodySystem(params: SystemParameters) extends Actor with ActorLogging {
  import params._

  var bodies = Seq.empty[ActorRef]
  var nReady = 0
  var nFinished = 0

  def startSimulation() {
    val range = 0 until n

    // spawn actors
    range foreach { k =>
      bodies :+= context.watch {
        context.actorOf(Props(classOf[Body],
          BodyParameters(k, 1), params), name = "body-" + k)
      }
    }

    // send peer lists to actors
    bodies foreach { body =>
      body ! ActorRefSeq(bodies.filter { b => b != body })
    }
  }

  override def receive: Receive = {
    case Start =>
      log.info("Starting simulation...")
      startSimulation()

    case Ready =>
      nReady += 1
      if(nReady == n) {
        bodies foreach { body =>
          body ! Start
        }
      }

    case Finished =>
      nFinished += 1
      if(nFinished == n) {
        context.stop(self)
        log.info("Finished!")
      }
  }
}