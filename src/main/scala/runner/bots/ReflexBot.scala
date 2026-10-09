package runner.bots

import runner.*

class ReflexBot(seed: Long = 202L) extends CyberBot {

  override def name: String = "ReflexBot"

  private def validateMove(currentLane: Lane, desiredAction: Action): Action = {
    (currentLane, desiredAction) match {
      case (Lane.Left, Action.MoveLeft)   => Action.KeepRunning
      case (Lane.Right, Action.MoveRight) => Action.KeepRunning
      case (_, validAction)               => validAction
    }
  }

  override def decide(observation: Observation): Action = {
    val nextSlice = observation.upcoming.headOption
    if (nextSlice.isEmpty) return Action.KeepRunning

    val currentLane = observation.hero.lane
    val slice = nextSlice.get

   
    val hasLow = slice.itemAt(currentLane, Height.Low) != CellItem.Empty
    val hasMid = slice.itemAt(currentLane, Height.Mid) != CellItem.Empty
    val hasHigh = slice.itemAt(currentLane, Height.High) != CellItem.Empty

    val desiredAction = (hasLow, hasMid, hasHigh) match {
      case (true, _, true) =>
        if (currentLane == Lane.Left) Action.MoveRight
        else if (currentLane == Lane.Right) Action.MoveLeft
        else Action.MoveRight

      case (true, _, _) => Action.Jump         
      case (_, true, _) => Action.Duck          
      case (_, _, true) => Action.KeepRunning   
      case _            => Action.KeepRunning   
    }

    validateMove(currentLane, desiredAction)
  }
}