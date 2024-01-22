package org.goldenport.record.unitofwork

/*
 * @since   Oct. 28, 2023
 * @version Nov.  7, 2023
 * @author  ASAMI, Tomoharu
 */
case class Emulation(
  slots: List[Emulation.Slot] = Nil
) {
  def isEmpty: Boolean = slots.isEmpty

  def apply(): Unit = apply(Emulation.Context.empty)

  def apply(ctx: Emulation.Context): Unit = for (s <- slots) try {
    s.apply(ctx)
  }
}

object Emulation {
  val empty = Emulation()

  case class Context()
  object Context {
    val empty = Context()
  }

  sealed trait Guard {
    def isAccept(ctx: Context): Boolean
  }
  object Guard {
    case object All extends Guard {
      def isAccept(ctx: Context): Boolean = true
    }
  }

  sealed trait Action {
    def apply(ctx: Context): Unit
  }
  object Action {
    case class Raise(e: () => Throwable) extends Action {
      def apply(ctx: Context): Unit = throw e()
    }

    def raise(e: => Throwable) = Raise(() => e)
  }

  case class Slot(guard: Guard, actions: List[Action]) {
    def apply(ctx: Context) =
      if (guard.isAccept(ctx))
        actions.map(_.apply(ctx))
      else
        Unit
  }

  def raise(e: => Throwable): Emulation = Emulation(List(Slot(Guard.All, List(Action.raise(e)))))
}
