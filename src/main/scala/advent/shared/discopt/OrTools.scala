package advent.shared.discopt

object OrTools {
  private lazy val loadLib: Unit = System.loadLibrary("jniortools")

  def ensureLoaded(): Unit = loadLib
}
