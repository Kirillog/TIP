package tip.lattices

object VarSizeLattice extends LatticeWithOps {

  sealed trait VarSizeElement
  case object Bottom     extends VarSizeElement { override def toString = "⊥"       }
  case object BoolSize   extends VarSizeElement { override def toString = "bool"     }
  case object ByteSize   extends VarSizeElement { override def toString = "byte"     }
  case object CharSize   extends VarSizeElement { override def toString = "char"     }
  case object IntSize    extends VarSizeElement { override def toString = "int"      }
  case object BigIntSize extends VarSizeElement { override def toString = "bigint"   }
  case object AnySize    extends VarSizeElement { override def toString = "any"      }

  type Element = VarSizeElement

  override val bottom: Element = Bottom
  override val top: Element    = AnySize

  def lub(x: Element, y: Element): Element = (x, y) match {
    case (AnySize, _) | (_, AnySize)                 => AnySize
    case (Bottom, a)                                 => a
    case (a, Bottom)                                 => a
    case (a, b) if a == b                            => a
    case (BoolSize, a)                               => a
    case (a, BoolSize)                               => a
    case (ByteSize, CharSize) | (CharSize, ByteSize) => IntSize
    case _                                           => BigIntSize
  }

  private val INF = 2 * Int.MaxValue.toLong

  private def lo(t: Element): Long = t match {
    case Bottom     => Long.MaxValue
    case BoolSize   => 0L
    case ByteSize   => -128L
    case CharSize   => 0L
    case IntSize    => Int.MinValue.toLong
    case BigIntSize => -INF
    case AnySize    => -INF
  }

  private def hi(t: Element): Long = t match {
    case Bottom     => Long.MinValue
    case BoolSize   => 1L
    case ByteSize   => 127L
    case CharSize   => 65535L
    case IntSize    => Int.MaxValue.toLong
    case BigIntSize => INF
    case AnySize    => INF
  }

  private def fromRange(l: Long, h: Long): Element = {
    if      (l > h)                                                    Bottom
    else if (l >= 0L    && h <= 1L)                                    BoolSize
    else if (l >= -128L && h <= 127L)                                  ByteSize
    else if (l >= 0L    && h <= 65535L)                                CharSize
    else if (l >= Int.MinValue.toLong && h <= Int.MaxValue.toLong)     IntSize
    else                                                               BigIntSize
  }

  def num(i: Int): Element = fromRange(i.toLong, i.toLong)

  def plus(a: Element, b: Element): Element = (a, b) match {
    case (Bottom, _) | (_, Bottom)   => Bottom
    case (AnySize, _) | (_, AnySize) => AnySize
    case _                           => fromRange(lo(a) + lo(b), hi(a) + hi(b))
  }

  def minus(a: Element, b: Element): Element = (a, b) match {
    case (Bottom, _) | (_, Bottom)   => Bottom
    case (AnySize, _) | (_, AnySize) => AnySize
    case _                           => fromRange(lo(a) - hi(b), hi(a) - lo(b))
  }

  def times(a: Element, b: Element): Element = (a, b) match {
    case (Bottom, _) | (_, Bottom)   => Bottom
    case (AnySize, _) | (_, AnySize) => AnySize
    case _ =>
      val ps = Seq(lo(a) * lo(b), lo(a) * hi(b), hi(a) * lo(b), hi(a) * hi(b))
      fromRange(ps.min, ps.max)
  }

  def div(a: Element, b: Element): Element = (a, b) match {
    case (Bottom, _) | (_, Bottom)   => Bottom
    case (AnySize, _) | (_, AnySize) => AnySize
    case _ =>
      val m = math.max(math.abs(lo(a)), hi(a))
      fromRange(-m, m)
  }

  def eqq(a: Element, b: Element): Element = (a, b) match {
    case (Bottom, _) | (_, Bottom) => Bottom
    case _                         => BoolSize
  }

  def gt(a: Element, b: Element): Element = (a, b) match {
    case (Bottom, _) | (_, Bottom) => Bottom
    case _                         => BoolSize
  }
}

