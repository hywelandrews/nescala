package helpers

object Unsigned {

  sealed trait UType

  case object uByte extends UType

  case object uShort extends UType

  case object uInt extends UType

  case object uLong

  trait UnsignedConverter[A] extends Any {
    def uInt(a: A): A
    def uByte(a: A): A
    def uShort(a:A):A
  }

  trait UnsignedConverterLong[A] extends UnsignedConverter[A] {
    def uLong(a: A): A
  }

  implicit val IntUnsignedOps = new UnsignedConverter[Int] {
    override def uByte(a: Int): Int = a & 0xFF

    override def uShort(a: Int): Int = a & 0xFFFF

    override def uInt(a: Int): Int = a & 0xFFFFFFFF
  }

  implicit val LongUnsignedOps = new UnsignedConverterLong[Long] {
    override def uLong(a: Long): Long = a & 0xFFFFFFFFFFFFFFFFL
    override def uInt(a: Long): Long = a & 0xFFFFFFFF
    override def uByte(a: Long): Long = a & 0xFF
    override def uShort(a: Long): Long = a & 0xFFFF
  }

  implicit class UnsignedIntOps[A](val lhs: Int) extends AnyVal {
    type A = Int
    def as[T >: UType](rhs: T)(implicit ev: UnsignedConverter[Int]): Int = rhs match {
      case `uByte`  => ev.uByte(lhs)
      case `uShort` => ev.uShort(lhs)
      case `uInt`   => ev.uInt(lhs)
    }
  }

  implicit class UnsignedLongOps[A](val lhs: Long) extends AnyVal {
    type A = Long
    def as[T >: uLong.type](rhs: T)(implicit ev: UnsignedConverterLong[Long]): Long = rhs match {
      case `uLong`  => ev.uLong(lhs)
      case `uInt`   => ev.uInt(lhs)
      case `uByte`  => ev.uByte(lhs)
      case `uShort` => ev.uShort(lhs)
    }
  }
}

