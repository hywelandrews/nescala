package com.owlandrews.nescala.helpers

object Unsigned {

  sealed trait UType {
    val mask: Int
  }

  case object uByte extends UType {
    val mask = 0xFF
  }

  case object uShort extends UType {
    val mask = 0xFFFF
  }

  case object uInt extends UType {
    val mask = 0xFFFFFFFF
  }

  case object uLong {
    val mask = 0xFFFFFFFFFFFFFFFFL
  }

  implicit class UnsignedIntOps(val lhs: Int) extends AnyVal {
     def as[T >: UType](rhs: T): Int = rhs match {
      case `uByte`  => lhs & uByte.mask
      case `uShort` => lhs & uShort.mask
      case `uInt`   => lhs & uInt.mask
    }
  }

  implicit class UnsignedLongOps(val lhs: Long) extends AnyVal {
     def as[T >: uLong.type](rhs: T): Long = rhs match {
      case `uByte`  => lhs & uByte.mask
      case `uShort` => lhs & uShort.mask
      case `uInt`   => lhs & uInt.mask
      case `uLong`  => lhs & uLong.mask
    }
  }
}

