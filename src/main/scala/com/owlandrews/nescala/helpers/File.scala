package com.owlandrews.nescala.helpers

import com.owlandrews.nescala.Console

object File {
  import java.io.File
  import java.net.URL
  import java.io.{FileFilter, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
  import javax.imageio.ImageIO

  import scala.util.Try
  import scala.xml.XML
  import scala.language.postfixOps

  import sys.process._

  import com.typesafe.config.ConfigFactory

  def Download(url: String, filename: String) = (for{
    url <- Try(new URL(url))
    conn <- Try(url.openConnection().connect())
    file <- Try(new File(filename))
  } yield Try(url  #> file !!)) map {x => new File(filename)}

  def Writer(filename: String)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(new File(filename))
    try op(p)
    finally p.close()
  }

  def Write(filename: String, content: String) = {
    val res = new java.io.PrintWriter(new File(filename))
    res.write(content)
    res.close()
  }

  def Filter = new FileFilter {
    override def accept(pathname: File): Boolean = pathname.getName.toLowerCase.endsWith(".nes")
  }

  def Image(file:Try[File]) = file.map(ImageIO.read)

  def Image(filename:String) = Try(ImageIO.read(resource(filename)))

  def Xml(filename:String) = XML.load(resource("/database.xml"))

  def Config(filename:String) = {
    val file = new File(filename)
    file.exists() match {
      case true => ConfigFactory.parseFile(file)
      case false => ConfigFactory.empty()
    }
  }

  def SaveState(console:Console) = {
    val fos = new FileOutputStream(s"$ApplicationFolder/${console.cartridge.CRC}.save")
    val oos = new ObjectOutputStream(fos)

    oos.writeObject(console)
    oos.close()
  }

  def LoadState(crc:String):Try[Console] = Try {
    val fis = new FileInputStream(s"$ApplicationFolder/$crc.save")
    val ois = new ObjectInputStreamWithCustomClassLoader(fis)

    val console = ois.readObject.asInstanceOf[Console]
    ois.close()
    console
  }

  // Taken from: https://gist.github.com/ramn/5566596
  private class ObjectInputStreamWithCustomClassLoader(fileInputStream: FileInputStream) extends ObjectInputStream(fileInputStream) {
    override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
      try { Class.forName(desc.getName, false, getClass.getClassLoader) }
      catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
    }
  }

  lazy val ApplicationFolder: File = {
    val settingDirectory = System.getProperty("user.home") + "/.nescala"
    val settings = new java.io.File(settingDirectory)
    if (!settings.exists()) settings.mkdir()
    settings
  }

  private def resource(filename:String) = getClass.getResourceAsStream(filename)
}
