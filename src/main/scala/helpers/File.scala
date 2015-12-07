package helpers

import java.io.FileFilter
import javax.imageio.ImageIO

import com.typesafe.config.ConfigFactory

import scala.util.Try
import scala.xml.XML

object File {
  import java.io.File
  import java.net.URL

  import sys.process._

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

  lazy val ApplicationFolder: File = {
    val settingDirectory = System.getProperty("user.home") + "/.nescala"
    val settings = new java.io.File(settingDirectory)
    if (!settings.exists()) settings.mkdir()
    settings
  }

  private def resource(filename:String) = getClass.getResourceAsStream(filename)
}
