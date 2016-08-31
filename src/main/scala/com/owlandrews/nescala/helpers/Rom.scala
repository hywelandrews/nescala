package com.owlandrews.nescala.helpers

import java.awt.Image

import scala.util.Try

case class Rom(title:String, thumbnail:Image, path:String)

object Rom {
  private val applicationFolder = File.ApplicationFolder
  private val nodes = File.Xml("/database.xml") \\ "game"
  private lazy val boxArt = applicationFolder.listFiles()
  private lazy val defaultIcon = File.Image("/cartridge.png").getOrElse(throw new Exception("Unable to load resource cartridge.png"))
  private lazy val url = "https://s3.amazonaws.com/nescala/boxart/"

  def apply(crc:String, filename:String, filePath:String, thumbnail:Thumbnail):Rom = {

    val node = nodes filter (_ \\ "@crc" exists (_.text.contains(crc)))

    if (node.nonEmpty) {
      val name = node.head \@ "name"
      val imageFile = name.replace(" ", "+").replace("'", "%27") + ".jpg"
      val imagePath = s"${applicationFolder.getAbsolutePath}/$imageFile"
      val icon = boxArt.find(_.getName == imageFile)
        .map(x => File.Image(Try(x)))
        .getOrElse(File.Image(File.Download(url + imageFile, imagePath)))

      icon.map(i => Rom(name, thumbnail.Resize(i), filePath))
        .getOrElse(Rom(filename, thumbnail.Resize(defaultIcon), filePath))
    } else Rom(filename, thumbnail.Resize(defaultIcon), filePath)
  }
}
