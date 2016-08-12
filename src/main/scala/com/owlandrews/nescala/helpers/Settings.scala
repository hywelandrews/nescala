package com.owlandrews.nescala.helpers

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}

import scala.swing.event.Event
import scala.util.Try

object Settings extends swing.Publisher with AutoCloseable {

  abstract class PreferenceChanged extends Event

  class LastFileSelectDirectory extends PreferenceChanged

  class GameLibrary extends PreferenceChanged

  private val configFile = File.ApplicationFolder.getAbsolutePath + "/nescala.json"
  private val config = File.Config(configFile)
  private lazy val appliedConfigs = config.root().unwrapped()

  def lastFileSelectDirectory: String = appliedConfigs.getOrDefault("lastFileSelectDirectory", ConfigValueFactory.fromAnyRef(System.getProperty("user.home"))).toString

  def lastFileSelectDirectory_=(path:String): Unit = {
    appliedConfigs.put("lastFileSelectDirectory", ConfigValueFactory.fromAnyRef(path))
    publish(new LastFileSelectDirectory)
  }

  def gameLibrary: Option[String] = Try(appliedConfigs.get("gameLibrary").toString).toOption

  def gameLibrary_=(path: String): Unit = {
    appliedConfigs.put("gameLibrary", path)
    publish(new GameLibrary)
  }

  final def close() = {
    val config = ConfigFactory.parseMap(appliedConfigs)
    File.Write(configFile, config.root.render(com.typesafe.config.ConfigRenderOptions.concise().setJson(true)))
  }
}