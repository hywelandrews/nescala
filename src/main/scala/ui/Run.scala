package ui

import java.awt.{BorderLayout, Canvas}
import javax.swing._
import javax.swing.border.EmptyBorder
import javax.swing.filechooser.FileFilter

import helpers.{File, Settings}
import org.lwjgl.LWJGLException
import org.lwjgl.opengl.Display

import scala.swing.{Action, Dimension, MenuBar, _}

object Run extends SimpleSwingApplication {

  private val scale = 2
  private val initialWindowSize = new Dimension(256 * scale, 240 * scale)
  private var filePath:Option[String] = None
  private var gameThread:Option[Thread] = None
  private val director:Director = new Director(gameCanvas, gameLibrary, new Audio)
  private def mediumGrey = new swing.Color(52,61,70)

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  override def startup(args: Array[String]) = {
    super.startup(args)
    director.Menu
  }

  lazy val top = new MainFrame {
    title = s"${nescala.BuildInfo.name} ${nescala.BuildInfo.version}"
    minimumSize = new Dimension(256, 240)
    size = initialWindowSize
    contents = scrollWrapper
    peer.setFocusable(true)
    peer.setLocationRelativeTo(null)
    peer.add(gameMenu.peer, java.awt.BorderLayout.SOUTH)
    pack()

    override def closeOperation() {
      Settings.close()
      super.closeOperation()
    }
  }

  private lazy val scrollWrapper = new ScrollPane {
    contents = gameLibrary
    preferredSize = initialWindowSize
    border = new EmptyBorder(0, 0, 0, 0)
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
  }

  private lazy val gameLibrary: WrapPanel = new WrapPanel(initialWindowSize.getWidth.toInt) {
    visible = true
    border = new EmptyBorder(5, 5, 10, 5)
    background = mediumGrey
  }

  private lazy val gameMenu: MenuBar = new MenuBar {
    opaque = true
    border = new EmptyBorder(0, 5, 0, 15)
    background = mediumGrey
    preferredSize = new swing.Dimension(initialWindowSize.getWidth.toInt, 32)
    contents += gameMenuIcon("eject", Action("")(Eject))
    contents += gameMenuIcon("reload", Action("")(director.Reset))
    contents += gameMenuIcon("play", Action("")(director.Resume))
    contents += gameMenuIcon("pause", Action("")(director.Pause))
    contents += separator
    contents += gameMenuIcon("document_add", Action("")(openFileDialog.foreach(StartDisplay)), active = true)
    contents += gameMenuIcon("folder_add", Action("")(OpenFolderDialog), active = true)
    visible = true
  }

  private val consoleControlNames = List("eject", "reload", "play", "pause")

  private def consoleControls(enable:Boolean) =
    gameMenu.contents.filter(x => consoleControlNames.contains(x.name)).foreach(_.enabled = enable)

  private def gameMenuIcon(path:String, gameAction: Action, active: Boolean = false) = new Button {
    name = path
    opaque = false
    border = new EmptyBorder(1, 1, 1, 1)
    horizontalAlignment = Alignment.Center
    action = gameAction
    icon = File.Image(s"/$path.png").map(new ImageIcon(_)).getOrElse(throw new Exception(s"Error loading $path icon"))
    enabled = active
  }

  private val separator = new Separator {
    maximumSize = new Dimension(initialWindowSize.getWidth.toInt, 0)
    opaque = true
    background = mediumGrey
  }

  private lazy val gameCanvas = new Canvas {
    setSize(initialWindowSize)
    setFocusable(true)
    setIgnoreRepaint(true)

    override def addNotify() = {
      super.addNotify()
      gameThread = filePath.map{ path => attachDisplay(this, path)}
      gameThread.foreach(t => t.start())
      filePath = None
    }

    override def removeNotify() = {
      detachDisplay()
      super.removeNotify()
    }

    def attachDisplay(frame: Canvas, path:String) = new Thread {
         override def run() = {
            try {
              Display.setParent(frame)
              Display.create()
              director.Start(path)
            } catch {
              case e: LWJGLException => e.printStackTrace()
            }
        }
    }

    def detachDisplay():Unit = {
      director.Close
      gameThread.foreach(t => t.join())
    }
  }

  def StepFrame(image:Image) = {
    val panel = new JPanel()
    panel.add(new JLabel(new ImageIcon(image)))

    val scrollPane = new JScrollPane(panel)
    JOptionPane.showMessageDialog(null, scrollPane)
  }

  def Eject = {
    top.peer.remove(gameCanvas)
    top.peer.add(scrollWrapper.peer)
    top.peer.pack()
    scrollWrapper.peer.requestFocus()
    consoleControls(enable = false)
    director.Menu
  }

  def StartDisplay(path:String) = {
    filePath = Some(path)
    top.peer.remove(scrollWrapper.peer)
    top.peer.add(gameCanvas, BorderLayout.CENTER)
    top.peer.pack()
    gameCanvas.requestFocus()
    consoleControls(enable = true)
  }

  def OpenFolderDialog = {
    val fileChooser = new FileChooser(){
      title = "Select Game Library"
      fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      peer.setCurrentDirectory(new java.io.File(Settings.lastFileSelectDirectory))
    }

    if(fileChooser.showOpenDialog(null) == FileChooser.Result.Approve){
      Settings.gameLibrary = fileChooser.selectedFile.getAbsolutePath
      director.Reset
    }
  }

  private def openFileDialog = {
    import java.io.File
    val fileChooser = new FileChooser(){
      fileHidingEnabled = true
      title = "Select Rom File"
      fileFilter = new FileFilter {
        override def getDescription: String = ".nes"

        override def accept(file: File): Boolean = file.getName.toLowerCase.endsWith(".nes")
      }
      fileSelectionMode = FileChooser.SelectionMode.FilesAndDirectories
      peer.setCurrentDirectory(new File(Settings.lastFileSelectDirectory))
    }

    if(fileChooser.showOpenDialog(null) == FileChooser.Result.Approve){
      Settings.lastFileSelectDirectory = fileChooser.selectedFile.getAbsolutePath
      Option(fileChooser.selectedFile.getPath)
    } else None
  }
}