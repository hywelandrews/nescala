package ui

import java.awt.{BorderLayout, Canvas}
import javax.swing._
import javax.swing.border.EmptyBorder
import javax.swing.filechooser.FileFilter

import scala.swing.{Action, Dimension, Menu, MenuBar, MenuItem, _}

import org.lwjgl.LWJGLException
import org.lwjgl.opengl.Display

import helpers.Settings

object Run extends SimpleSwingApplication {

  private val scale = 2
  private val initialWindowSize = new Dimension(256 * scale, 240 * scale)
  private var filePath:Option[String] = None
  private var gameThread:Option[Thread] = None
  private val director:Director = new Director(gameCanvas, menuPanel, new Audio)

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  //TODO: Move init Mac OSX native menu
  System.setProperty("apple.laf.useScreenMenuBar", "true")
  val app = com.apple.eawt.Application.getApplication
  app.setDefaultMenuBar(mainMenuBar)

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
    pack()

    override def closeOperation() {
      Settings.close()
      super.closeOperation()
    }
  }

  private lazy val resetMenuItem = new MenuItem(Action("Reset")(director.Reset)) {
    enabled = false
  }

  private lazy val mainMenuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Load Rom")(openFileDialog.foreach(StartDisplay)))
      contents += resetMenuItem
    }
  }.peer

  private lazy val scrollWrapper = new ScrollPane {
    contents = menuPanel
    preferredSize = initialWindowSize
    border = new EmptyBorder(0, 0, 0, 0)
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
  }

  private lazy val menuPanel: WrapPanel = new WrapPanel(initialWindowSize.getWidth.toInt) {
    visible = true
    border = new EmptyBorder(5, 5, 10, 5)
    background = new swing.Color(52,61,70)
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
              resetMenuItem.enabled = true
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

  def StartDisplay(path:String) = {
    filePath = Some(path)
    top.peer.remove(scrollWrapper.peer)
    top.peer.add(gameCanvas, BorderLayout.CENTER)
    top.peer.pack()
    gameCanvas.requestFocus()
    resetMenuItem.enabled = true
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