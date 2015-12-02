package ui

import java.awt.{BorderLayout, Canvas}
import java.util.prefs.Preferences
import javax.swing._
import javax.swing.filechooser.FileFilter

import org.lwjgl.LWJGLException
import org.lwjgl.opengl.Display

import scala.swing.{Action, _}

object Run extends SimpleSwingApplication {

  import java.io.File

  implicit val macroGl = org.macrogl.Macrogl.default

  private val scale = 2
  private val initialWindowSize = new Dimension(256 * scale, 240 * scale)
  private var filePath:Option[String] = None
  private var gameThread:Option[Thread] = None
  private val director:Director = new Director(canvas, new Audio)
  private val preferences = Preferences.userNodeForPackage(this.getClass)
  private val lastDirectorySetting = "LAST_OUTPUT_DIR"

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  //Init Mac OSX native menu
  System.setProperty("apple.laf.useScreenMenuBar", "true")
  val app = com.apple.eawt.Application.getApplication
  app.setDefaultMenuBar(mainMenuBar)

  lazy val top = new MainFrame {
    title = s"${nescala.BuildInfo.name} ${nescala.BuildInfo.version}"
    minimumSize = new Dimension(256, 240)
    size = initialWindowSize

    peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    peer.setFocusable(true)
    peer.setLocationRelativeTo(null)
    peer.setIgnoreRepaint(true)
  }

  def StepFrame(image:Image) = {
    val panel = new JPanel()
    panel.add(new JLabel(new ImageIcon(image)))

    val scrollPane = new JScrollPane(panel)
    JOptionPane.showMessageDialog(null, scrollPane)
  }

  private lazy val resetMenuItem = new MenuItem(Action("Reset")(director.Reset)) {
    enabled = false
  }

  private lazy val mainMenuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Load Rom")(openFileDialog.foreach(addDisplay)))
      contents += resetMenuItem
    }
  }.peer

  private lazy val canvas = new Canvas {
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

  private def addDisplay(path:String) = {
    top.peer.remove(canvas)
    filePath = Some(path)
    top.peer.add(canvas, BorderLayout.CENTER)
    top.peer.pack()
    canvas.requestFocus()
    resetMenuItem.enabled = true
  }

  private def openFileDialog = {
    val fileChooser = new FileChooser(){
      fileHidingEnabled = true
      title = "Select Rom File"
      fileFilter = new FileFilter {
        override def getDescription: String = ".nes"

        override def accept(file: File): Boolean = file.getName.toLowerCase.endsWith(".nes")
      }
      fileSelectionMode = FileChooser.SelectionMode.FilesAndDirectories
      peer.setCurrentDirectory(new File(preferences.get(lastDirectorySetting, "user.home")))
    }

    if(fileChooser.showOpenDialog(null) == FileChooser.Result.Approve){
      preferences.put(lastDirectorySetting, fileChooser.selectedFile.getAbsolutePath)
      Option(fileChooser.selectedFile.getPath)
    }
    else None
  }
}