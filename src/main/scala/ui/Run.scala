package ui

import java.awt.{BorderLayout, Canvas}
import javax.swing._
import javax.swing.filechooser.FileFilter

import org.lwjgl.LWJGLException
import org.lwjgl.opengl.{AWTGLCanvas, Display}

import scala.swing.{Action, _}

object Run extends SimpleSwingApplication {

  import java.io.File

  val scale = 2
  val initialWindowSize = new Dimension(256 * scale, 240 * scale)

  private var filePath:Option[String] = None

  implicit val macroGl = org.macrogl.Macrogl.default

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  //Init Mac OSX native menu
  System.setProperty("apple.laf.useScreenMenuBar", "true")
  val app = com.apple.eawt.Application.getApplication
  app.setDefaultMenuBar(mainMenuBar)

  System.setProperty("org.lwjgl.opengl.Window.undecorated", "true")

  lazy val top = new MainFrame {
    title = s"${nescala.BuildInfo.name} ${nescala.BuildInfo.version}"
    size = initialWindowSize
    minimumSize = new Dimension(256, 240)

    peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    peer.setFocusable(true)
    peer.setLocationRelativeTo(null)
    peer.setIgnoreRepaint(true)
  }

  def stepFrame(image:Image) {
    val label = new JLabel(new ImageIcon(image))

    val panel = new JPanel()
    panel.add(label)

    val scrollPane = new JScrollPane(panel)
    JOptionPane.showMessageDialog(null, scrollPane)
  }

  def canvas:Canvas = new AWTGLCanvas {
    setSize(initialWindowSize)
    setFocusable(true)
    setIgnoreRepaint(true)

    override def addNotify() = {
      super.addNotify()
      filePath.foreach{ path => attachDisplay(this, path).start()}
    }

    override def removeNotify() = {
      detachDisplay()
      super.removeNotify()
    }

    def attachDisplay(frame: this.type, path:String) = new Thread {
         override def run() = {
            try {
              Display.setParent(frame)
              Display.create()
              Director(frame, new Audio).Start(path)
              filePath = None
            } catch {
              case e: LWJGLException => e.printStackTrace()
            }
        }
    }

    def detachDisplay():Unit = Display.destroy()
  }

  private def addDisplay(path:String) = {
    top.peer.remove(canvas)
    filePath = Some(path)
    top.peer.add(canvas, BorderLayout.CENTER)
    top.peer.pack()
  }

  def mainMenuBar = {
    val menuBar = new MenuBar() {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Load Rom")(openFileDialog.foreach(addDisplay)))
        contents += new MenuItem(Action("Exit")(System.exit(0)))
      }
    }
    menuBar.peer
  }

  def openFileDialog:Option[String] = {
    val fileChooser = new FileChooser(){
      fileHidingEnabled = true
      title = "Select Rom File"
      fileFilter = new FileFilter {
        override def getDescription: String = ".nes"

        override def accept(file: File): Boolean = file.getName.toLowerCase.endsWith(".nes")
      }
      fileSelectionMode = FileChooser.SelectionMode.FilesAndDirectories
      peer.setCurrentDirectory(new File("user.home"))
    }

    if(fileChooser.showOpenDialog(null) == FileChooser.Result.Approve) Option(fileChooser.selectedFile.getPath)
    else None
  }
}