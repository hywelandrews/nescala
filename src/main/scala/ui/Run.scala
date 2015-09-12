package ui

import java.awt.Toolkit
import java.awt.event._
import javax.swing._

import nescala.BuildInfo

/**
 * Created by Hywel on 7/30/15.
 */
object Run extends JFrame {

  private final val listener = new AL()

  class AL extends ActionListener with WindowListener {

    override def actionPerformed(arg0:ActionEvent)
    {
      // placeholder for more robust handler
      if (arg0.getActionCommand.equals("Quit")) {
        //nes.quit()
      }
    }

    override def windowDeiconified(windowEvent: WindowEvent): Unit = ???

    override def windowClosing(windowEvent: WindowEvent): Unit = ???

    override def windowClosed(windowEvent: WindowEvent): Unit = ???

    override def windowActivated(windowEvent: WindowEvent): Unit = ???

    override def windowOpened(windowEvent: WindowEvent): Unit = ???

    override def windowDeactivated(windowEvent: WindowEvent): Unit = ???

    override def windowIconified(windowEvent: WindowEvent): Unit = ???
  }

    def main(args: Array[String]) = {
        //  construct window
        this.setTitle(s"Nescala ${BuildInfo.version}")
        this.setResizable(false)
      buildMenus()
      //setRenderOptions()
    }

    def buildMenus() {
      val menus = new JMenuBar()
      val file = new JMenu("File")
      val itemOpenRom = file.add(new JMenuItem("Open ROM"))
      itemOpenRom.addActionListener(listener)
      itemOpenRom.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

      file.addSeparator()

      val itemPreferences = file.add(new JMenuItem("Preferences"))
      itemPreferences.addActionListener(listener)
      itemPreferences.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

      file.addSeparator()

      val itemToggleFullscreen = file.add(new JMenuItem("Toggle Fullscreen"))
      itemToggleFullscreen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0))
      itemToggleFullscreen.addActionListener(listener)
      menus.add(file)

      val itemQuit = file.add(new JMenuItem("Quit"))
      itemQuit.addActionListener(listener)
      menus.add(file)

    val nesmenu = new JMenu("NES")
    val itemReset = nesmenu.add( new JMenuItem("Reset"))
      itemReset.addActionListener(listener)
      itemReset.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

    val itemHardReset = nesmenu.add(new JMenuItem("Hard Reset"))
      itemHardReset.addActionListener(listener)
      itemHardReset.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_T,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

   val itemPause =  nesmenu.add(new JMenuItem("Pause"))
      itemPause.addActionListener(listener)
      itemPause.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F7, 0))

      val itemResume = nesmenu.add(new JMenuItem("Resume"))
      itemResume.addActionListener(listener)
      itemResume.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F8, 0))

      val itemFastForward = nesmenu.add(new JMenuItem("Fast Forward"))
      itemFastForward.addActionListener(listener)
      itemFastForward.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

      val itemFrameAdvance = nesmenu.add(new JMenuItem("Frame Advance"))
      itemFrameAdvance.addActionListener(listener)
      itemFrameAdvance.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

      nesmenu.addSeparator()

      val itemControllerSettings = nesmenu.add(new JMenuItem("Controller Settings"))
      itemControllerSettings.addActionListener(listener)
      itemControllerSettings.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

      val itemCheatCodes = nesmenu.add(new JMenuItem("Cheat Codes"))
      itemCheatCodes.addActionListener(listener)
      itemCheatCodes.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

      nesmenu.addSeparator()

      val itemRomInfo = nesmenu.add(new JMenuItem("ROM Info"))
      itemRomInfo.addActionListener(listener)
      itemRomInfo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
      Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

      menus.add(nesmenu)

      val help = new JMenu("Help")
      val itemAbout = help.add(new JMenuItem("About"))
      itemAbout.addActionListener(listener)
      itemAbout.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0))
      menus.add(help)
      this.setJMenuBar(menus)
    }
}
