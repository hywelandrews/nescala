### Nescala

A NES emulator written in Scala.

### Screenshots

![Screenshots](http://i.imgur.com/PiG6XCD.jpg)

### How to run

    ~ sbt run 

### Controls

Single Controller supported, the button mapping is hard-coded.
Keyboard controls are:

| Nintendo              | Emulator    |
| --------------------- | ----------- |
| Up, Down, Left, Right | Arrow Keys  |
| Start                 | Enter       |
| Select                | Right Shift |
| A                     | Z           |
| B                     | X           |
| A (Turbo)             | A           |
| B (Turbo)             | S           |
| Reset                 | CTRL+R      |
| Eject                 | CTRL+E      |
| Pause                 | CTRL+S      |
| Resume                | CTRL+C      |

PS3/4 and XBOX 360 Controllers are also supported

### Mappers

The following mappers have been implemented:

* NROM (0)
* MMC1 (1)
* UNROM (2)
* CNROM (3)
* MMC3 (4)
* AOROM (7)
* MMC2 (9)
* VRC2b (23)
* VRC4b (25)

### Known Issues

* APU emulation needs attention in regards to timing and DMA
