### Nescala

A NES emulator written in Scala.

### Dependencies

    sbt 

### Controls

Joysticks are supported, although the button mapping is currently hard-coded.
Keyboard controls are indicated below.

| Nintendo              | Emulator    |
| --------------------- | ----------- |
| Up, Down, Left, Right | Arrow Keys  |
| Start                 | Enter       |
| Select                | Right Shift |
| A                     | Z           |
| B                     | X           |
| A (Turbo)             | A           |
| B (Turbo)             | S           |

### Mappers

The following mappers have been implemented:

* NROM (0)
* MMC1 (1)
* UNROM (2)
* CNROM (3)

### Known Issues

* Performance, excessive object creation is causing gc pauses
* APU emulation needs attention in regards to timing and DMA 
