# COPY T/H

## Overview

COPY T/H is a special copier that copies Turbo 2000 files to the H1: host device. This allows decoding Turbo 2000 files from wave files using selected 8-bit Atari emulators.

## Operation

1. Sample signal from your cassette to a WAVE file.
1. Setup your emulator to emulate ATARI XL/XE machine with 64 KB of RAM.
1. Setup your emulator to enable H1: host device in read/write mode. Clear directory assigned to the H1: host device.
1. Load the WAVE file as an emulated tape. Setup the emulator to emulate a Turbo 2000 enhanced data recorder.
1. Run COPY T/H. Press START to begin copying.
1. COPY T/H will keep copying Turbo 2000 files to the H1: device. The copier creates both binary files (.xex) and tape images (.cas). Every file is reported in the H1:LISTING.TXT file.
1. Press RESET when done. Review contents of the directory assigned to the H1: device.

## Emulators supported

Atari800 with a8cas extensions, Altirra.

- [atari800-a8cas](http://a8cas.sourceforge.net/features.html#patch)
- [atari800-a8cas modified](http://www.arus.net.pl/FUJI/a8cas-util/downloads/modified-atari800-emulator.html)
- [Altirra](https://www.virtualdub.org/altirra.html)

There is no point in running this copier with real hardware as there is no H1: host device.

## Binaries

There are two binaries shipped:

- copyth10.xex. Writes 10-character names to the H1: device. Use with the atari800-a8cas emulators, or Altirra 4.10-test2 and above.
- copyth8.xex. Writes 8-character names to the H1: device. Use with the Altirra emulator version up to 4.10-test1. Be advised that because of the short names, files can get overwritten.


## Why this copier was written

The supported emulators have decent DSP capabilities that help with decoding of Turbo 2000 tapes. This copier allows you tu use the capabilities to quickly decode files from whole sides of sampled tapes. The output both to .xex and .cas allows to preserve multi-stage software with convenience.
