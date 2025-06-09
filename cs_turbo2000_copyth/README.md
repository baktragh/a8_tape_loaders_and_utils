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

## Notes

- The H1: device provided by the emulator must be configured to support long file names
- Running this copier with real computer doesn't work

## Binaries

- copyth11.xex. Writes 11-character names to the H1: device.

## Why this copier was written

The supported emulators have decent DSP capabilities that help with decoding of Turbo 2000 tapes. This copier allows you tu use the capabilities to quickly decode files from whole sides of sampled tapes. The output both to .xex and .cas allows to preserve multi-stage software with convenience.
