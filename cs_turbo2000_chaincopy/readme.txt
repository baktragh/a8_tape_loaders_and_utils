CHAINCOPY 1.3 - Auxiliary copy utility
for TURGEN SYSTEM and Czechoslovak Turbo 2000
http://turgen.sourceforge.net/
========================================================================

1. Overview
===========
Chaincopy is a file copier (TAPE>MEMORY>TAPE) for the Czechoslovak Turbo 2000 system.

Chaincopy performs the following:

1. Reads TYPE 04 (BINARY) Turbo 2000 files and converts them to the
   Turbo 2000 - ChainLoading special file format. This special file format 
   allows you to load and execute segmented binary load files from tape.
   
2. Copies Turbo 2000 files of other types unchanged. This allows you to
   copy normal Turbo 2000 files, typically the TYPE 03 (CODE) files.       

Chaincopy requires an XL or XE machine with at least 64 KB of RAM.

2. Why to use Chaincopy 
=======================

The Turbo 2000 system does not provide a file format that would allow you to
load, save, and run segmented binary load files. You can use TYPE 04 files to
load and save your segmented binary load files, but you cannot run them.

Provided that most of the binary load files available in internet archives are
segmented, this is a severe limitation.

With Chaincopy, you can read TYPE 04 files and convert them to a special file  
format (Turbo 2000 - Chainloading) that allows you to load and run them.

Chaincopy is very helpful when you are downloading binary load files from 
internet archives and recording your own tapes, but your only recording device
available is your data recorder. In this case, with TURGEN SYSTEM and Chaincopy,
you can create your own tapes as follows:

1. Transfer Chaincopy to your Atari computer using TURGEN SYSTEM
   and a cassette adapter.
   
2. Convert your binary load file to a TYPE 04 Turbo 2000 file using TURGEN 
   SYSTEM and transfer it to the Chaincopy copier using the cassette adapter.
   To convert the binary load file, use the Turbo 2000 plugin and select
   the Binary file to binary turbo conversion.
   
3. Save your binary file in the Turbo 2000 - ChainLoading file format to a tape.      
   
3. Notes
========
- To exit the copier, press the OPTION key
- To restart the copier, press the BREAK key. NEVER USE THE RESET KEY!
- To abort loading of a file, press the BREAK key. Saving of a file
  cannot be aborted.

4. Limitations
==============
- Maximum size of a copied file is 52479 bytes
- Attempts to copy corrupt binary files will have unpredictable results
- The only supported Turbo system is Turbo 2000
  
5. License
==========
This software is in public domain