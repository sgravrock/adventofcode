These programs are written for THINK Pascal 4 on Motorola 68k Macintoshes.

## What you'll need

* An old Mac, or an emulator such as [Basilisk II](https://www.emaculation.com/doku.php/basilisk_ii).
  I'm not sure if a PowerPC machine will work. I used a Mac Classic.
* System 6 through Mac OS 8.1. I used 6.0.7.
* [THINK Pascal 4.0](https://www.macintoshrepository.org/1758-symantec-think-pascal-4-0).
* A working copy of the Textify program (in this repo) or some other way to
   fix both line endings and file types when copying text files to the old Mac.
* A way to get files onto the old Mac.

## General process for building and running things

1. Copy the source files and your puzzle input file onto the old Mac.
2. Use Textify to convert the files.
3. Create a new THINK Pascal project and add the source file to it.
4. Hit CMD-g.

Most of the programs are not well-behaved Mac applications. If you build them
and run them from the Finder, you won't have a chance to see the result. Run
them from inside the THINK Pascal environment instead.

## Portability

Maybe?

THINK Pascal implements quite a few extensions to ANSI Pascal, and I'm almost
certainly using some of them. There was a fair degree of de facto
standardization of extensions among Pascal compilers for the Mac, and some
modern implementations like FreePascal support that dialect. But it's hard to
know for sure what I'm depending on without actually trying these out on a 
variety of compilers.

A few days use Object Pascal. That extension was and is widespread, but I
would expect modern dialects to have departed somewhat from the 1980s/90s
flavor.

I/O is at least a little unportable in every case. Some days use `OldFileName`,
which is a built-in function that shows the standard file open dialog. As far
as I know it's unique to THINK Pascal. Others use Macintosh system I/O. In
those cases, the parts that do file I/O will need to be rewritten to run on
other systems, including modern Macs running OS X.

In descending order of how likely it is to work:
1. Other THINK Pascal versions
2. Other 1980s or early 90s Mac Pascal compilers
3. Other Pascal compilers that explicitly support the Macintosh dialect, such
   as FreePascal with `{$MODE MACPAS}`
4. Other ANSI or post-ANSI Pascal compilers
5. Genuine Wirth Pascal (you masochist)
