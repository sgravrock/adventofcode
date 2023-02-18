# Transferring files to and from old Macs

Getting files onto and off of old Macs can be difficult
for a number of reasons:

* All Mac files have type metadata that is lost when they're stored on a non-Mac
  filesystem. Without that metdata, the files can't be opened.
* Many Mac files, particularly applications, have resource forks that are lost
  when they're stored on a non-Mac filesystem.
* The archive format that's commonly used to work around the above problems is
  proprietary and can only be read by software that can itself be difficult to
  get onto old Macs.
* The HFS filesystem used by older Macs can't be read by anything newer than
  Snow Leopard, and can't be written by anything newer than Leopard. HFS
  implementations for other operating systems generally can't write resource forks
  or type/creator metadata.
* Web browsers that speak sufficiently new TLS aren't available for the Mac OS
  versions that support HFS.
* Older Macs can't network with modern computers.
* Older Macs don't support modern removable media. Newer computers don't support
  older removable media out of the box, and in most cases can't be made to work
  with 400k or 800k floppies.
* The floppy drives in old Macs are old machines with lots of moving parts,
  lubricants, etc. They are well beyond their design life and are likely to need
  service before they'll work properly.

The exact solution to these problems is going to depend on what you have on
hand. I was trying to exchange files with a Mac Classic, which has a 1.4MB
floppy drive and LocalTalk but not Ethernet. The floppy drive could reliably
read disks but not write them. I had a modern Mac running OS X Monterey, an
older Intel Mac running OS X Snow Leopard, a Windows 10 machine with no
provision for a floppy drive, and a USB floppy drive.

## The easy problem: Copying an HFS floppy image to floppy disk

Some Macintosh software has been archived in the form of HFS floppy disk images.
Notably, [THINK Pascal 4](https://www.macintoshrepository.org/1758-symantec-think-pascal-4-0)
is available in this format. This makes things easyish because there's no 
problem with file attributes or resource forks, and nothing needs to be
un-archived on the receiving side. I was able to write the images to a disk in 
a USB floppy drive on my modern Mac laptop using the `dd` command. (**Warning**:
`dd` will happily overwrite whatever volume you point it at, even your boot
disk or a mounted Time Machine backup. Be careful, triple-check the destination
device, and consider turning off Time Machine temporarily.) The key arguments
seem to be `bs=512 conv=noerror,sync`. I'm not sure how important `noerror` is,
but the block size is definitely load-bearing.

I used an IBM-branded Teac USB floppy drive that looks like it's from the early
2000s or so. I tried a couple of new-production USB floppy drives but they were
both defective. I could not find a way to write disk images to a USB floppy
drive under Windows. Windows floppy image writing software expects to work with
a real PC floppy drive controller, not a USB mass storage device that happens to
use a floppy disk.

Most .img files and some "Disk Copy images" on sites like macintoshrepository.org
are raw images that can be written directly to a floppy in this manner. Some are
a more complex Disk Copy format. More on that below. You can tell the difference
using the `file` command on OS X. If it says something like 
`Macintosh HFS data block size: 512...` and the file is no larger than a floppy
disk, you can write it directly with `dd`. It doesn't matter if the image is
1.4MB or 800K. You can write either kind to a 1.4MB floppy the same way, and the
disk will be readable in a 1.4MB drive.

## Harder: Dealing with missing Desktop files when your floppy drive can't write

The Desktop file is an index of Finder metadata such as which files are on the
desktop, which files use which icons, etc. Every HFS filesystem is supposed to
have one, but I ran into a couple of cases where software was distributed in
disk images that lacked a Desktop file. Normally that's not a problem because
the file will be regenerated when you insert the disk. But the floppy drive in
my old Mac can't write, and the system ejects the disk when it can't create the
Desktop file.

Solving this requires a machine that can both read and write either the HFS
floppy itself or an image of it. My oldest "modern" Mac, running Snow Leopard,
is just slightly too new to do that. I ended up using a Windows program called
[CiderPress](https://a2ciderpress.com/) to copy files from the bad disk image to
a known good one. The trick is to copy and paste the files within CiderPress
rather than exporting and re-importing them, since the Windows filesystem can't
store Mac file attributes or resource forks.

## Harder: Dealing with text files

Advent of Code puzzles use individualized input files which are usually big
enough that you wouldn't want to type them in. Copying them to the old Mac
involves all the above, plus a few new hurdles. The files don't start out on an
HFS image. They don't have type and creator attributes, which are needed to open
them on the old Mac. (Classic Mac OS does not use file extensions.) And the
files start out with a line feed at the end of each line (or carriage return +
line feed if they were donwloaded on Windows), while classic Mac text editors
expect a carriage return.

The initial process I used was:

1. On the modern Mac, fix up the line endings: `tr '\n' '\r' < input.txt > input.mac.txt`
2. Copy the resulting file to the Windows machine.
3. Write the file to an HFS floppy image using CiderPress.
4. Copy the image to the modern Mac.
5. Write the image to a floppy.
6. Copy the file from the floppy to the old Mac's hard disk.
7. Use ResEdit to set the file type to `TEXT` and the creator to `ttxt`.

That works, but it's kind of a pain. I didn't do that too many times before
writing [Textify](https://github.com/sgravrock/adventofcode/tree/master/2022/pascal/textify),
which runs on the old Mac and automates the line ending and type/creator fixes.

Another problem with this approach is that it involves writing every sector of
the floppy disk just to transfer a small text file. Most floppy disks available
today were made after 2000, when floppies were used mainly for file transfer
between machines rather than as the primary storage for anything. They were a
lot less reliable than older floppies even when new, and I find that they don't
last long in heavy use. Once I had a BlueSCSI (see below), I switched to using
that instead of floppies.

## Harder: Going the other direction

With a fully working floppy drive, "assembly is the reverse of disassembly". But
the floppy drive in my old Mac wasn't fully working. It couldn't write. This
initially left me with no way to get data off the machine: It couldn't write to
any removable storage and didn't have any network protocols or even physical
network media in common with modern computers.

I solved this with a [BlueSCSI](https://bluescsi.com/), which is a device that
connects to the old Mac's SCSI bus and presents HFS images on an SD card to the
host computer as if they are SCSI hard disks. The external model can plug
directly into the SCSI port on the back of the Mac, but I found that it was a
lot easier to use if I connected it with a short cable. The hard disk images
used by the BlueSCSI aren't exactly the same as raw HFS filesystem images (they
have a partition table), but CiderPress can still handle them.

To get files off the old Mac with this setup:

1. Shut down the old Mac. (SCSI is not hot swappable.)
2. Take the SD card out of the BlueSCSI and put it in the Windows machine.
3. Use CiderPress to export the desired files.
4. Copy the files to the new Mac.
5. Convert line endings.

Converting line endings in this direction can be a little trickier. Old Macs
used the [MacRoman](http://www.alanwood.net/demos/macroman.html) character
set, which is not entirely compatible with UTF-8. `tr` on OS X can handle the
subset of MacRoman that's commonly used in English text and Pascal source code,
but reliably chokes on resource scripts. I solved that with
[a trivial C program](https://github.com/sgravrock/adventofcode/blob/master/2022/pascal/textify/unconv.c).
An easier method is to open the file in BBEdit, change the encoding and line 
ending, and save it.

## Unsolved: Dealing with StuffIt archives and the other Disk Copy image format

Early Mac software tends to be archived as HFS images. Later software,
particularly anything that was distributed on CD rather than floppies, tends to
be archived in StuffIt files or a newer Disk Copy image format that isn't just
a raw copy of the disk.

StuffIt was the most popular archiving and compression software on classic Macs.
StuffIt archives come in two forms: an older format that can't survive on
non-Mac filesystems, and a newer format that can. `.sit` files on sites like 
macintoshrepository.org are, for obvious reasons, always the newer format. Older
versions of StuffIt are available on HFS floppy images, but the versions that
can deal with the newer format are only available on the newer Disk Copy image
format. I haven't found a way to write those images to physical media on modern
computers. The raw image is wrapped in some sort of container format, so writing
the file directly to a floppy results in an unreadable disk. The obvious
solution is to open the image with Disk Copy on the old Mac, but there's a
chicken-and-egg problem: I don't have Disk Copy, and it's only distributed as a
StuffIt archive. So I need Disk Copy to get StuffIt, and I need StuffIt to get
Disk Copy.

Supposedly Disk Copy came with System 6 and later, although it's not on my
computer or on any of the System 7 boot images I've found. It could be that most
people removed Disk Copy to save space, and reinstalling System 6 would solve
the problem. It's also possible that these archives need a newer version of Disk
Copy.

I'm not aware of any off-the-shelf software that solves this chicken-and-egg
problem. It's possible that I could program my way out of it. I've found that
[The Unarchiver](https://theunarchiver.com/), unlike modern versions of StuffIt,
can extract files from StuffIt archives. If the files are extracted to the
standard OS X filesystem, they don't lose their attributes or resource forks.
I could probably make something that splits the file up into a set of
plain-old-stream-of-bytes files on the new Mac and then reassembles them on the
old one. I just haven't gotten around to it yet.

## Things that would have been easier (Or: Do as I say, not as I do)

If I'd had a "bridge" machine that was new enough to either have USB or speak 
Ethernet + SMB but old enough to speak LocalTalk, a lot of this would've been 
much easier. I could've copied downloaded images or archives to the bridge 
machine, extracted the files onto an AppleTalk share, and then copied them to 
the old Mac. Even a slightly older machine that supported Ethernet and TCP but
not SMB or USB would've helped quite a bit. It's not just a matter of having
compatible media, although that helps quite a bit. A lot of 1990s Mac software
was archived by people who had machines of this generation, and they used file
formats that are a lot harder to deal with if you only have older and newer
computers.

Simply doing my retrocomputing on such a machine, rather than using it as a 
bridge to something older, would've been even easier. I had sound reasons for 
not going that route. But if you don't specifically want something older,
getting a computer that can talk directly to your daily driver is going to make
everything a lot easier.

Getting Mac OS 9 running in the [Basilisk II emulator](https://www.emaculation.com/doku.php/basilisk_ii)
would also have helped a lot. This is how the BlueSCSI folks recommend you get
software onto an old Mac. Physical media still would've been a pain, especially
for anything that's too big to fit on a floppy, but dealing with newer archive
and disk image formats would've been much easier. However, I haven't been able
to get Basilisk II to boot. I'm not sure if the ARM build doesn't work or if
I'm just misconfiguring it. Probably the latter, but the error messages don't
give me any path forward.

A few people have ported HFS to newer versions of OS X. I'm aware of at least
one kernel driver and two [MacFUSE](https://osxfuse.github.io/) plugins. It's
not clear whether any of them still work on Monterey, and I didn't want to risk
it on my daily driver. Running one of those on a slightly older version of OS X
might have been a really nice solution.

## Things that would have been harder (Or: Computers to avoid if you're new at this)

The 400k and 800k floppy disks used by *really* old Macs are physically
incompatible with modern drives. They need a variable-speed motor and a stronger
magnetic field. Disk written in a modern floppy drive aren't readable in an 800k
or 400k drive, and vice versa. The 1.4MB floppy drives in vintage Macs like my
Classic are the newest drives that can read and write 400k and 800k floppies.
So if I was trying to use a machine with those drives I'd need something like my
Classic to serve as a bridge machine, and everything I described above would
just have been Step 1. And that's assuming that all of the floppy drives
involved work. The older the drive, the less likely that is.

Anything older than a Mac Plus doesn't have a SCSI port. That rules out use of
a BlueSCSI, which I found to be the by far best tool for physical file transfer.
External hard drives were available for those computers, but they used either
the floppy drive port or the serial port. Good luck getting *that* to talk to
anything newer.

Go far enough back and you're going to have to deal with the older MFS
filesystem. (The Mac 512k and 512ke don't support HFS out of the box, and the
original Mac 128k can't use it at all.) Tools that deal with MFS are largely
extinct. Apple dropped support for it long enough ago that a lot of 
otherwise-reasonable bridge machines can't handle it.

The big lesson to take away from all of this is: Unless you like solving these
kinds of puzzles you should get the newest machine, in the best condition, that
will still scratch your particular retrocomputing itch.