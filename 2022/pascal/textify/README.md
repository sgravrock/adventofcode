Textify converts Unix text files for use on the Macintosh. It converts line
endings from \n to \r and sets the file type and creator so that the file can
be opened in TeachText.

## Building

You'll need:

* [THINK Pascal 4](https://www.macintoshrepository.org/1758-symantec-think-pascal-4-0). 
  Other Pascal compilers for the Mac are also likely to work.
* SARez (included with THINK Pascal)
* ResEdit (included with THINK Pascal)

There is a chicken-and-egg problem here: The Textify source code is made up of
text files, which must be converted before you can build the converter program.
Use `tr` for this, e.g. `tr '\n' < original-file > mac-copy`. Then get the
converted copies onto the old Mac.

Use ResEdit to set the file type of `textify.r` and `textify.p` to `TEXT`.
Optionally set the creator of `textify.p` to `PJMM`.

Use SARez to compile `textify.r` to `textify.rsrc`.

Create a new THINK Pascal project and add `textify.p` to it. Select Run-> Run
Options and specify `textify.rsrc` as the resource file. Finally, select
Project -> Build Application.
