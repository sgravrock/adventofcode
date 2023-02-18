In addition to the usual build steps, this program requires a resource file.
To set it up:

1. Copy `day 14 part 2.r` onto the Mac, converting line endings as usual.
2. Use SARez to compile `day 14 part 2.r` to `day 14 part 2.rsrc`. Be sure to
   put `day 14 part 2.rsrc` in the same folder as the THINK Pascal project file.
3. In THINK Pascal, select Run-> Run Options and specify `day 14 part 2.rsrc`
   as the resource file.

Version 883078a and later should link `µRuntime.lib` rather than `runtime.lib`.
Older versions need `sysfileutils.p` and newer versions need `µSysfileutils.p`.
Check the `uses` statement near the top of `day 14 part 2.p` to see which is
needed.