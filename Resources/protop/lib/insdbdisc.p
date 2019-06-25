/* lib/insdbdisc.p
 *
 */

define input-output parameter dbDirList as character no-undo format "x(250)".

define variable xtext as character no-undo format "x(80)".

if opsys = "unix" then 
  xtext = '      Entries should have a leading "/".'.
 else
  xtext = '      Entries should have a leading "<Drive:~\>".'.

dbDiscovery_loop: do while true on endkey undo, leave on error undo, leave:

  display
    skip(1)
    "      If you would like ProTop to try to discover your running databases enter the list                   " skip
    "      of directories to search below.  Please separate search directories with a comma.                   " skip
&IF OPSYS="Windows" &THEN
    "      You may need to use ALT-9-2 to enter ~\ on a non-English keyboard.                                   " skip
&ENDIF
    xtext                                                                                                        skip
    skip(1)
    "      Leave blank to skip this step.                                                                      " skip
    skip(1)
    dbDirList at 15 view-as fill-in size 60 by 1
    skip(1)
    "      Only running databases will be auto-discovered.                                                     " skip
    skip(1)
   with
    frame updDbDiscovery
    title " ProTop 3 Database Discovery "
    centered
    row 3
    width 110
    no-labels
  .

  update
    dbDirList
   go-on( "enter", "return" )
   with
    frame updDbDiscovery
  .

  /*** if dbDirList = "" then ***/ leave dbDiscovery_loop.

end.

return.
