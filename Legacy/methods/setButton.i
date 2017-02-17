    &IF "{2}" EQ "Cancel" &THEN
&SCOPED-DEFINE image Graphics/32x32/error.ico
&ELSEIF "{2}" EQ "Close"  &THEN
&SCOPED-DEFINE image Graphics/32x32/door_exit.ico
&ELSEIF "{2}" EQ "Save"   &THEN
&SCOPED-DEFINE image Graphics/32x32/floppy_disk.ico
&ELSEIF "{2}" EQ "Update" &THEN
&SCOPED-DEFINE image Graphics/32x32/floppy_disk_window.ico
&ENDIF

{1}:LOAD-IMAGE ("{&image}").
{1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".

&UNDEFINE image