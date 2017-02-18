&SCOPED-DEFINE imageDir Graphics/32x32/
&SCOPED-DEFINE imageName 
&SCOPED-DEFINE imageType .ico

    &IF "{2}" EQ " "      &THEN &SCOPED-DEFINE imageName inactive
&ELSEIF "{2}" EQ "Cancel" &THEN &SCOPED-DEFINE imageName error
&ELSEIF "{2}" EQ "Close"  &THEN &SCOPED-DEFINE imageName door_exit
&ELSEIF "{2}" EQ "Down"   &THEN &SCOPED-DEFINE imageName nav_down
&ELSEIF "{2}" EQ "First"  &THEN &SCOPED-DEFINE imageName navigate_beginning
&ELSEIF "{2}" EQ "Last"   &THEN &SCOPED-DEFINE imageName navigate_end
&ELSEIF "{2}" EQ "Left"   &THEN &SCOPED-DEFINE imageName navigate_left
&ELSEIF "{2}" EQ "Right"  &THEN &SCOPED-DEFINE imageName navigate_right
&ELSEIF "{2}" EQ "Save"   &THEN &SCOPED-DEFINE imageName floppy_disk
&ELSEIF "{2}" EQ "Up"     &THEN &SCOPED-DEFINE imageName nav_up
&ELSEIF "{2}" EQ "Update" &THEN &SCOPED-DEFINE imageName floppy_disk_window
&ENDIF

&IF "{&imageName}" NE "" &THEN
{1}:LOAD-IMAGE ("{&imageDir}{&imageName}{&imageType}").
&ENDIF
{1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".
