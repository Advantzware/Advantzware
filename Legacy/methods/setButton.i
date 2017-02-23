&SCOPED-DEFINE imageName 

    &IF "{2}" EQ " "       &THEN &SCOPED-DEFINE imageName inactive.png
&ELSEIF "{2}" EQ "Cancel"  &THEN &SCOPED-DEFINE imageName error.png
&ELSEIF "{2}" EQ "Close"   &THEN &SCOPED-DEFINE imageName door_exit.png
&ELSEIF "{2}" EQ "Down"    &THEN &SCOPED-DEFINE imageName nav_down.png
&ELSEIF "{2}" EQ "First"   &THEN &SCOPED-DEFINE imageName navigate_beginning.png
&ELSEIF "{2}" EQ "Last"    &THEN &SCOPED-DEFINE imageName navigate_end.png
&ELSEIF "{2}" EQ "Left"    &THEN &SCOPED-DEFINE imageName navigate_left.png
&ELSEIF "{2}" EQ "No Note" &THEN &SCOPED-DEFINE imageName sign_forbidden.png
&ELSEIF "{2}" EQ "No UDF"  &THEN &SCOPED-DEFINE imageName sign_forbidden.png
&ELSEIF "{2}" EQ "Note"    &THEN &SCOPED-DEFINE imageName edit.png
&ELSEIF "{2}" EQ "Right"   &THEN &SCOPED-DEFINE imageName navigate_right.png
&ELSEIF "{2}" EQ "Save"    &THEN &SCOPED-DEFINE imageName floppy_disk.png
&ELSEIF "{2}" EQ "Up"      &THEN &SCOPED-DEFINE imageName nav_up.png
&ELSEIF "{2}" EQ "UDF"     &THEN &SCOPED-DEFINE imageName window_dialog.png
&ELSEIF "{2}" EQ "Update"  &THEN &SCOPED-DEFINE imageName pencil.png
&ELSEIF "{2}" EQ "Reset"   &THEN &SCOPED-DEFINE imageName undo_32.png
&ELSEIF "{2}" EQ "Add"     &THEN &SCOPED-DEFINE imageName document_empty.png
&ELSEIF "{2}" EQ "Copy"    &THEN &SCOPED-DEFINE imageName copy.png
&ELSEIF "{2}" EQ "Delete"  &THEN &SCOPED-DEFINE imageName delete.png
&ENDIF

&IF "{3}" EQ "" &THEN
&SCOPED-DEFINE imageDir Graphics/32x32/
&ELSE
&SCOPED-DEFINE imageDir Graphics/{3}x{3}/
&ENDIF

DO:
    &IF "{&imageName}" NE "" &THEN
    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
        {1}:LOAD-IMAGE ("{&imageDir}{&imageName}").
        &ENDIF
        IF {1}:LABEL NE "" THEN 
        {1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".
    END.
END.