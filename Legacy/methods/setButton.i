/* setButton.i - rstark - 2.22.2017 */

&SCOPED-DEFINE imageFolder Graphics/32x32/
&IF "{3}" NE "" &THEN
&SCOPED-DEFINE imageFolder Graphics/{3}x{3}/
&ENDIF
&SCOPED-DEFINE imageType .png
&SCOPED-DEFINE imageName

    &IF "{2}" EQ " "       &THEN &SCOPED-DEFINE imageName inactive
&ELSEIF "{2}" EQ "Add"     &THEN &SCOPED-DEFINE imageName document_empty
&ELSEIF "{2}" EQ "Cancel"  &THEN &SCOPED-DEFINE imageName error
&ELSEIF "{2}" EQ "Close"   &THEN &SCOPED-DEFINE imageName door_exit
&ELSEIF "{2}" EQ "Copy"    &THEN &SCOPED-DEFINE imageName copy
&ELSEIF "{2}" EQ "Delete"  &THEN &SCOPED-DEFINE imageName delete
&ELSEIF "{2}" EQ "Down"    &THEN &SCOPED-DEFINE imageName nav_down
&ELSEIF "{2}" EQ "First"   &THEN &SCOPED-DEFINE imageName navigate_beginning
&ELSEIF "{2}" EQ "Last"    &THEN &SCOPED-DEFINE imageName navigate_end
&ELSEIF "{2}" EQ "Left"    &THEN &SCOPED-DEFINE imageName navigate_left
&ELSEIF "{2}" EQ "No Note" &THEN &SCOPED-DEFINE imageName sign_forbidden
&ELSEIF "{2}" EQ "No UDF"  &THEN &SCOPED-DEFINE imageName sign_forbidden
&ELSEIF "{2}" EQ "Note"    &THEN &SCOPED-DEFINE imageName edit
&ELSEIF "{2}" EQ "Reset"   &THEN &SCOPED-DEFINE imageName undo_32
&ELSEIF "{2}" EQ "Right"   &THEN &SCOPED-DEFINE imageName navigate_right
&ELSEIF "{2}" EQ "Save"    &THEN &SCOPED-DEFINE imageName floppy_disk
&ELSEIF "{2}" EQ "Up"      &THEN &SCOPED-DEFINE imageName nav_up
&ELSEIF "{2}" EQ "UDF"     &THEN &SCOPED-DEFINE imageName window_dialog
&ELSEIF "{2}" EQ "Update"  &THEN &SCOPED-DEFINE imageName pencil
&ENDIF

DO:
    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
        &IF "{&imageName}" NE "" &THEN
        {1}:LOAD-IMAGE ("{&imageFolder}{&imageName}{&imageType}").
        &ENDIF
        IF {1}:LABEL NE "" THEN 
        {1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".
    END.
END.