/* buttonImage.i - rstark - 2.22.2017 */

&GLOBAL-DEFINE imageName
&GLOBAL-DEFINE imageFolder Graphics/32x32/
&IF "{3}" NE "" &THEN
&GLOBAL-DEFINE imageFolder Graphics/{3}x{3}/
&ENDIF

    &IF "{2}" EQ " "       &THEN &GLOBAL-DEFINE imageName inactive
&ELSEIF "{2}" EQ "Add"     &THEN &GLOBAL-DEFINE imageName document_empty
&ELSEIF "{2}" EQ "Cancel"  &THEN &GLOBAL-DEFINE imageName error
&ELSEIF "{2}" EQ "Close"   &THEN &GLOBAL-DEFINE imageName door_exit
&ELSEIF "{2}" EQ "Copy"    &THEN &GLOBAL-DEFINE imageName copy
&ELSEIF "{2}" EQ "Delete"  &THEN &GLOBAL-DEFINE imageName delete
&ELSEIF "{2}" EQ "Done"    &THEN &GLOBAL-DEFINE imageName door_exit
&ELSEIF "{2}" EQ "Down"    &THEN &GLOBAL-DEFINE imageName nav_down
&ELSEIF "{2}" EQ "First"   &THEN &GLOBAL-DEFINE imageName navigate_beginning
&ELSEIF "{2}" EQ "Last"    &THEN &GLOBAL-DEFINE imageName navigate_end
&ELSEIF "{2}" EQ "Left"    &THEN &GLOBAL-DEFINE imageName navigate_left
&ELSEIF "{2}" EQ "Print"   &THEN &GLOBAL-DEFINE imageName printer
&ELSEIF "{2}" EQ "No Note" &THEN &GLOBAL-DEFINE imageName sign_forbidden
&ELSEIF "{2}" EQ "No UDF"  &THEN &GLOBAL-DEFINE imageName sign_forbidden
&ELSEIF "{2}" EQ "Note"    &THEN &GLOBAL-DEFINE imageName edit
&ELSEIF "{2}" EQ "Reset"   &THEN &GLOBAL-DEFINE imageName undo_32
&ELSEIF "{2}" EQ "Right"   &THEN &GLOBAL-DEFINE imageName navigate_right
&ELSEIF "{2}" EQ "Save"    &THEN &GLOBAL-DEFINE imageName floppy_disk
&ELSEIF "{2}" EQ "Up"      &THEN &GLOBAL-DEFINE imageName nav_up
&ELSEIF "{2}" EQ "UDF"     &THEN &GLOBAL-DEFINE imageName window_dialog
&ELSEIF "{2}" EQ "Update"  &THEN &GLOBAL-DEFINE imageName pencil
&ENDIF
