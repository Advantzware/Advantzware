/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

     
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{methods/template/globaldef.i}
ON CLOSE OF THIS-PROCEDURE 
    RUN dispatch IN THIS-PROCEDURE ('destroy':U).

RUN dispatch ('create-objects':U).
       
/* Execute this code only if not being run PERSISTENT, i.e., if in test mode
   of one kind or another or if this is a Main Window. Otherwise postpone 
   'initialize' until told to do so. */

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
&ENDIF
    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
    MAIN-BLOCK:
    DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

        /* Now enable the interface and wait for the exit condition.            */
        /* Set the option frame size and colour to give blue background to icons and 
          add the handle of scope define object to temptable for resizizng */
        RUN beforeinitialize.
        RUN dispatch ('initialize':U).
        /* Add the handle of all smart object to be resized/shifted on resize to the temptable and 
          Shift all the icons towards right */
        RUN afterinitialize.
        IF NOT THIS-PROCEDURE:PERSISTENT THEN
            WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
END.
&ENDIF



