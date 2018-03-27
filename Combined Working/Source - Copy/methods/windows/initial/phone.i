/* phone.i */

&IF "{&IAMWHAT}" NE "sphone" &THEN
headervalue:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-header.
&ENDIF