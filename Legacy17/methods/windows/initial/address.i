/* address.i */

&IF "{&IAMWHAT}" NE "saddress" &THEN
headervalue:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-header.
&ENDIF