/* notes.i */

&IF "{&IAMWHAT}" NE "snotes" &THEN
headervalue:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-header.
IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN
oFormControl:TEXT = oFormControl:TEXT + " for " + ip-header.
&ENDIF

{&window-name}:TOP-ONLY = YES.

