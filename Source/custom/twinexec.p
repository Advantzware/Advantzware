  /* twinexec.p */
  
  DEF VAR lv-cmd AS cha NO-UNDO.
  DEF VAR lv-image AS cha NO-UNDO.
  DEF VAR lv-return AS INT NO-UNDO.


  lv-cmd = "c:\program files\adobe\acrobat 5.0\reader\acrord32.exe".    
  lv-image = "r:\asi_gui9\source\boximage\accordfg.pdf".

      /*
     lv-cmd = "c:\progra~~1\adobe\acroba~~1.0\reader\acrord32.exe".
     MESSAGE lv-cmd itemfg.box-image VIEW-AS ALERT-BOX.
     
     
     OS-COMMAND  VALUE('"' + lv-cmd + " " + itemfg.box-image + '"').
     */

     /* lv-cmd = ('"' + lv-cmd + " " + lv-image + '"').  NOT working */
     lv-cmd = lv-cmd + CHR(32) + lv-image. 
     
     
     /*RUN winexec (INPUT lv-cmd + CHR(32) + lv-image, INPUT 1,OUTPUT lv-return). */
     RUN winexec (INPUT lv-cmd , INPUT 1,OUTPUT lv-return). 
MESSAGE lv-return VIEW-AS ALERT-BOX.

PROCEDURE WinExec EXTERNAL "kernel32.dll":
    DEFINE INPUT PARAMETER programname AS cha.
    DEFINE INPUT PARAMETER visualstyle AS long.
    DEFINE RETURN PARAM statuscode AS LONG.
END.

