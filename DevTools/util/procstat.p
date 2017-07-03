/* util/procstat.p - Display by module the number of entries of z_proc and a dest directory */
/* Author Andre Legault                                             October 1999            */ 

  DEFINE VARIABLE i           AS integer   LABEL "Z_proc Entries" FORMAT ">,>>>,>>9-".
  DEFINE VARIABLE prevmod     AS CHARACTER LABEL "Module" FORMAT "x(15)".
  DEFINE VARIABLE curmod      AS CHARACTER.
  DEFINE VARIABLE cDest-dir   AS CHARACTER LABEL "Directory"   INIT "progui" FORMAT "x(40)".
  DEFINE VARIABLE cReportFile AS CHARACTER LABEL "Report File" INIT "comp-Stat.log" FORMAT "x(40)".
  DEFINE VARIABLE OKpressed   AS LOGICAL INITIAL TRUE.
  
  DEFINE VARIABLE nbEntries   AS integer COLUMN-LABEL "Directory Number" FORMAT ">,>>>,>>9-".
 
  DEFINE STREAM dirdata.
  DEFINE STREAM messArea.
   
  DEFINE BUTTON btnStart LABEL "&Start Process".
  DEFINE BUTTON btnReportFile  LABEL "&Browse".
  DEFINE BUTTON btnExit  LABEL "&Exit".
  
  FUNCTION fDir RETURN CHARACTER (fModuleName AS CHARACTER).
    DEFINE VARIABLE rValue AS CHARACTER.
    DEFINE VARIABLE compteur AS INTEGER.
    IF NUM-ENTRIES(fModuleName,"/") < 2 THEN RETURN "".
    REPEAT compteur = 1 TO NUM-ENTRIES(fModuleName,"/") - 1:
       rValue = rValue + ENTRY(compteur,fModuleName,"/") + "/".
    END.
    rValue = SUBSTRING(rValue, 1, length(rValue) - 1).
    RETURN rValue.
  END FUNCTION.
  
  FORM SKIP(1) cDest-dir   colon 13 
       SKIP(1) cReportFile colon 13 SPACE(2) btnReportFile space(2) 
       SKIP(1) btnStart at 15 SPACE (5) btnExit
       SKIP (1)
       WITH FRAME fParamete CENTERED SIDE-LABELS.
    
  FORM prevmod i nbEntries
       WITH FRAME detail 18 DOWN width 80.
  
  IF "{&WINDOW-SYSTEM}" <> "TTY" THEN cDest-dir = "progui".
  
  DISPLAY cDest-dir cReportFile btnReportFile btnStart btnExit WITH FRAME fParamete.
  
  ENABLE cDest-dir cReportFile btnReportFile btnStart btnExit with three-D 
         WITH FRAME fParamete.

  ON LEAVE OF cDest-Dir ASSIGN cDest-Dir.
  
  ON CHOOSE OF btnReportFile
  DO:
     RUN RptOutput (INPUT-OUTPUT cReportFile).
     DISPLAY cReportFile WITH FRAME fParamete.
  END.
  
  ON CHOOSE OF btnStart
  DO:
      MESSAGE "Computation in Process. Please wait..." cDest-dir.
      HIDE FRAME fParamete.
      FIND FIRST  z_proc NO-LOCK NO-ERROR.
      IF AVAILABLE  z_proc 
      THEN ASSIGN prevMod  = fDir(z_proc.module-name) /* ENTRY(1,z_proc.module-name,"/") */
                  curmod   = fDir(z_proc.module-name) /* ENTRY(1,z_proc.module-name,"/"). */.
    
      IF cReportFile:SCREEN-VALUE NE "screen" THEN OUTPUT TO VALUE(cReportFile:SCREEN-VALUE).
  
      FOR EACH  z_proc WHERE  z_proc.can-compile = yes NO-LOCK: 
        IF NUM-ENTRIES(z_proc.module-name,"/") < 2 THEN NEXT.
        IF INDEX(z_proc.module-name,".w1") NE 0    THEN NEXT. /* NEVER WAS COMPILED WITH .W1 */
        ASSIGN curmod = fDir(z_proc.module-name). /* ENTRY(1,z_proc.module-name,"/"). */
        IF prevmod NE curmod THEN DO:
           RUN ReadDir (INPUT cDest-dir, INPUT prevMod, OUTPUT nbEntries).
           DISPLAY  prevmod 
                    i 
                    nbEntries 
                    i - nbEntries format ">>>,>>9-" LABEL "DIFFERENCE" WHEN i NE nbEntries
                    WITH FRAME Detail.
           DOWN 1 WITH FRAME Detail.
           ASSIGN prevmod = curmod i = 0.
        END.
        i = i + 1.
      END.
      ENABLE cDest-dir btnStart btnExit with three-D WITH FRAME fParamete.
      IF cReportFile:SCREEN-VALUE NE "screen" THEN OUTPUT CLOSE.

   END.

  WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW OR choose of btnExit FOCUS btnStart.


  /* PROCEDURE DEFINITIONS */

  PROCEDURE ReadDir.
    DEFINE INPUT  PARAMETER cDest-dir AS CHARACTER.
    DEFINE INPUT  PARAMETER curMod AS CHARACTER.
    DEFINE OUTPUT PARAMETER nbEntries AS INTEGER.
  
    DEFINE VARIABLE c-base AS CHARACTER.
    DEFINE VARIABLE c-type AS CHARACTER.
    DEFINE VARIABLE c-path AS CHARACTER.
  
    IF SEARCH(cDest-dir + "/" + curMod + "/_tag") = ? THEN LEAVE.
  
    INPUT STREAM dirdata FROM OS-DIR(cDest-dir + "/" + curMod) NO-ECHO .
 
    ReadDir-LOOP:
    REPEAT:
      IMPORT STREAM dirdata UNFORMATTED c-path.
      ASSIGN c-base = ENTRY(1,c-path," ")             
             c-type = ENTRY(3,c-path," ")
             c-path = ENTRY(2,c-path," ").           
      IF INDEX(c-base,".r") EQ 0 THEN NEXT.
      ASSIGN nbEntries = nbEntries + 1.
    END.
    INPUT STREAM dirdata CLOSE.
  END PROCEDURE.

PROCEDURE RptOutput.
  DEFINE INPUT-OUTPUT PARAMETER cReportFile as CHARACTER.  
  IF "{&WINDOW-SYSTEM}" <> "TTY" THEN DO:
    SYSTEM-DIALOG GET-FILE cReportFile
                  FILTERS "Source Files (*.log )" "*.log ",
                          "All Files (*.*)" "*.*"  
                  ASK-OVERWRITE
                  CREATE-TEST-FILE
                  USE-FILENAME    
                  SAVE-AS
                  MUST-EXIST
                  /*RETURN-TO-START-DIR */
                  TITLE "Choose a Report Log file"
                  UPDATE OKpressed.

  END.
  RETURN cReportFile.
END PROCEDURE.
