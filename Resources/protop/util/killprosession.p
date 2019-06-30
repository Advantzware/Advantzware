/*****************************************************************************/
/*                                                                           */
/*    Program        :  killprosession.p                                     */ 
/*                                                                           */
/*    Purpose        :  Monitors a user session that is going to be killed   */
/*                                                                           */
/*    Param          :  "( CONNECTED | MONITOR ) UserNum                     */
/*                      Example param: "CONNECTED 5"                         */
/*                                                                           */
/*****************************************************************************/

DEFINE VARIABLE pUserNum  AS INT  NO-UNDO.
DEFINE VARIABLE pPID      AS INT  NO-UNDO.
DEFINE VARIABLE pFcn      AS CHAR NO-UNDO.
DEFINE VARIABLE PSLine    AS CHAR NO-UNDO.
DEFINE VARIABLE PauseSec  AS INT NO-UNDO. /* Seconds to pause between loops */
DEFINE VARIABLE TotalSec  AS INT NO-UNDO. /* Total seconds to loop */
DEFINE VARIABLE Cnt       AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE Time1     AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE Time2     AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE DBIO1     AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE DBIO2     AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE DeadCnt   AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE DeadIO    AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE Disconct  AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE InTrans   AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE InRollBk  AS LOG INIT NO NO-UNDO.

DEFINE VARIABLE LatchList     AS CHAR INIT "" NO-UNDO.
DEFINE VARIABLE OldLatchList  AS CHAR INIT "" NO-UNDO.

/* Param 2 is either the user num or the PID, depending on param one */
Assign pUserNum   = INT(ENTRY(2,SESSION:PARAM," ")) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
DO:
  MESSAGE "ERROR:" ERROR-STATUS:GET-MESSAGE(1).
  QUIT.
END.
Assign pPID       = pUserNum.

Assign pFcn = ENTRY(1,SESSION:PARAM," ") NO-ERROR.
IF pFcn <> "MONITOR" AND pFcn <> "CONNECTED" THEN
DO:
   MESSAGE "ERROR: INVALID PARAMETER: " pFcn.
   QUIT.
END.

Assign PauseSec  = 10
       TotalSec  = 600.

/* If the program was called with Param 1 = CONNECTED */
/* just return usernum and quit. */
IF pFcn = "CONNECTED" THEN
DO:
   FIND DICTDB._Connect WHERE DICTDB._Connect._Connect-Pid = pPID 
        NO-LOCK NO-ERROR.
   IF AVAILABLE DICTDB._Connect THEN
      MESSAGE DICTDB._Connect._Connect-Usr.
   ELSE
     MESSAGE "NOT_CONNECTED".

   QUIT.
END.

/**
    We wait for the process to end on its own up to TotalSec seconds
    BUT
    we make sure the process is actually doing something.
**/

DO WHILE Cnt < TotalSec: 
  FIND DICTDB._Connect WHERE DICTDB._Connect._Connect-Usr = pUserNum 
       NO-LOCK NO-ERROR.

  IF NOT AVAIL DICTDB._Connect THEN
  DO:
     MESSAGE "NOT_CONNECTED".
     QUIT.
  END.

  IF DICTDB._Connect._Connect-Disconnect <> 0 THEN
  DO:
     ASSIGN Disconct = YES.
     MESSAGE "INFO    : User disconnect already initiated".
  END.
  ELSE
     MESSAGE "WARNING : No user disconnect seems to have been initiated".

  IF DICTDB._Connect._Connect-TransID <> 0 THEN
  DO:
     Assign InTrans = YES.
     MESSAGE "INFO    : Process in transaction " 
             DICTDB._Connect._Connect-TransID.
  END.
  ELSE
     MESSAGE "INFO    : Process NOT in transaction ". 

  IF DICTDB._Connect._Connect-Resync = 1 THEN
  DO:
     ASSIGN InRollBk = YES.
     MESSAGE "CRITICAL: Process rolling back transaction - do not kill!" .
  END.
  ELSE
     MESSAGE "INFO    : Process NOT attempting to roll back a transaction".

  IF DICTDB._Connect._Connect-Interrupt <> 0 THEN
     MESSAGE "INFO    : Process interrupted by signal".

  IF TRIM(DICTDB._Connect._Connect-Wait) <> "--" AND 
     DICTDB._Connect._Connect-Wait1 <> 0 THEN
  DO: 
     MESSAGE "CRITICAL: Process waiting for resource :" 
     DICTDB._Connect._Connect-Wait.
  END.
  /** 
     Check if the user may be holding latches.
     WARNING: _Latch shows the LAST user to hold the latch.  THIS DOES
              NOT NECESSARILY MEAN THE USER IS HOLDING THE LATCH NOW!!!! 
  **/
  ASSIGN OldLatchList = LatchList.
            LatchList = ""
            .
  FOR EACH DICTDB._Latch WHERE DICTDB._Latch._Latch-hold = pUserNum NO-LOCK:
     Assign LatchList = LatchList + STRING(DICTDB._Latch._Latch-ID) + " ".
  END.
  MESSAGE "INFO    : Old Latch List: " OldLatchList.
  MESSAGE "INFO    : New Latch List: " LatchList.

  /** Check if the process is actually reading/writing to the db **/
  FIND DICTDB._UserIO WHERE DICTDB._UserIO._UserIO-Usr = pUserNum 
       NO-LOCK NO-ERROR.
  IF AVAILABLE DICTDB._UserIO THEN
  DO:
     Assign DBIO2 = DICTDB._UserIO._UserIO-DbAccess + 
                    DICTDB._UserIO._UserIO-BiRead   +
                    DICTDB._UserIO._UserIO-BiWrite.
     MESSAGE "INFO    : Database I/O in last" PauseSec "seconds :" 
             (DBIO2 - DBIO1).
  END.
  IF DBIO2 > DBIO1 THEN
     ASSIGN DBIO1  = DBIO2
            DeadIO = 0
            .
  ELSE
     Assign DeadIO = DeadIO + 1.

  /** Check if the process is consuming CPU time **/
  INPUT THROUGH "ps -o ~"time=~" -p" VALUE(DICTDB._Connect._Connect-Pid) 
        NO-ECHO.
  SET PSLine.
  INPUT CLOSE.
  Time2 = (INT(ENTRY(1,PSLine,":")) * 3600) + 
          (INT(ENTRY(2,PSLine,":")) * 60) +
           INT(ENTRY(3,PSLine,":")).
   
  MESSAGE "INFO    : CPU Time in last" PauseSec "seconds :" 
          (Time2 - Time1) "secs".
  IF Time2 > Time1 THEN
     ASSIGN Time1   = Time2
            DeadCnt = 0 
            .
  ELSE
     DeadCnt = DeadCnt + 1.

  IF DeadCnt >= 5  AND DeadIO >= 5 THEN
  DO:
    MESSAGE "INFO    : No change in CPU use or DB I/O".
    MESSAGE  "NO_CPU_OR_DBIO".
    QUIT.
  END.
  IF Disconct = NO AND InTrans = NO AND InRollBk = NO THEN
  DO:
    MESSAGE "INFO    : Process seems stable".
    MESSAGE  "STABLE_PROCESS".
    QUIT.
  END.

  /** Wait and loop **/
  MESSAGE "".
  MESSAGE "********************* SLEEP" PauseSec "seconds *****************".
  MESSAGE "".
  PAUSE PauseSec.
  Cnt = Cnt + PauseSec.
END.

/** 
   TotalSec seconds are up and the process is still alive.  We quit with
   the appropriate msg based on whether or not the process is still
   rolling back.
**/
FIND DICTDB._Connect WHERE DICTDB._Connect._Connect-Usr = pUserNum 
     NO-LOCK NO-ERROR.

IF NOT AVAIL DICTDB._Connect THEN
DO:
   MESSAGE "NOT_CONNECTED".
   QUIT.
END.

IF DICTDB._Connect._Connect-Resync = 1 THEN
DO:
   MESSAGE "CRITICAL: Process still rolling back actively".
   MESSAGE "CRITICAL: Rerun this process to continue waiting".
   MESSAGE "TIMEOUT_RESYNC".
END.
ELSE
DO:
   MESSAGE "INFO    : Still waiting - process active.".
   MESSAGE "TIMEOUT".
END.

QUIT.




