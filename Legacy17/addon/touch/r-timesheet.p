DEFINE INPUT PARAMETER IpWeekEnding AS DATE NO-UNDO.
DEFINE INPUT PARAMETER IpEmployee AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER IpOTHr1 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpOTHr2 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpOTHr3 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpOTHr4 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpOTHr5 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpOTHr6 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpOTHr7 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpDTHr1 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpDTHr2 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpDTHr3 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpDTHr4 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpDTHr5 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpDTHr6 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpDTHr7 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpMakeHr1 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpMakeHr2 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpMakeHr3 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpMakeHr4 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpMakeHr5 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpMakeHr6 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpMakeHr7 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpPTOVacHr1 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpPTOVacHr2 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpPTOVacHr3 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpPTOVacHr4 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpPTOVacHr5 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpPTOVacHr6 AS DEC NO-UNDO.
DEFINE INPUT PARAMETER IpPTOVacHr7 AS DEC NO-UNDO.

DEFINE VARIABLE LvTotRegHrs AS INT NO-UNDO.
DEFINE VARIABLE LvTotOTHrs AS INT NO-UNDO.
DEFINE VARIABLE LvTotDTHrs AS INT NO-UNDO.
DEFINE VARIABLE LvTotMakeUpHrs AS INT NO-UNDO.
DEFINE VARIABLE LvTotPTOVacHrs AS INT NO-UNDO.
DEFINE NEW SHARED VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE v-cell               AS CHARACTER    NO-UNDO.
DEFINE VARIABLE t-dwg                AS CHAR         NO-UNDO.
DEFINE VARIABLE t-name               AS CHARACTER    NO-UNDO   FORMAT "x(40)".
DEFINE VARIABLE t-fnd                AS LOGICAL      NO-UNDO   INIT "False".
DEFINE VARIABLE t-seq                AS INTEGER      NO-UNDO.
DEFINE VARIABLE inRowCount           AS INTEGER      NO-UNDO   INITIAL 1.
DEFINE VARIABLE chFile               AS CHAR         NO-UNDO.
DEFINE VARIABLE LvLineCnt            AS INT          NO-UNDO.
DEFINE VARIABLE CurrDir              AS CHAR         NO-UNDO.
DEFINE VARIABLE LvCtr                as int          no-undo.
DEFINE VARIABLE CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE VARIABLE AdobePrinter         AS CHAR         NO-UNDO.
define variable CommandString        AS CHAR         NO-UNDO.
define variable WshNetwork           as com-handle.
DEFINE VARIABLE LvFirstTimePrint     AS LOGICAL      NO-UNDO   INIT no.
DEFINE VARIABLE vcTemplateFile       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE LvDate               AS DATE NO-UNDO. 
DEFINE VARIABLE j                    AS INT          NO-UNDO.
DEF VAR mypict AS COM-HANDLE.
DEF VAR v-makehr AS DEC EXTENT 7 NO-UNDO.
DEF VAR v-ptovachr AS DEC EXTENT 7 NO-UNDO.
DEF VAR v-othr AS DEC EXTENT 7 NO-UNDO.
DEF VAR v-dthr AS DEC EXTENT 7 NO-UNDO.

FIND FIRST TIMESHEET WHERE
     TIMESHEET.WEEKENDING = IpWeekending AND
     timesheet.employee = Ipemployee
     NO-LOCK NO-ERROR.

IF NOT AVAILABLE timesheet THEN
   RETURN.

FIND FIRST employee NO-LOCK WHERE
     employee.employee = timesheet.employee
     NO-ERROR.

IF NOT AVAILABLE employee THEN
DO:
    MESSAGE "No Employee record available." VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

  ASSIGN
    v-makehr[1] = IpMakeHr1
    v-makehr[2] = IpMakeHr2
    v-makehr[3] = IpMakeHr3
    v-makehr[4] = IpMakeHr4
    v-makehr[5] = IpMakeHr5
    v-makehr[6] = IpMakeHr6
    v-makehr[7] = IpMakeHr7
    v-ptovachr[1] = IpPTOVacHr1
    v-ptovachr[2] = IpPTOVacHr2
    v-ptovachr[3] = IpPTOVacHr3
    v-ptovachr[4] = IpPTOVacHr4
    v-ptovachr[5] = IpPTOVacHr5
    v-ptovachr[6] = IpPTOVacHr6
    v-ptovachr[7] = IpPTOVacHr7
    v-othr[1] = IpotHr1
    v-othr[2] = IpotHr2
    v-othr[3] = IpotHr3
    v-othr[4] = IpotHr4
    v-othr[5] = IpotHr5
    v-othr[6] = IpotHr6
    v-othr[7] = IpotHr7
    v-dthr[1] = IpdtHr1
    v-dthr[2] = IpdtHr2
    v-dthr[3] = IpdtHr3
    v-dthr[4] = IpdtHr4
    v-dthr[5] = IpdtHr5
    v-dthr[6] = IpdtHr6
    v-dthr[7] = IpdtHr7.

  RUN UTIL/CurrDir.p (output CurrDir).

  chFile = CurrDir + "\Template\TimeSheet.xlt".

  if chFile = ? then do:
    MESSAGE "Your Excel Template: " vcTemplateFile  skip
            "Please verify that the file exists."
      VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  end.
  
  /* Start a new session of Excel. */
  if not (valid-handle (chExcelApplication)) THEN
  DO:
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
    chExcelApplication:VISIBLE = TRUE.
  END.
  
  /* Check if Excel got initialized. */
  IF not (valid-handle (chExcelApplication)) THEN
  DO:
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  /* Name */
  v-cell = "R3C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = employee.FIRST_name + " " + employee.LAST_name.

  /* WeekEnding */
  v-cell = "R4C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN
     chExcelApplication:ActiveCell:Value = STRING(timesheet.weekending)
     inrowcount = 11.

  DO j = 1 TO 7 :        
    
    ASSIGN
       inrowcount = inrowcount + 1
       v-cell = "R" + STRING(inrowcount) + "C3".

    IF TimeSheet.TIME_Login1[j] <> "" THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = TimeSheet.TIME_Login1[j].
    END.

    ASSIGN v-cell = "R" + STRING(inrowcount) + "C4".
    IF TimeSheet.TIME_Logout1[j] <> "" THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = TimeSheet.TIME_Logout1[j].
    END.
    
    ASSIGN v-cell = "R" + STRING(inrowcount) + "C5".
    IF TimeSheet.TIME_Login2[j] <> "" THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = TimeSheet.TIME_Login2[j].
    END.
    ASSIGN v-cell = "R" + STRING(inrowcount) + "C6".
    IF TimeSheet.TIME_Logout2[j] <> "" THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = TimeSheet.TIME_Logout2[j].
    END.

    ASSIGN v-cell = "R" + STRING(inrowcount) + "C7".
    IF decimal(TimeSheet.reg_hrs[j]) > 0 THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = replace( STRING(TimeSheet.reg_hrs[j], ">9.99"), ".", ":").
    END.
    ASSIGN LvtotRegHrs = LvtotRegHrs +   
      ( TRUNCATE( TimeSheet.reg_hrs[j] , 0 ) * 3600 ) +
      ( (( TimeSheet.reg_hrs[j] - TRUNCATE( TimeSheet.reg_hrs[j] , 0 ) ) * 100 ) * 60).

    ASSIGN v-cell = "R" + STRING(inrowcount) + "C8".
    IF decimal(v-othr[j]) > 0 THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = replace( STRING(v-othr[j], ">9.99"), ".", ":").
    END.
    ASSIGN LvtotOTHrs = LvtotOTHrs +   
      ( TRUNCATE(v-othr[j], 0) * 3600 ) +
      ( ((v-othr[j] - TRUNCATE( v-othr[j],0)) * 100 ) * 60).
    
    ASSIGN v-cell = "R" + STRING(inrowcount) + "C9".
    IF decimal(v-dthr[j]) > 0 THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = replace( STRING(v-dthr[j], ">9.99"), ".", ":").
    END.
    ASSIGN LvtotDTHrs = LvtotDTHrs +   
      ( TRUNCATE(v-dthr[j], 0) * 3600 ) +
      ( ((v-dthr[j] - TRUNCATE(v-dthr[j], 0)) * 100) * 60).

    ASSIGN v-cell = "R" + STRING(inrowcount) + "C10".
    IF decimal(v-makehr[j]) > 0 THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = replace( STRING(v-makehr[j], ">9.99"), ".", ":").
    END.
    ASSIGN LvtotMakeUpHrs = LvtotMakeUpHrs +   
      ( TRUNCATE( v-makehr[j] , 0 ) * 3600 ) +
      ( (( v-makehr[j] - TRUNCATE( v-makehr[j], 0)) * 100 ) * 60).

    ASSIGN v-cell = "R" + STRING(inrowcount) + "C11".
    IF decimal(v-ptovachr[j]) > 0 THEN
    DO:
      chExcelApplication:Goto(v-cell) NO-ERROR.
      chExcelApplication:ActiveCell:Value = replace( STRING(v-ptovachr[j], ">9.99"), ".", ":").
    END.
    ASSIGN LvtotPTOVacHrs = LvtotPTOVacHrs +   
      ( TRUNCATE( v-ptovachr[j], 0) * 3600 ) +
      ( (( v-ptovachr[j] - TRUNCATE( v-ptovachr[j], 0 ) ) * 100 ) * 60).    
  END.
  ASSIGN v-cell = "R19" + "C7".
  IF LvtotRegHrs > 0 THEN
  DO:
     chExcelApplication:Goto(v-cell) NO-ERROR.
     chExcelApplication:ActiveCell:Value =  string( truncate(LvTotRegHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotRegHrs mod 3600) / 60,0), "99").
  END.

  ASSIGN v-cell = "R19" + "C8".
  IF LvtotOTHrs > 0 THEN
  DO:
     chExcelApplication:Goto(v-cell) NO-ERROR.
     chExcelApplication:ActiveCell:Value =  string( truncate(LvTotOTHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotOTHrs mod 3600) / 60,0), "99").
  END.
  ASSIGN v-cell = "R19" + "C9".
  IF LvtotDTHrs > 0 THEN
  DO:
     chExcelApplication:Goto(v-cell) NO-ERROR.
     chExcelApplication:ActiveCell:Value =  string( truncate(LvTotDTHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotDTHrs mod 3600) / 60,0), "99").
  END.

  ASSIGN v-cell = "R19" + "C10".
  IF LvtotMakeUpHrs > 0 THEN
  DO:
     chExcelApplication:Goto(v-cell) NO-ERROR.
     chExcelApplication:ActiveCell:Value =  string( truncate(LvTotMakeUpHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotMakeUpHrs mod 3600) / 60,0), "99").
  END.

  ASSIGN v-cell = "R19" + "C11".
  IF LvtotPTOVacHrs > 0 THEN
  DO:
     chExcelApplication:Goto(v-cell) NO-ERROR.
     ASSIGN chExcelApplication:ActiveCell:Value =  string( truncate(LvTotPTOVacHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotPTOVacHrs mod 3600) / 60,0), "99").
  END.
  
  /* Notes */
  v-cell = "R36C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = timesheet.notes.

  IF SEARCH(CurrDir + "\Signature\" + employee.employee + ".jpg") <> ? THEN
  DO:
    /* Employee Signature */
    chExcelApplication:Goto("R24C3") NO-ERROR.
    ASSIGN
       mypict = chExcelApplication:Range("C24"):Parent:Pictures:Insert(CurrDir + "\Signature\" + employee.employee + ".jpg")
       mypict:TOP = chExcelApplication:Range("C24"):TOP
       mypict:LEFT = chExcelApplication:Range("C24"):LEFT.
    RELEASE OBJECT mypict.
  END.
  /*  */
  
  chExcelApplication:Goto("R26C9") NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = string(timesheet.submittedOn).

  IF SEARCH(CurrDir + "\Signature\" + timesheet.ApprovedBy + ".jpg") <> ? THEN
  DO:
    /* Managers Signature */
    chExcelApplication:Goto("R28C3") NO-ERROR.
    ASSIGN
       mypict = chExcelApplication:Range("C28"):Parent:Pictures:Insert(CurrDir + "\Signature\" + timesheet.ApprovedBy + ".jpg")
       mypict:TOP = chExcelApplication:Range("C28"):TOP
       mypict:LEFT = chExcelApplication:Range("C28"):LEFT.
    RELEASE OBJECT mypict.
  END.
  /*  */
  chExcelApplication:Goto("R30C9") NO-ERROR.
  chExcelApplication:ActiveCell:Value = string(timesheet.ApprovedOn).
  chExcelApplication:Goto("R1c1") NO-ERROR.

  chExcelApplication:ActiveSheet:Protect("advance4me").

  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chHyper NO-ERROR.
