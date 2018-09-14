 /* batch/runbatch.i */ 
  
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  DEFINE BUFFER b-prgrms FOR prgrms.

  DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
  DEFINE VARIABLE v-dirname LIKE b-prgrms.DIR_group NO-UNDO.
  DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
  DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
  DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
  DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.
  DEFINE VARIABLE v-can-run AS LOG NO-UNDO.
  DEFINE VARIABLE v-can-update AS LOG NO-UNDO.
  DEFINE VARIABLE v-can-create AS LOG NO-UNDO.
  DEFINE VARIABLE v-can-delete AS LOG NO-UNDO.
  DEFINE VARIABLE printok AS LOG NO-UNDO.
  DEFINE VARIABLE v-print-name AS cha NO-UNDO.
  DEFINE VARIABLE v-copies AS INT INIT 1 NO-UNDO.
  DEFINE VARIABLE v-startDate AS DATE NO-UNDO.
  DEFINE VARIABLE v-startTime AS INT NO-UNDO.
  DEFINE VARIABLE v-endDate AS DATE NO-UNDO.
  DEFINE VARIABLE v-endTime AS INT NO-UNDO.
  DEFINE VARIABLE v-dayOfWeek AS LOG NO-UNDO EXTENT 7.
  DEFINE VARIABLE v-repeatWeekly AS LOG NO-UNDO.
  DEFINE VARIABLE v-continue AS LOG NO-UNDO.

  IF rd-dest NE 5 THEN
  /* RUN custom/d-printB.w (OUTPUT v-print-name,OUTPUT v-copies). */
     RUN batch/batParam.w (OUTPUT v-print-name,
                           OUTPUT v-copies,
                           OUTPUT v-startDate,OUTPUT v-startTime,
                           OUTPUT v-endDate,OUTPUT v-endTime,
                           OUTPUT v-dayOfWeek[1],
                           OUTPUT v-dayOfWeek[2],
                           OUTPUT v-dayOfWeek[3],
                           OUTPUT v-dayOfWeek[4],
                           OUTPUT v-dayOfWeek[5],
                           OUTPUT v-dayOfWeek[6],
                           OUTPUT v-dayOfWeek[7],
                           OUTPUT v-repeatWeekly,
                           OUTPUT v-continue).
IF v-continue THEN DO:
  RUN custom/usrprtb.p
      ("{1}",
       FRAME {&FRAME-NAME}:HANDLE,
       v-print-name,v-print-name,v-copies,
       v-startDate,v-startTime,
       v-endDate,v-endTime,
       v-dayOfWeek[1],
       v-dayOfWeek[2],
       v-dayOfWeek[3],
       v-dayOfWeek[4],
       v-dayOfWeek[5],
       v-dayOfWeek[6],
       v-dayOfWeek[7],
       v-repeatWeekly,
       CURRENT-WINDOW:TITLE).

  IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
     INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
     INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN v-prgmname = USERID("NOSWEAT") + "..".
  ELSE DO:
     v-prgmname = PROGRAM-NAME(1).
  
     IF INDEX(v-prgmname,"\") > 0 THEN v-prgmname = REPLACE(v-prgmname,"\","/").
  
    ASSIGN
    period_pos = INDEX(PROGRAM-NAME(1),".")
    v-dirname = IF INDEX(v-prgmname,"/") > 0 THEN SUBSTRING(v-prgmname,1,INDEX(v-prgmname,"/") - 1) ELSE ""
    v-prgmname = IF INDEX(v-prgmname,"/") > 0 THEN SUBSTR(v-prgmname,INDEX(v-prgmname,"/") + 1) ELSE v-prgmname
    
    /*v-prgmname = substring(PROGRAM-NAME(1),1,period_pos)*/
    period_pos = INDEX(v-prgmname,".")
    v-prgmname = substring(v-prgmname,1,period_pos).
  END.
  
  FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname AND
                      b-prgrms.DIR_group = v-dirname NO-LOCK NO-ERROR.
  IF NOT AVAIL b-prgrms THEN FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname NO-LOCK NO-ERROR.
  
    

   FIND FIRST user-print WHERE user-print.program-id = "{1}"
                           AND user-print.company = cocode
                           AND user-print.batch = "" NO-LOCK NO-ERROR.
   IF AVAIL user-print THEN DO:
       ASSIGN user-print.prgmName = IF AVAIL b-prgrms THEN b-prgrms.prgmname ELSE "". 
       
   END.     


  MESSAGE "Procedure is scheduled..." VIEW-AS ALERT-BOX.
END. /* if v-continue */
