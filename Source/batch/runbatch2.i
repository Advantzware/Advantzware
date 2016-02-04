 /* batch/runbatch.i */ 
  
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  DEF VAR printok AS LOG NO-UNDO.
  DEF VAR v-print-name AS cha NO-UNDO.
  DEF VAR v-copies AS INT NO-UNDO.
  /*
  SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
   
  RUN custom/usrprtb.p
      ("{1}", FRAME {&FRAME-NAME}:HANDLE, SESSION:PRINTER-NAME, SESSION:PRINTER-PORT, CURRENT-WINDOW:TITLE).
  */

  /*  using prt-name or prt-port
  RUN custom/d-printB.w (OUTPUT v-print-name,OUTPUT v-copies). */

  RUN custom/d-printC.w (OUTPUT v-print-name,OUTPUT v-copies).
  MESSAGE "printer: " v-print-name VIEW-AS ALERT-BOX.
  RUN custom/usrprtb.p
      ("{1}", FRAME {&FRAME-NAME}:HANDLE, v-print-name,v-print-name, v-copies, CURRENT-WINDOW:TITLE).





/* prgsecdt.i  security for can-run,can-update,can_create and can_delete */

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE v-dirname LIKE b-prgrms.DIR_group NO-UNDO.
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

DEF VAR v-can-run AS LOG NO-UNDO.
DEF VAR v-can-update AS LOG NO-UNDO.
DEF VAR v-can-create AS LOG NO-UNDO.
DEF VAR v-can-delete AS LOG NO-UNDO.


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

  
FIND FIRST reftable WHERE reftable.reftable = "Batchrpt"
                        AND reftable.CODE = "{1}" NO-LOCK NO-ERROR.
IF NOT AVAIL reftable THEN DO:
     CREATE reftable.
     ASSIGN reftable.reftable = "Batchrpt"
            reftable.CODE = "{1}"
            reftable.code2 = IF AVAIL b-prgrms THEN b-prgrms.prgmname ELSE "".
END.

  MESSAGE "Procedure is scheduled..." VIEW-AS ALERT-BOX.
