/* custom/prntbat.p   Print procedure in Batch */

DEF INPUT PARAM ipFileName AS CHAR NO-UNDO /* print file name */.
DEF INPUT PARAM ipFont AS INT NO-UNDO.
DEF INPUT PARAM ipOrientation AS CHAR NO-UNDO.
DEF INPUT PARAM ipCopies AS INT NO-UNDO.
DEF INPUT PARAM ipPrtname AS CHAR NO-UNDO.

{methods/defines/hndldefs.i}
{custom/gcompany.i}
IF NOT VALID-HANDLE(Persistent-Handle) THEN
RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
{custom/getcmpny.i}

DEF VAR batchName AS CHAR NO-UNDO.
DEF VAR pTitle AS CHAR NO-UNDO.
DEF VAR result AS LOG NO-UNDO.
DEF VAR lvOrnt AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.

/* if nothing to process/print... bail */
IF ipFileName EQ '' THEN RETURN.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ gcompany
       AND sys-ctrl.name EQ 'Batch' NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = gcompany
    sys-ctrl.name = 'Batch'
    sys-ctrl.descrip = 'Batch Output Destination'.
END. /* not avail */

IF sys-ctrl.log-fld THEN DO: /* batch destination turned on */
  ASSIGN
    batchName = SUBSTR(PROGRAM-NAME(2),1,LENGTH(PROGRAM-NAME(2)) - 1)
    i = R-INDEX(batchName,'/').
  IF i EQ 0 THEN i = R-INDEX(batchName,'\').
  IF i NE 0 THEN batchName = SUBSTR(batchName,i + 1).
  FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ batchName NO-ERROR.
  IF AVAILABLE prgrms THEN DO:
    ASSIGN
      pTitle = REPLACE(prgrms.prgtitle,' ','')
      pTitle = REPLACE(pTitle,'/','')
      pTitle = REPLACE(pTitle,'\','')
      pTitle = REPLACE(pTitle,',','')
      batchName = sys-ctrl.char-fld + '\' + pTitle + '.'
                + STRING(YEAR(TODAY),'9999')
                + STRING(MONTH(TODAY),'99')
                + STRING(DAY(TODAY),'99') + '.'
                + STRING(TIME,'99999').
    OS-COPY VALUE(ipFileName) VALUE(batchName).
  END.
END.
ELSE DO: /* batch destination turned off */
  /* SESSION:PRINTER-NAME = ip-prtname. NOT WORKDING */
  IF ipCopies EQ 0 THEN ipCopies = 1.
  lvOrnt = IF ipOrientation BEGINS 'L' THEN 2 ELSE 0.
  DO i = 1 TO ipCopies:
    RUN 'adecomm/_osprint.p'
      (INPUT ?,
       INPUT ipFileName,
       INPUT ipFont,
       INPUT lvOrnt,
       INPUT 0,
       INPUT 0,
       OUTPUT result).
  END. /* do i */
END. /* if log-fld */
