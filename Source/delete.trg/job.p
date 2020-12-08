&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

/* not delete if estimate exists */
FIND est WHERE est.rec_key = {&TABLENAME}.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
    {methods/triggers/delete.i}
END.
    
{sys/inc/var.i NEW SHARED}
ASSIGN
 cocode = {&TABLENAME}.company
 locode = {&TABLENAME}.loc.

/* moved to jc/v-{&TABLENAME}.w 
DEF VAR lv-msg AS CHAR NO-UNDO.


lv-msg = "".

IF {&TABLENAME}.stat EQ "A" THEN
  lv-msg = "This job has material allocated".

ELSE
IF {&TABLENAME}.stat EQ "W" then
  lv-msg = "This job has work-in-process".

ELSE
IF INDEX("CXZ", {&TABLENAME}.stat) GT 0 THEN
  lv-msg = "This job has been closed".

ELSE DO:
  FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ {&TABLENAME}.company
        AND oe-ordl.job-no  EQ {&TABLENAME}.job-no
        AND oe-ordl.job-no2 EQ {&TABLENAME}.job-no2
      NO-LOCK NO-ERROR.
  IF AVAIL oe-ordl THEN lv-msg = "An order exists with this Job#".
END.

IF lv-msg NE "" THEN DO:
  MESSAGE lv-msg + ", cannot delete..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.
*/
RUN Purge_SimulateOrDeleteRecordsByTable(
    INPUT  {&TABLENAME},
    INPUT  ROWID({&TABLENAME}),
    INPUT  YES,        /* Delete Records ? */
    INPUT  NO,         /* Create .csv files for child tables? */
    INPUT  YES,        /* Called from trigger?  */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ). 
    
/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.    
     
