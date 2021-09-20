&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPurgeProcs AS HANDLE NO-UNDO.

RUN util/purgeProcs.p PERSISTENT SET hPurgeProcs.

/* not delete if estimate exists */
FIND est WHERE est.rec_key = {&TABLENAME}.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
    {methods/triggers/delete.i}
END.
    
{sys/inc/var.i NEW SHARED}
ASSIGN
 cocode = {&TABLENAME}.company
 locode = {&TABLENAME}.loc.

{jc/jc-dall.i}

RUN Purge_SimulateAndPurgeJobRecords IN hPurgeProcs (
    BUFFER {&TABLENAME},
    INPUT  YES,        /* Delete Records ? */
    INPUT  NO,         /* Create .csv files for child tables? */
    INPUT  YES,        /* Called from trigger?  */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ). 
    
IF {&TABLENAME}.exported THEN DO:
  {&TABLENAME}.stat = "X".
  {jc/kiwiexp4.i}
END.    
/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.    
     
