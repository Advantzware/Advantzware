
/*------------------------------------------------------------------------
    File        : getPoAdders.p
    Purpose     : 

    Syntax      :

    Description : Obtain adder codes and description for a PO line

    Author(s)   : Wade Kaldawi
    Created     : Tue Aug 08 14:07:29 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{po/getPoAdders.i}
DEFINE INPUT  PARAMETER iprPoOrdlRow AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ttPoAdders.

DEFINE VARIABLE addersText AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cost     AS DECIMAL NO-UNDO.
DEFINE BUFFER xjob-mat FOR job-mat.


/* ***************************  Main Block  *************************** */
FIND FIRST po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ iprPoOrdlRow
 NO-ERROR.
 IF AVAILABLE po-ordl THEN DO: 
   
    IF po-ordl.job-no NE "" THEN 
    DO:
        RELEASE xjob-mat.
        FIND FIRST job NO-LOCK 
            WHERE job.company EQ po-ordl.company
            AND job.job-no  EQ po-ordl.job-no
            AND job.job-no2 EQ INT(po-ordl.job-no2)
            NO-ERROR.
        IF AVAILABLE job THEN
            FIND FIRST xjob-mat NO-LOCK 
                WHERE xjob-mat.company  EQ job.company
                AND xjob-mat.job      EQ job.job
                AND xjob-mat.job-no   EQ job.job-no
                AND xjob-mat.job-no2  EQ job.job-no2
                AND xjob-mat.frm      EQ INT(po-ordl.s-num)
                AND xjob-mat.blank-no EQ INT(po-ordl.b-num)
                AND xjob-mat.rm-i-no  EQ po-ordl.i-no
                NO-ERROR.
        IF AVAILABLE xjob-mat THEN 
        DO:
               
            ASSIGN
                addersText = ''.

            FOR EACH job-mat NO-LOCK
                WHERE job-mat.company  EQ xjob-mat.company
                AND job-mat.job      EQ xjob-mat.job
                AND job-mat.frm      EQ xjob-mat.frm
                AND job-mat.job-no   EQ xjob-mat.job-no
                AND job-mat.job-no2  EQ xjob-mat.job-no2
                USE-INDEX seq-idx,

                FIRST item NO-LOCK
                WHERE item.company  EQ job-mat.company
                AND item.i-no     EQ job-mat.i-no
                AND item.mat-type EQ "A":
                CREATE ttPoAdders.
                ASSIGN ttPoAdders.poLineNumber     = po-ordl.line 
                       ttPoAdders.adderCode    = ITEM.i-no 
                       ttPoAdders.adderDescription = item.i-name
                       .                     
                 
            END. /* each job-mat */          
            
        END. /* if avail job-mat */
    END. /* if job-no gt "" */
END. /* IF AVAILABLE po-ordl */