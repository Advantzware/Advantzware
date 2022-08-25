/* copy of cerep/jobcarded.p   Xprint FC Factory  Ticket for Mclean */
/*------------------------------------------------------------------------
    File        : cerep/jobmclean.p
    Purpose     : 

    Syntax      :

    Description : print folding job ticket  

    Author(s)   : Sewa Singh 
    Created     : fri aug 9 19:29:35 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER v-format LIKE sys-ctrl.char-fld.
DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR NO-UNDO.

{api/ttAPIOutboundEvent.i}

FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.

{sys/inc/var.i shared}
{jcrep/r-ticket.i "shared"}

DEFINE VARIABLE v-job           AS CHARACTER FORMAT "x(9)" EXTENT 2 INIT [" ","zzzzzzzzz"].
DEFINE VARIABLE v-job2          AS INTEGER   FORMAT "999" EXTENT 2 INIT [000,999].
DEFINE VARIABLE v-reprint       AS LOG.
DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lIsAPIActive    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE li              AS INTEGER   NO-UNDO.

DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
ASSIGN
    v-job[1]    = fjob-no
    v-job[2]    = tjob-no
    v-job2[1]   = fjob-no2
    v-job2[2]   = tjob-no2
    v-reprint   = reprint
    .

DEFINE TEMP-TABLE ttEstCostHeaderID NO-UNDO
    FIELD estCostHeaderID AS INT64
    FIELD riJobHeader     AS ROWID
    .

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

RUN Outbound_IsApiClientAndScopeActive IN hdOutboundProcs (cocode, "", "JobTicket", v-format, "", "", "PrintJob", OUTPUT lIsAPIActive).
IF NOT lIsAPIActive THEN DO:
    MESSAGE "'JobTicket' API is not configured for '" + v-format + "' format"  
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

/* build tt-reftable */
FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company               EQ cocode
    AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
        TRIM(job-hdr.job-no) +
        STRING(job-hdr.job-no2,"999")  GE fjob-no
    AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
        TRIM(job-hdr.job-no) +
        STRING(job-hdr.job-no2,"999")  LE tjob-no
    AND job-hdr.job-no2 GE fjob-no2
    AND job-hdr.job-no2 LE tjob-no2
    AND (production OR
    job-hdr.ftick-prnt           EQ v-reprint OR
    PROGRAM-NAME(2) MATCHES "*r-tickt2*")
    AND CAN-FIND(FIRST job WHERE job.company EQ cocode
    AND job.job     EQ job-hdr.job
    AND job.job-no  EQ job-hdr.job-no
    AND job.job-no2 EQ job-hdr.job-no2
    AND job.stat    NE "H"
    AND (job.pr-printed EQ reprint OR
    NOT production))
    USE-INDEX job-no,

    FIRST est
    WHERE est.company  EQ job-hdr.company
    AND est.est-no   EQ job-hdr.est-no
    NO-LOCK

    BREAK BY job-hdr.job
    BY job-hdr.job-no
    BY job-hdr.job-no2
    BY job-hdr.frm: 

    FIND FIRST job
        WHERE job.company EQ cocode
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
        NO-LOCK NO-ERROR.

    IF production AND
        job.cs-trans-date NE ? THEN 
    DO:
        li = 0.
        DO WHILE li LT 1000:
            li = li + 1.
            FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE job THEN
                ASSIGN
                    job.pr-printed    = YES
                    job.pr-user-id-p  = USERID("nosweat")
                    job.pr-print-date = TODAY
                    job.pr-print-time = TIME
                    li                = 1000.
        END.
    END.

    ELSE 
    DO:
        li = 0.
        IF NOT job-hdr.ftick-prnt THEN 
        DO WHILE li LT 1000:
            li = li + 1.
            FIND FIRST bf-job-hdr EXCLUSIVE-LOCK
                 WHERE ROWID(bf-job-hdr) EQ ROWID(job-hdr)
                 NO-ERROR NO-WAIT.
            IF AVAILABLE bf-job-hdr THEN 
            DO:
                ASSIGN
                    bf-job-hdr.ftick-prnt = YES
                    li                    = 1000
                    .
            END.
            
            FIND CURRENT bf-job-hdr NO-LOCK NO-ERROR.
        END.

        li = 0.
        DO WHILE li LT 1000:
            li = li + 1.
            FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE job THEN 
            DO:
                li = 1000.

                IF NOT job.cs-printed THEN
                    ASSIGN
                        job.cs-printed    = YES
                        job.cs-user-id-p  = USERID("nosweat")
                        job.cs-print-date = TODAY
                        job.cs-print-time = TIME.

                IF approve THEN
                    ASSIGN
                        job.cs-to-pr      = YES
                        job.cs-user-id-t  = USERID("nosweat")
                        job.cs-trans-date = TODAY
                        job.cs-trans-time = TIME.
            END.
        END.
    END.
    
    /* Need to be re-factored later */
    FOR EACH estCostHeader NO-LOCK
         WHERE estCostHeader.company    EQ job-hdr.company
           AND estCostHeader.estimateNo EQ job-hdr.est-no
           AND estCostHeader.jobID      EQ job-hdr.job-no
           AND estCostHeader.jobid2     EQ job-hdr.job-no2
         BY estCostHeader.calcDateTime DESCENDING:
        LEAVE.
    END.
    IF NOT AVAILABLE estCostHeader THEN
        NEXT.
    
    FIND FIRST ttEstCostHeaderID NO-LOCK
         WHERE ttEstCostHeaderID.estCostHeaderID EQ estCostHeader.estCostHeaderID
         NO-ERROR.
    IF NOT AVAILABLE ttEstCostHeaderID THEN DO:
        CREATE ttEstCostHeaderID.
        ASSIGN
            ttEstCostHeaderID.estCostHeaderID = estCostHeader.estCostHeaderID
            ttEstCostHeaderID.riJobHeader     = ROWID(job-hdr)
            .    
    END.
END.
/* end of building tt-reftable */

RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
    INPUT  cocode,                             /* Company Code (Mandatory) */
    INPUT  "",                                /* Location Code (Mandatory) */
    INPUT  "JobTicket",                    /* API ID (Mandatory) */
    INPUT  "",                                     /* Scope ID */
    INPUT  "",                                     /* Scope Type */
    INPUT  "PrintJob",                         /* Trigger ID (Mandatory) */
    INPUT  "TTEstCostHeaderIDHandle",                           /* Comma separated list of table names for which data being sent (Mandatory) */
    INPUT  STRING(TEMP-TABLE ttEstCostHeaderID:HANDLE),                   /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
    INPUT  "Job Print",                          /* Primary ID for which API is called for (Mandatory) */   
    INPUT  "Job Card print",    /* Event's description (Optional) */
    OUTPUT lSuccess,                               /* Success/Failure flag */
    OUTPUT cMessage                                /* Status message */
    ).
                    
RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).

FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
IF AVAILABLE ttAPIOutboundEvent THEN DO:
    FIND FIRST apiOutboundEvent NO-LOCK
         WHERE apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
         NO-ERROR.
    IF AVAILABLE apiOutboundEvent THEN
        oplcRequestData = apiOutboundEvent.requestData.
END.    

FINALLY:
    DELETE PROCEDURE hdOutboundProcs.
END FINALLY.                                


/* **********************  Internal Procedures  *********************** */
