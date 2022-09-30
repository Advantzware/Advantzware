/*------------------------------------------------------------------------
  File:         tsPost.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 9.20.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttbl_pc-prdd
DEFINE TEMP-TABLE ttbl_pc-prdd NO-UNDO LIKE pc-prdd
    INDEX ttbl_pc-prdd IS PRIMARY
    company m-code op-date shift job frm blank-no
    .
DEFINE TEMP-TABLE ttbl_pc-prdh NO-UNDO LIKE pc-prdh
    INDEX ttbl_pc-prdh IS PRIMARY
    company m-code trans-date shift
    .
DEFINE TEMP-TABLE ttbl_rowid NO-UNDO
    FIELD pc-prdd_rowid AS ROWID
    FIELD total_time    AS INTEGER
    .
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.
DEFINE TEMP-TABLE w-job NO-UNDO
    FIELD job LIKE job.job
    .
{pc/pcprdd4u.i NEW}
{jc/jcgl-sh.i NEW}

DEFINE TEMP-TABLE ttNoCrew NO-UNDO
    FIELD noCrew AS CHARACTER FORMAT "x(80)" LABEL "Description"
    .
RUN spSetSessionParam ("SummaryTables", "1").
RUN spSetSessionParam ("SummaryHandle1", TEMP-TABLE ttNoCrew:HANDLE).
RUN spSetSessionParam ("SummaryTitle1", "No Crew").

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 213
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cTSPost   AS CHARACTER NO-UNDO.
DEFINE VARIABLE hJobProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lDCPostGL AS LOGICAL   NO-UNDO.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cINo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTSPostFG    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTSSecure    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dShiftPct    AS DECIMAL   NO-UNDO.    
    DEFINE VARIABLE iTotalTime   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTSPostFG    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lAutoPost    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTSPost      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTSPostFG    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTSSecure    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidToPost AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (
        cCompany,"TSPostFG","I",NO,NO,"","",
        OUTPUT cTSPostFG,OUTPUT lTSPostFG
        ).
    iTSPostFG = INTEGER(cTSPostFG).
    RUN sys/ref/nk1look.p (
        cCompany,"TSPostFG","L",NO,NO,"","",
        OUTPUT cTSPostFG,OUTPUT lTSPostFG
        ).
    lTSPostFG = cTSPostFG EQ "YES".
    RUN sys/ref/nk1look.p (
        cCompany,"TSPostFG","C",NO,NO,"","",
        OUTPUT cTSPostFG,OUTPUT lTSPostFG
        ).
    RUN sys/ref/nk1look.p (
        cCompany,"AutoPost","L",NO,NO,"","",
        OUTPUT cTSPost,OUTPUT lAutoPost
        ).
    lAutoPost = cTSPost EQ "YES".
    RUN sys/ref/nk1look.p (
        cCompany,"DCPostGL","L",NO,NO,"","",
        OUTPUT cTSPost,OUTPUT lDCPostGL
        ).
    lDCPostGL = cTSPost EQ "YES".
    RUN sys/ref/nk1look.p (
        cCompany,"TSPost","L",NO,NO,"","",
        OUTPUT cTSPost,OUTPUT lTSPost
        ).
    lTSPost = cTSPost EQ "YES".
    RUN sys/ref/nk1look.p (
        cCompany,"TSPost","C",NO,NO,"","",
        OUTPUT cTSPost,OUTPUT lTSPost
        ).
    RUN sys/ref/nk1look.p (
        cCompany,"TSSecure","L",NO,NO,"","",
        OUTPUT cTSSecure,OUTPUT lTSSecure
        ).
    lTSSecure = cTSSecure EQ "YES".
    RUN sys/ref/TSSecure.p (
        cCompany,"TSPost","C",NO,NO,"","",
        OUTPUT cTSSecure,OUTPUT lTSSecure
        ).
    RUN jc/Jobprocs.p PERSISTENT SET hJobProcs.
    FOR EACH ttbl_pc-prdh:    
        DELETE ttbl_pc-prdh.
    END.
    FOR EACH ttbl_pc-prdd:
        DELETE ttbl_pc-prdd.
    END.
    FOR EACH mach FIELDS(m-code dept mr-rate run-rate) NO-LOCK
        WHERE mach.company EQ cCompany
          AND mach.m-code  GE cStartMachine
          AND mach.m-code  LE cEndMachine,
        EACH machtran NO-LOCK
        WHERE machtran.company    EQ mach.company
          AND machtran.machine    EQ mach.m-code
          AND machtran.job_number GE cStartJobNo
          AND machtran.job_number LE cEndJobNo
          AND machtran.job_sub    GE iStartJobNo2
          AND machtran.job_sub    LE iEndJobNo2
          AND machtran.posted     EQ NO
        :
        IF NOT(((machtran.end_date GE dtStartDate AND
                 machtran.end_date LE dtEndDate)  AND
                (machtran.shift    GE cStartShift AND
                 machtran.shift    LE cEndShift))  OR
                CAN-FIND(FIRST mach
                         WHERE mach.company      EQ machtran.company
                           AND mach.m-code       EQ machtran.machine
                           AND mach.industry     EQ "X"))
                            OR machtran.end_date EQ ? THEN
        NEXT.
        lValidToPost = YES.
        IF machtran.start_date EQ machtran.end_date THEN DO:
            IF NOT CAN-FIND(FIRST ttbl_pc-prdh
                            WHERE ttbl_pc-prdh.company    EQ machtran.company
                              AND ttbl_pc-prdh.m-code     EQ machtran.machine
                              AND ttbl_pc-prdh.shift      EQ INTEGER(machtran.shift)
                              AND ttbl_pc-prdh.trans-date EQ machtran.end_date) THEN DO:
                CREATE ttbl_pc-prdh.
                ASSIGN
                    ttbl_pc-prdh.company    = machtran.company
                    ttbl_pc-prdh.m-code     = machtran.machine
                    ttbl_pc-prdh.shift      = INTEGER(machtran.shift)
                    ttbl_pc-prdh.trans-date = machtran.end_date
                    ttbl_pc-prdh.user-id    = USERID('ASI')
                    ttbl_pc-prdh.dept       = mach.dept[1]
                    .
            END.
            RUN jc/GetItemFromJob.p (
                machtran.company,
                machtran.job_number,
                machtran.job_sub,
                machtran.form_number,
                machtran.blank_number,
                mach.m-code,
                OUTPUT cINo,
                OUTPUT cIName
                ).
            FIND FIRST job NO-LOCK
                 WHERE job.company EQ machtran.company
                   AND job.job-no  EQ machtran.job_number
                   AND job.job-no2 EQ machtran.job_sub
                 NO-ERROR.
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company  EQ machtran.company
                   AND job-hdr.job-no   EQ machtran.job_number
                   AND job-hdr.job-no2  EQ machtran.job_sub
                   AND job-hdr.frm      EQ machtran.form_number
                   AND job-hdr.blank-no EQ machtran.blank_number
                 NO-ERROR.
            IF NOT AVAIL job-hdr THEN
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ machtran.company
                   AND job-hdr.job-no  EQ machtran.job_number
                   AND job-hdr.job-no2 EQ machtran.job_sub
                   AND job-hdr.frm     EQ machtran.form_number
                 NO-ERROR.
            FIND FIRST job-mch NO-LOCK
                 WHERE job-mch.company  EQ machtran.company
                   AND job-mch.job-no   EQ machtran.job_number
                   AND job-mch.job-no2  EQ machtran.job_sub
                   AND job-mch.frm      EQ machtran.form_number
                   AND job-mch.blank-no EQ machtran.blank_number
                 NO-ERROR.
            IF NOT AVAIL job-mch THEN
            FIND FIRST job-mch NO-LOCK
                 WHERE job-mch.company EQ machtran.company
                   AND job-mch.job-no  EQ machtran.job_number
                   AND job-mch.job-no2 EQ machtran.job_sub
                   AND job-mch.frm     EQ machtran.form_number
                 NO-ERROR.
            IF AVAILABLE job-hdr AND NOT AVAIL job-mch THEN
            FIND FIRST job-mch NO-LOCK
                 WHERE job-mch.company EQ machtran.company
                   AND job-mch.j-no    EQ job-hdr.j-no
                   AND job-mch.i-no    EQ job-hdr.i-no
                   AND job-mch.m-code  EQ machtran.machine
                 NO-ERROR.
            FIND FIRST ttbl_pc-prdd
                 WHERE ttbl_pc-prdd.company  EQ machtran.company
                   AND ttbl_pc-prdd.m-code   EQ machtran.machine
                   AND ttbl_pc-prdd.job-no   EQ machtran.job_number
                   AND ttbl_pc-prdd.job-no2  EQ machtran.job_sub
                   AND ttbl_pc-prdd.frm      EQ machtran.form_number
                   AND ttbl_pc-prdd.blank-no EQ machtran.blank_number
                   AND ttbl_pc-prdd.pass     EQ machtran.pass_sequence
                   AND ttbl_pc-prdd.i-no     EQ cINo
                   AND ttbl_pc-prdd.code     EQ machtran.charge_code
                   AND ttbl_pc-prdd.op-date  EQ machtran.end_date
                   AND ttbl_pc-prdd.start    EQ machtran.start_time
                   AND ttbl_pc-prdd.stopp    EQ machtran.end_time
                   AND ttbl_pc-prdd.shift    EQ INTEGER(machtran.shift)
                NO-ERROR.
            IF NOT AVAILABLE ttbl_pc-prdd THEN DO:
                CREATE ttbl_pc-prdd.
                ASSIGN
                    ttbl_pc-prdd.company  = machtran.company
                    ttbl_pc-prdd.m-code   = machtran.machine
                    ttbl_pc-prdd.job-no   = machtran.job_number
                    ttbl_pc-prdd.job-no2  = machtran.job_sub
                    ttbl_pc-prdd.frm      = machtran.form_number
                    ttbl_pc-prdd.blank-no = machtran.blank_number
                    ttbl_pc-prdd.pass     = machtran.pass_sequence
                    ttbl_pc-prdd.i-no     = cINo
                    ttbl_pc-prdd.i-name   = cIName
                    ttbl_pc-prdd.code     = machtran.charge_code
                    ttbl_pc-prdd.op-date  = machtran.end_date
                    ttbl_pc-prdd.start    = machtran.start_time
                    ttbl_pc-prdd.stopp    = machtran.end_time
                    ttbl_pc-prdd.startx   = SUBSTRING(STRING(ttbl_pc-prdd.start,"hh:mm"),1,2)
                                          + SUBSTRING(STRING(ttbl_pc-prdd.start,"hh:mm"),4,2)
                    ttbl_pc-prdd.stopx    = SUBSTRING(STRING(ttbl_pc-prdd.stopp,"hh:mm"),1,2)
                                          + SUBSTRING(STRING(ttbl_pc-prdd.stopp,"hh:mm"),4,2)
                    ttbl_pc-prdd.shift    = INTEGER(machtran.shift)
                    ttbl_pc-prdd.complete = machtran.completed
                    ttbl_pc-prdd.dept     = mach.dept[1]
                    ttbl_pc-prdd.hours    = machtran.total_time / 3600
                    ttbl_pc-prdd.j-no     = IF AVAILABLE job-hdr THEN job-hdr.j-no ELSE 0
                    ttbl_pc-prdd.job      = job.job
                    ttbl_pc-prdd.speed    = IF AVAILABLE job-mch THEN job-mch.speed ELSE 0
                    iTotalTime            = iTotalTime + machtran.total_time
                    .
                CREATE ttbl_rowid.
                ASSIGN
                    ttbl_rowid.pc-prdd_rowid = ROWID(ttbl_pc-prdd)
                    ttbl_rowid.total_time    = machtran.total_time
                    .
            END.
            IF lTSPost AND cTSPost EQ "Actual" THEN DO:
                ASSIGN 
                    ttbl_pc-prdd.crew     = 0
                    ttbl_pc-prdd.complete = IF lAutoPost THEN YES ELSE NO
                    .
                FOR EACH machemp NO-LOCK
                    WHERE machemp.table_rec_key = machtran.rec_key
                    :
                    ASSIGN 
                        ttbl_pc-prdd.crew                               = ttbl_pc-prdd.crew + 1
                        ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee 
                        ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)]   = machemp.rate
                        .            
                END. 
                IF ttbl_pc-prdd.crew EQ 0 THEN
                    IF lDoNotShowNoCrewMessage EQ NO THEN
                    RUN pNoCrew.
            END.
            ELSE DO:
                ttbl_pc-prdd.crew = 0.
                FOR EACH machemp NO-LOCK
                    WHERE machemp.table_rec_key = machtran.rec_key
                    :
                    ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
                END.
                IF ttbl_pc-prdd.crew EQ 0 THEN DO:
                    IF lDoNotShowNoCrewMessage EQ NO THEN
                    RUN pNoCrew.
                END.
                ELSE DO:
                    FIND FIRST job-code NO-LOCK
                         WHERE job-code.code EQ ttbl_pc-prdd.code
                         NO-ERROR.
                    IF AVAILABLE job-code AND job-code.cat EQ "MR" THEN
                    ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.mr-rate. 
                    ELSE IF AVAILABLE job-code AND job-code.cat EQ "RUN" THEN
                         ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.run-rate. 
                END.
            END.
        END.
        ELSE DO: /* Start Date and End Date are Different. Need two records in Advantzware */        
            IF NOT CAN-FIND(ttbl_pc-prdh
                            WHERE ttbl_pc-prdh.company    EQ machtran.company
                              AND ttbl_pc-prdh.m-code     EQ machtran.machine
                              AND ttbl_pc-prdh.shift      EQ INTEGER(machtran.shift)
                              AND ttbl_pc-prdh.trans-date EQ machtran.start_date) THEN DO:
                CREATE ttbl_pc-prdh.
                ASSIGN
                    ttbl_pc-prdh.company    = machtran.company
                    ttbl_pc-prdh.m-code     = machtran.machine
                    ttbl_pc-prdh.shift      = INTEGER(machtran.shift)
                    ttbl_pc-prdh.trans-date = machtran.start_date
                    ttbl_pc-prdh.user-id    = USERID('ASI')
                    ttbl_pc-prdh.dept       = mach.dept[1]
                    .
            END.
            RUN jc/GetItemFromJob.p (
                machtran.company,
                machtran.job_number,
                machtran.job_sub,
                machtran.form_number,
                machtran.blank_number,
                mach.m-code,
                OUTPUT cINo,
                OUTPUT cIName
                ).
            FIND FIRST job NO-LOCK
                 WHERE job.company EQ machtran.company
                   AND job.job-no  EQ machtran.job_number
                   AND job.job-no2 EQ machtran.job_sub
                 NO-ERROR.
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company  EQ machtran.company
                   AND job-hdr.job-no   EQ machtran.job_number
                   AND job-hdr.job-no2  EQ machtran.job_sub
                   AND job-hdr.frm      EQ machtran.form_number
                   AND job-hdr.blank-no EQ machtran.blank_number
                 NO-ERROR.
            IF NOT AVAIL job-hdr THEN
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ machtran.company
                   AND job-hdr.job-no  EQ machtran.job_number
                   AND job-hdr.job-no2 EQ machtran.job_sub
                   AND job-hdr.frm     EQ machtran.form_number
                 NO-ERROR.
            FIND FIRST job-mch NO-LOCK
                 WHERE job-mch.company  EQ machtran.company
                   AND job-mch.job-no   EQ machtran.job_number
                   AND job-mch.job-no2  EQ machtran.job_sub
                   AND job-mch.frm      EQ machtran.form_number
                   AND job-mch.blank-no EQ machtran.blank_number
                 NO-ERROR.
            IF NOT AVAIL job-mch THEN
            FIND FIRST job-mch NO-LOCK
                 WHERE job-mch.company EQ machtran.company
                   AND job-mch.job-no  EQ machtran.job_number
                   AND job-mch.job-no2 EQ machtran.job_sub
                   AND job-mch.frm     EQ machtran.form_number
                 NO-ERROR.
            IF AVAILABLE job-hdr AND NOT AVAIL job-mch THEN
            DO:
                FIND FIRST job-mch WHERE job-mch.company = machtran.company
                    AND job-mch.j-no = job-hdr.j-no
                    AND job-mch.i-no = job-hdr.i-no
                    AND job-mch.m-code = machtran.machine
                    NO-LOCK NO-ERROR.
            END.
            FIND FIRST ttbl_pc-prdd
                 WHERE ttbl_pc-prdd.company  EQ machtran.company
                   AND ttbl_pc-prdd.m-code   EQ machtran.machine
                   AND ttbl_pc-prdd.job-no   EQ machtran.job_number
                   AND ttbl_pc-prdd.job-no2  EQ machtran.job_sub
                   AND ttbl_pc-prdd.frm      EQ machtran.form_number
                   AND ttbl_pc-prdd.blank-no EQ machtran.blank_number
                   AND ttbl_pc-prdd.pass     EQ machtran.pass_sequence
                   AND ttbl_pc-prdd.i-no     EQ cINo
                   AND ttbl_pc-prdd.code     EQ machtran.charge_code
                   AND ttbl_pc-prdd.op-date  EQ machtran.start_date
                   AND ttbl_pc-prdd.start    EQ machtran.start_time
                   AND ttbl_pc-prdd.stopp    EQ 86400
                   AND ttbl_pc-prdd.shift    EQ INTEGER(machtran.shift)
                 NO-ERROR.
            IF NOT AVAILABLE ttbl_pc-prdd THEN DO:
                CREATE ttbl_pc-prdd.
                ASSIGN
                    ttbl_pc-prdd.company  = machtran.company
                    ttbl_pc-prdd.m-code   = machtran.machine
                    ttbl_pc-prdd.job-no   = machtran.job_number
                    ttbl_pc-prdd.job-no2  = machtran.job_sub
                    ttbl_pc-prdd.frm      = machtran.form_number
                    ttbl_pc-prdd.blank-no = machtran.blank_number
                    ttbl_pc-prdd.pass     = machtran.pass_sequence
                    ttbl_pc-prdd.i-no     = cINo
                    ttbl_pc-prdd.i-name   = cIName
                    ttbl_pc-prdd.code     = machtran.charge_code
                    ttbl_pc-prdd.op-date  = machtran.start_date
                    ttbl_pc-prdd.start    = machtran.start_time
                    ttbl_pc-prdd.stopp    = 86400
                    ttbl_pc-prdd.startx   = SUBSTRING(STRING(ttbl_pc-prdd.start,"hh:mm"),1,2)
                                          + SUBSTRING(STRING(ttbl_pc-prdd.start,"hh:mm"),4,2)
                    ttbl_pc-prdd.stopx    = SUBSTRING(STRING(ttbl_pc-prdd.stopp,"hh:mm"),1,2)
                                          + SUBSTRING(STRING(ttbl_pc-prdd.stopp,"hh:mm"),4,2)
                    ttbl_pc-prdd.shift    = INTEGER(machtran.shift)
                    ttbl_pc-prdd.complete = machtran.complete 
                    ttbl_pc-prdd.dept     = mach.dept[1]
                    ttbl_pc-prdd.hours    = ((86400 - machtran.start_time) / 3600)
                    ttbl_pc-prdd.j-no     = IF AVAILABLE job-hdr THEN job-hdr.j-no ELSE 0
                    ttbl_pc-prdd.job      = job.job
                    ttbl_pc-prdd.speed    = IF AVAILABLE job-mch THEN job-mch.speed ELSE 0
                    iTotalTime            = iTotalTime + machtran.total_time
                    .
                CREATE ttbl_rowid.
                ASSIGN
                    ttbl_rowid.pc-prdd_rowid = ROWID(ttbl_pc-prdd)
                    ttbl_rowid.total_time    = 86400 - machtran.start_time
                    .
            END.
            IF lTSPost AND cTSPost EQ "Actual" THEN DO:
                ASSIGN 
                    ttbl_pc-prdd.crew     = 0
                    ttbl_pc-prdd.complete = lAutoPost
                    .

                FOR EACH machemp NO-LOCK
                    WHERE machemp.table_rec_key EQ machtran.rec_key
                    :
                    ASSIGN 
                        ttbl_pc-prdd.crew                               = ttbl_pc-prdd.crew + 1
                        ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee 
                        ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)]   = machemp.rate
                        .            
                END.             
                IF ttbl_pc-prdd.crew EQ 0 THEN
                    IF lDoNotShowNoCrewMessage EQ NO THEN
                    RUN pNoCrew.
            END.
            ELSE DO:
                ttbl_pc-prdd.crew = 0.
                FOR EACH machemp NO-LOCK
                    WHERE machemp.table_rec_key EQ machtran.rec_key
                    :
                    ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
                END.
                IF ttbl_pc-prdd.crew EQ 0 THEN DO:
                    IF lDoNotShowNoCrewMessage EQ NO THEN
                    RUN pNoCrew.
                END.
                ELSE  DO:
                    FIND FIRST job-code NO-LOCK
                         WHERE job-code.code EQ ttbl_pc-prdd.code
                         NO-ERROR.
                    IF AVAILABLE job-code AND job-code.cat EQ "MR" THEN
                    ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.mr-rate. 
                    ELSE IF AVAILABLE job-code AND job-code.cat EQ "RUN" THEN
                         ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.run-rate. 
                END.
            END.
            IF machtran.end_time NE 0 THEN DO:
                IF NOT CAN-FIND(FIRST ttbl_pc-prdh
                                WHERE ttbl_pc-prdh.company    EQ machtran.company
                                  AND ttbl_pc-prdh.m-code     EQ machtran.machine
                                  AND ttbl_pc-prdh.shift      EQ INTEGER(machtran.shift)
                                  AND ttbl_pc-prdh.trans-date EQ machtran.end_date) THEN DO:
                    CREATE ttbl_pc-prdh.
                    ASSIGN
                        ttbl_pc-prdh.company    = machtran.company
                        ttbl_pc-prdh.m-code     = machtran.machine
                        ttbl_pc-prdh.shift      = INTEGER(machtran.shift)
                        ttbl_pc-prdh.trans-date = machtran.end_date
                        ttbl_pc-prdh.user-id    = USERID('ASI')
                        ttbl_pc-prdh.dept       = mach.dept[1]
                        .
                END.
                RUN jc/GetItemFromJob.p (
                    machtran.company,
                    machtran.job_number,
                    machtran.job_sub,
                    machtran.form_number,
                    machtran.blank_number,
                    mach.m-code,
                    OUTPUT cINo,
                    OUTPUT cIName
                    ).
                FIND FIRST job NO-LOCK
                     WHERE job.company EQ machtran.company
                       AND job.job-no  EQ machtran.job_number
                       AND job.job-no2 EQ machtran.job_sub
                     NO-ERROR.
                FIND FIRST job-hdr NO-LOCK
                     WHERE job-hdr.company  EQ machtran.company
                       AND job-hdr.job-no   EQ machtran.job_number
                       AND job-hdr.job-no2  EQ machtran.job_sub
                       AND job-hdr.frm      EQ machtran.form_number
                       AND job-hdr.blank-no EQ machtran.blank_number
                     NO-ERROR.
                IF NOT AVAIL job-hdr THEN
                FIND FIRST job-hdr NO-LOCK
                     WHERE job-hdr.company EQ machtran.company
                       AND job-hdr.job-no  EQ machtran.job_number
                       AND job-hdr.job-no2 EQ machtran.job_sub
                       AND job-hdr.frm     EQ machtran.form_number
                     NO-ERROR.
                FIND FIRST job-mch NO-LOCK
                     WHERE job-mch.company  EQ machtran.company
                       AND job-mch.job-no   EQ machtran.job_number
                       AND job-mch.job-no2  EQ machtran.job_sub
                       AND job-mch.frm      EQ machtran.form_number
                       AND job-mch.blank-no EQ machtran.blank_number
                     NO-ERROR.
                IF NOT AVAIL job-mch THEN
                    FIND FIRST job-mch NO-LOCK
                         WHERE job-mch.company EQ machtran.company
                           AND job-mch.job-no  EQ machtran.job_number
                           AND job-mch.job-no2 EQ machtran.job_sub
                           AND job-mch.frm     EQ machtran.form_number
                         NO-ERROR.
                IF AVAILABLE job-hdr AND NOT AVAILABLE job-mch THEN
                FIND FIRST job-mch NO-LOCK
                     WHERE job-mch.company EQ machtran.company
                       AND job-mch.j-no    EQ job-hdr.j-no
                       AND job-mch.i-no    EQ job-hdr.i-no
                       AND job-mch.m-code  EQ machtran.machine
                     NO-ERROR.
                FIND FIRST ttbl_pc-prdd
                     WHERE ttbl_pc-prdd.company  EQ machtran.company
                       AND ttbl_pc-prdd.m-code   EQ machtran.machine
                       AND ttbl_pc-prdd.job-no   EQ machtran.job_number
                       AND ttbl_pc-prdd.job-no2  EQ machtran.job_sub
                       AND ttbl_pc-prdd.frm      EQ machtran.form_number
                       AND ttbl_pc-prdd.blank-no EQ machtran.blank_number
                       AND ttbl_pc-prdd.pass     EQ machtran.pass_sequence
                       AND ttbl_pc-prdd.i-no     EQ cINo
                       AND ttbl_pc-prdd.code     EQ machtran.charge_code
                       AND ttbl_pc-prdd.op-date  EQ machtran.end_date
                       AND ttbl_pc-prdd.start    EQ 0
                       AND ttbl_pc-prdd.stopp    EQ machtran.end_time /*- 86400*/
                       AND ttbl_pc-prdd.shift    EQ INTEGER(machtran.shift)
                     NO-ERROR.
                IF NOT AVAILABLE ttbl_pc-prdd THEN DO:
                    CREATE ttbl_pc-prdd.
                    ASSIGN
                        ttbl_pc-prdd.company  = machtran.company
                        ttbl_pc-prdd.m-code   = machtran.machine
                        ttbl_pc-prdd.job-no   = machtran.job_number
                        ttbl_pc-prdd.job-no2  = machtran.job_sub
                        ttbl_pc-prdd.frm      = machtran.form_number
                        ttbl_pc-prdd.blank-no = machtran.blank_number
                        ttbl_pc-prdd.pass     = machtran.pass_sequence
                        ttbl_pc-prdd.i-no     = cINo
                        ttbl_pc-prdd.i-name   = cIName
                        ttbl_pc-prdd.code     = machtran.charge_code
                        ttbl_pc-prdd.op-date  = machtran.end_date
                        ttbl_pc-prdd.start    = 0
                        ttbl_pc-prdd.stopp    = machtran.end_time /*- 86400*/
                        ttbl_pc-prdd.startx   = SUBSTRING(STRING(ttbl_pc-prdd.start,"hh:mm"),1,2)
                                              + SUBSTRING(STRING(ttbl_pc-prdd.start,"hh:mm"),4,2)
                        ttbl_pc-prdd.stopx    = SUBSTRING(STRING(ttbl_pc-prdd.stopp,"hh:mm"),1,2)
                                              + SUBSTRING(STRING(ttbl_pc-prdd.stopp,"hh:mm"),4,2)
                        ttbl_pc-prdd.shift    = INTEGER(machtran.shift)
                        ttbl_pc-prdd.complete = machtran.COMPLETE
                        ttbl_pc-prdd.dept     = mach.dept[1]
                        ttbl_pc-prdd.hours    = machtran.end_time / 3600
                        ttbl_pc-prdd.j-no     = IF AVAILABLE job-hdr THEN job-hdr.j-no ELSE 0
                        ttbl_pc-prdd.job      = job.job
                        ttbl_pc-prdd.speed    = IF AVAILABLE job-mch THEN job-mch.speed ELSE 0
                        .             
                    CREATE ttbl_rowid.
                    ASSIGN
                        ttbl_rowid.pc-prdd_rowid = ROWID(ttbl_pc-prdd)
                        ttbl_rowid.total_time    = machtran.end_time
                        .
                END.
                IF lTSPost AND cTSPost EQ "Actual" THEN DO:
                    ASSIGN 
                        ttbl_pc-prdd.crew     = 0
                        ttbl_pc-prdd.complete = lAutoPost
                        .        
                    FOR EACH machemp NO-LOCK
                        WHERE machemp.table_rec_key EQ machtran.rec_key
                        :
                        ASSIGN 
                            ttbl_pc-prdd.crew                               = ttbl_pc-prdd.crew + 1
                            ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee 
                            ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)]   = machemp.rate
                            .               
                    END.             
                    IF ttbl_pc-prdd.crew EQ 0 THEN
                        IF lDoNotShowNoCrewMessage EQ NO THEN
                        RUN pNoCrew.
                END.
                ELSE  DO:
                    ttbl_pc-prdd.crew = 0.
                    FOR EACH machemp NO-LOCK
                        WHERE machemp.table_rec_key EQ machtran.rec_key
                        :
                        ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
                    END.
                    IF ttbl_pc-prdd.crew EQ 0 THEN DO:
                        IF lDoNotShowNoCrewMessage EQ NO THEN
                        RUN pNoCrew.
                    END.
                    ELSE DO:
                        FIND FIRST job-code NO-LOCK
                             WHERE job-code.code EQ ttbl_pc-prdd.code
                             NO-ERROR.
                        IF AVAILABLE job-code AND job-code.cat EQ "MR" THEN
                        ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.mr-rate. 
                        ELSE IF AVAILABLE job-code AND job-code.cat EQ "RUN" THEN
                        ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.run-rate. 
                    END.             
                END.
            END. /*end time not equal to 0 */
        END. /* else start_date = end_date */
        FOR EACH ttbl_rowid:
            FIND FIRST ttbl_pc-prdd
                 WHERE ROWID(ttbl_pc-prdd) EQ ttbl_rowid.pc-prdd_rowid.
            ASSIGN
                dShiftPct          = IF ttbl_rowid.total_time / iTotalTime EQ ? THEN 0
                                     ELSE ttbl_rowid.total_time / iTotalTime
                dShiftPct          = IF dShiftPct EQ ? THEN 0 ELSE dShiftPct
                ttbl_pc-prdd.waste = machtran.waste_qty * dShiftPct
                ttbl_pc-prdd.qty   = machtran.run_qty * dShiftPct
                ttbl_pc-prdd.waste = IF ttbl_pc-prdd.waste EQ ? THEN 0 ELSE ttbl_pc-prdd.waste
                ttbl_pc-prdd.qty   = IF ttbl_pc-prdd.qty EQ ? THEN 0 ELSE ttbl_pc-prdd.qty
                .
            DELETE ttbl_rowid.
        END. /* each ttbl_rowid */
        iTotalTime = 0.
    END. // each mach
    IF lValidToPost THEN DO:
        RUN GL_CheckModClosePeriod (
            cCompany,
            TODAY,
            "FG",
            OUTPUT cMessage,
            OUTPUT lSuccess
            ).  
        IF NOT lSuccess THEN DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX INFORMATION.
            RETURN.
        END.      
    END. // valid to post
    RUN pPost.
    IF VALID-HANDLE(hJobProcs ) THEN
    DELETE PROCEDURE hJobProcs .

END PROCEDURE.

PROCEDURE pGLFromWork:
    DEFINE INPUT PARAMETER ipiRun   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiTRNum AS INTEGER NO-UNDO.

    DEFINE VARIABLE credits AS DECIMAL NO-UNDO.
    DEFINE VARIABLE debits  AS DECIMAL NO-UNDO. 

    FIND FIRST period NO-LOCK
        WHERE period.company EQ cCompany
          AND period.pst     LE TODAY
          AND period.pend    GE TODAY
        NO-ERROR.
    IF AVAILABLE period THEN
    FOR EACH work-gl 
        WHERE (ipiRun EQ 1 AND work-gl.job-no NE "")
           OR (ipiRun EQ 2 AND work-gl.job-no EQ "")
        BREAK BY work-gl.actnum
        :
        ASSIGN
            debits  = debits  + work-gl.debits
            credits = credits + work-gl.credits
            .
        IF LAST-OF(work-gl.actnum) THEN DO:      
            RUN GL_SpCreateGLHist(
                cCompany,
                work-gl.actnum,
                "JCOST",
                "Production Job Costing",
                TODAY,
                debits - credits,
                ipiTRNum,
                period.pnum,
                "A",
                TODAY,
                (IF work-gl.job-no NE "" THEN "Job:" + work-gl.job-no + "-" + STRING(work-gl.job-no2,"999") ELSE ""),
                "FG"
                ). 
            ASSIGN
                debits  = 0
                credits = 0
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pNoCrew:
    CREATE ttNoCrew.
    ttNoCrew.noCrew = "No Crew is Entered for Machine: "
                    + ttbl_pc-prdd.m-code
                    + " Job#: "
                    + ttbl_pc-prdd.job-no + "-"
                    + STRING(ttbl_pc-prdd.job-no2)
                    + "Charge Code: "
                    + ttbl_pc-prdd.code
                    .

END PROCEDURE.

PROCEDURE pPost:
    DEFINE VARIABLE iJobLen AS INTEGER NO-UNDO.

    DEFINE BUFFER bMach FOR mach.

    iJobLen = DYNAMIC-FUNCTION("sfCommon_GetJobLen", cCompany).
    FOR EACH tt-report:
        DELETE tt-report.
    END.
    FOR EACH bMach FIELDS(m-code) NO-LOCK
        WHERE bMach.company EQ cCompany
          AND bMach.m-code  GE cStartMachine
          AND bMach.m-code  LE cEndMachine,
        EACH machtran EXCLUSIVE-LOCK
        WHERE machtran.company EQ bMach.company
          AND machtran.machine EQ bMach.m-code
          AND machtran.posted  EQ NO
          AND FILL(" ", iJobLen - LENGTH(TRIM(machtran.job_number))) + TRIM(machtran.job_number) GE cStartJobNo
          AND FILL(" ", iJobLen - LENGTH(TRIM(machtran.job_number))) + TRIM(machtran.job_number) LE cEndJobNo
          AND machtran.job_sub GE iStartJobNo2
          AND machtran.job_sub LE iEndJobNo2
        :
        IF NOT(((machtran.end_date GE dtStartDate AND
                 machtran.end_date LE dtEndDate)  AND
                (machtran.shift    GE cStartShift AND
                 machtran.shift    LE cEndShift))  OR
            CAN-FIND(FIRST mach
                     WHERE mach.company      EQ machtran.company
                       AND mach.m-code       EQ machtran.machine
                       AND mach.industry     EQ "X"))
                        OR machtran.end_date EQ ? THEN
        NEXT.
        FOR EACH machemp EXCLUSIVE-LOCK
            WHERE machemp.table_rec_key EQ machtran.rec_key
              AND machemp.shift         GE cStartShift
              AND machemp.shift         LE cEndShift
              AND machemp.end_date      GE dtStartDate 
              AND machemp.end_date      LE dtEndDate
              AND machemp.posted        EQ NO
            USE-INDEX pi-machemp
            :
            machemp.posted = YES.         
        END.
        machtran.posted = YES.
    END.
    FOR EACH ttbl_pc-prdh:
        CREATE  pc-prdh.
        BUFFER-COPY ttbl_pc-prdh TO pc-prdh.
    END.
    FOR EACH ttbl_pc-prdd:
        CREATE  pc-prdd.
        BUFFER-COPY ttbl_pc-prdd TO pc-prdd.
        IF lPost THEN DO:
            CREATE tt-report.
            tt-report.rec-id = RECID(pc-prdd).
        END.
    END.
    IF lPost THEN
    RUN pPostWIP.

END PROCEDURE.

PROCEDURE pPostWIP:
    DEFINE VARIABLE v-loc      LIKE fg-bin.loc     NO-UNDO.
    DEFINE VARIABLE v-loc-bin  LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE VARIABLE v-up-hs    LIKE eb.num-up      NO-UNDO.
    DEFINE VARIABLE v-up       LIKE eb.num-up      NO-UNDO.
    DEFINE VARIABLE v-out      LIKE est-op.n-out   NO-UNDO.
    DEFINE VARIABLE v-on       LIKE eb.num-up      NO-UNDO.
    DEFINE VARIABLE v-est-type LIKE est.est-type   NO-UNDO.
    DEFINE VARIABLE v-trnum    LIKE gl-ctrl.trnum  NO-UNDO.
    DEFINE VARIABLE x            AS INTEGER        NO-UNDO.
    DEFINE VARIABLE cocode       AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE i            AS INTEGER        NO-UNDO.
    DEFINE VARIABLE v-tspost-val AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE v-auto-bin   AS CHARACTER      NO-UNDO.

    DEFINE BUFFER bf-eb     FOR eb.
    DEFINE BUFFER bf-itemfg FOR itemfg.

    ASSIGN
        cocode = cCompany
        v-tspost-val = cTSPost
        .
    FOR EACH tt-report NO-LOCK,
        FIRST pc-prdd WHERE RECID(pc-prdd) EQ tt-report.rec-id,
        FIRST mach NO-LOCK
        WHERE mach.company EQ cCompany
          AND mach.loc     EQ cLocation
          AND mach.m-code  EQ pc-prdd.m-code,
        FIRST job NO-LOCK
        WHERE job.company EQ cCompany
          AND job.job     EQ pc-prdd.job
          AND job.job-no  EQ pc-prdd.job-no
          AND job.job-no2 EQ pc-prdd.job-no2             
        BREAK BY pc-prdd.m-code
        TRANSACTION:

        FIND FIRST w-job
             WHERE w-job.job EQ job.job
             NO-ERROR.
        IF NOT AVAILABLE w-job THEN
        CREATE w-job.
        w-job.job = job.job.

        ASSIGN
            v-up  = 1
            v-out = 1
            v-on  = 1
            .
        FIND FIRST est NO-LOCK
             WHERE est.company EQ job.company
               AND est.est-no  EQ job.est-no
             NO-ERROR.
        v-est-type = IF AVAILABLE est THEN
        est.est-type ELSE 1.
        IF v-est-type GT 4 THEN
        v-est-type = v-est-type - 4.

        FOR EACH mach-part EXCLUSIVE-LOCK
            WHERE mach-part.company EQ mach.company
              AND mach-part.m-code  EQ mach.m-code
            :
            mach-part.total-impressions-run = mach-part.total-impressions-run
                                            + pc-prdd.qty + pc-prdd.waste
                                            .
            FIND FIRST reftable EXCLUSIVE-LOCK
                 WHERE reftable.reftable EQ "MACHPARTHOURS"
                   AND reftable.company  EQ mach-part.company
                   AND reftable.loc      EQ mach-part.m-code
                   AND reftable.code     EQ mach-part.rm-part-code
                 NO-ERROR.
            IF NOT AVAILABLE reftable THEN DO:
                CREATE reftable.
                ASSIGN
                    reftable.reftable = "MACHPARTHOURS"
                    reftable.company  = mach-part.company
                    reftable.loc      = mach-part.m-code
                    reftable.code     = mach-part.rm-part-code
                    . 
            END.
            reftable.val[1] = reftable.val[1] + pc-prdd.hours.
            RELEASE reftable.
        END.

        IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
           mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
        RUN pUpdatePlateDie (ROWID(pc-prdd), "P", v-est-type).

        IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
           mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
        RUN pUpdatePlateDie (ROWID(pc-prdd), "D", v-est-type).

        IF INDEX("AP",mach.p-type) GT 0 THEN
        ASSIGN
            v-on  = 1
            v-up  = 1
            v-out = 1
            .
        ELSE
        IF AVAILABLE est THEN DO:
            RUN sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, OUTPUT v-up).
            FIND FIRST ef NO-LOCK
                WHERE ef.company EQ est.company
                  AND ef.est-no  EQ est.est-no
                  AND ef.form-no EQ pc-prdd.frm
                NO-ERROR.
            IF AVAILABLE ef THEN DO:
                RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
                v-on = v-up * v-on.
            END.
            FIND FIRST est-op NO-LOCK
                 WHERE est-op.company EQ est.company
                   AND est-op.est-no  EQ est.est-no
                   AND est-op.s-num   EQ pc-prdd.frm
                   AND (est-op.b-num  EQ pc-prdd.blank-no
                    OR pc-prdd.blank-no EQ 0)
                   AND est-op.m-code  EQ pc-prdd.m-code
                   AND est-op.op-pass EQ pc-prdd.pass
                   AND est-op.dept    EQ pc-prdd.dept
                   AND est-op.line    LT 500
                 NO-ERROR.
            IF ((AVAILABLE est-op) AND est-op.op-sb) OR
               ((NOT AVAILABLE est-op) AND mach.p-type NE "B") THEN DO:
                IF AVAILABLE est-op THEN
                RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).
                ELSE v-out = 1.
                v-up = v-up * v-out.
            END.
            ELSE v-up = 1.
            v-on = v-on / v-up.
        END.
        v-up-hs = 1.

        IF pc-prdd.dept EQ "HS" AND AVAILABLE est AND
           mach.therm  AND mach.p-type EQ "S" THEN
        RUN sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, OUTPUT v-up-hs).

        {pc/pcmchact.i}

        IF pc-prdd.complete THEN DO:
            RUN pc/pcprdd4u.p (ROWID(pc-prdd)).
            FOR EACH tt-job-hdr,
                FIRST itemfg NO-LOCK
                WHERE itemfg.company    EQ cCompany
                  AND itemfg.i-no       EQ tt-job-hdr.i-no
                  AND itemfg.case-count GT 0
                :
                x = 1.
                FOR EACH fg-rctd NO-LOCK BY fg-rctd.r-no DESCENDING:
                    LEAVE.
                END.
                IF AVAILABLE fg-rctd THEN
                x = fg-rctd.r-no.

                FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.
                IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT x THEN
                x = fg-rcpth.r-no.

                CREATE fg-rctd.
                ASSIGN
                    fg-rctd.r-no       = X + 1
                    fg-rctd.rct-date   = pc-prdd.op-date
                    fg-rctd.trans-time = pc-prdd.op-time
                    fg-rctd.company    = cCompany
                    fg-rctd.rita-code  = "R"
                    fg-rctd.i-name     = itemfg.i-name
                    fg-rctd.i-no       = tt-job-hdr.i-no
                    fg-rctd.job-no     = pc-prdd.job-no
                    fg-rctd.job-no2    = pc-prdd.job-no2
                    .
                ASSIGN
                    v-up  = 1
                    v-out = 1
                    .
                IF AVAILABLE est AND INDEX("APB",mach.p-type) LE 0 THEN DO:
                    RUN sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, OUTPUT v-up).
                    FIND FIRST est-op NO-LOCK
                        WHERE est-op.company EQ est.company
                          AND est-op.est-no  EQ est.est-no
                          AND est-op.s-num   EQ pc-prdd.frm
                          AND (est-op.b-num  EQ pc-prdd.blank-no
                           OR pc-prdd.blank-no EQ 0)
                          AND est-op.m-code  EQ pc-prdd.m-code
                          AND est-op.op-pass EQ pc-prdd.pass
                          AND est-op.dept    EQ pc-prdd.dept
                          AND est-op.line    LT 500
                        NO-ERROR.
                    IF AVAILABLE est-op AND est-op.n-out NE 0 THEN
                    v-out = est-op.n-out.
                END.

                ASSIGN
                    fg-rctd.b-num      = pc-prdd.blank-no
                    fg-rctd.s-num      = pc-prdd.frm
                    fg-rctd.t-qty      = pc-prdd.qty / v-up-hs * v-out * v-up
                    fg-rctd.pur-uom    = itemfg.prod-uom
                    fg-rctd.cost-uom   = itemfg.prod-uom
                    fg-rctd.std-cost   = tt-job-hdr.std-tot-cost
                    fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
                    fg-rctd.qty-case   = itemfg.case-count
                    fg-rctd.partial    = fg-rctd.t-qty MODULO itemfg.case-count
                    fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
                    fg-rctd.cases-unit = 1
                    .
                IF fg-rctd.t-qty LE 0 THEN
                fg-rctd.cases = 0.
                RELEASE fg-bin.
                FIND FIRST reftable NO-LOCK
                     WHERE reftable.reftable EQ "pc/pcprddu3.p"
                       AND reftable.company  EQ pc-prdd.company
                       AND reftable.code     EQ STRING(RECID(pc-prdd))
                     NO-ERROR.
                IF AVAILABLE reftable THEN DO:
                    ASSIGN
                        fg-rctd.cases      = reftable.val[1]
                        fg-rctd.qty-case   = reftable.val[2]
                        fg-rctd.cases-unit = reftable.val[3]
                        fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case)
                        .
                    FIND FIRST fg-bin NO-LOCK 
                        WHERE fg-bin.rec_key EQ reftable.code2 
                        NO-ERROR.
                END.
                IF AVAILABLE fg-bin THEN
                ASSIGN
                    v-loc       = fg-bin.loc
                    v-loc-bin   = fg-bin.loc-bin
                    fg-rctd.tag = fg-bin.tag
                    .
                ELSE
                IF v-auto-bin EQ "ShipTo" THEN DO:
                    /*get estimate blank file from finished goods item file*/
                    FIND FIRST eb
                        WHERE eb.company  EQ cCompany
                          AND eb.est-no   EQ itemfg.est-no
                          AND eb.stock-no EQ itemfg.i-no
                        USE-INDEX est-no NO-LOCK NO-ERROR.
                    IF AVAILABLE eb THEN DO:
                        /*get customer file from estimate blank file*/
                        FIND FIRST cust NO-LOCK
                            WHERE cust.company EQ cCompany
                              AND cust.cust-no EQ eb.cust-no
                            NO-ERROR.
                        IF AVAILABLE cust THEN DO:              
                            FIND FIRST shipto NO-LOCK
                                WHERE shipto.company = cCompany
                                  AND shipto.cust-no = cust.cust-no 
                                NO-ERROR.
                            IF AVAILABLE shipto THEN DO:
                                FIND FIRST fg-bin NO-LOCK
                                     WHERE fg-bin.company EQ cCompany
                                       AND fg-bin.loc     EQ shipto.loc
                                       AND fg-bin.loc-bin EQ shipto.loc-bin
                                       AND fg-bin.i-no    EQ ""
                                     NO-ERROR.
                                IF AVAILABLE fg-bin THEN
                                ASSIGN
                                    v-loc     = shipto.loc
                                    v-loc-bin = shipto.loc-bin
                                    .
                            END.
                            IF v-loc EQ "" AND v-loc-bin EQ "" THEN DO:
                                FIND FIRST fg-bin NO-LOCK
                                    WHERE fg-bin.company EQ cCompany
                                      AND fg-bin.loc     EQ itemfg.def-loc
                                      AND fg-bin.loc-bin EQ itemfg.def-loc-bin
                                      AND fg-bin.i-no    EQ ""
                                    NO-ERROR.
                                IF AVAILABLE fg-bin THEN 
                                ASSIGN 
                                    v-loc     = itemfg.def-loc
                                    v-loc-bin = itemfg.def-loc-bin
                                    .
                            END. /*if avail shipto*/
                        END. /*if avail cust*/
                    END. /*if avail eb*/
                END. /*if system default is shipto*/
                /*else if "FGFILE" then get from finished goods file*/
                ELSE  DO:
                    FIND FIRST fg-bin NO-LOCK
                         WHERE fg-bin.company EQ cCompany
                           AND fg-bin.loc     EQ itemfg.def-loc
                           AND fg-bin.loc-bin EQ itemfg.def-loc-bin
                           AND fg-bin.i-no    EQ ""
                         NO-ERROR.
                    IF AVAILABLE fg-bin THEN 
                    ASSIGN
                        v-loc     = itemfg.def-loc
                        v-loc-bin = itemfg.def-loc-bin
                        .
                END. /*else FGFILE*/

            /*if bin and warehouse are blank, goto cust "X" shipto file*/
            IF v-loc EQ "" AND v-loc-bin EQ "" THEN DO:
                FIND FIRST cust NO-LOCK
                    WHERE cust.company EQ cCompany
                      AND cust.active  EQ "X"
                    NO-ERROR.
                IF AVAILABLE cust THEN DO:
                    FIND FIRST shipto NO-LOCK
                         WHERE shipto.company EQ cCompany
                           AND shipto.cust-no EQ cust.cust-no  
                         NO-ERROR.
                    IF AVAILABLE shipto THEN DO:
                        FIND FIRST fg-bin NO-LOCK
                             WHERE fg-bin.company EQ cCompany
                               AND fg-bin.loc     EQ shipto.loc
                               AND fg-bin.loc-bin EQ shipto.loc-bin
                               AND fg-bin.i-no    EQ ""
                             NO-ERROR.
                        ASSIGN
                            v-loc     = shipto.loc
                            v-loc-bin = shipto.loc-bin
                            .
                    END.                                  
                END.
            END.

            ASSIGN
                fg-rctd.loc     = v-loc
                fg-rctd.loc-bin = v-loc-bin
                .
            FIND FIRST fg-bin NO-LOCK
                 WHERE fg-bin.company EQ fg-rctd.company
                   AND fg-bin.i-no    EQ fg-rctd.i-no
                   AND fg-bin.job-no  EQ pc-prdd.job-no
                   AND fg-bin.job-no2 EQ pc-prdd.job-no2
                   AND fg-bin.loc     EQ fg-rctd.loc
                   AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                   AND fg-bin.tag     EQ fg-rctd.tag
                 NO-ERROR.
                IF AVAILABLE fg-bin THEN
                fg-rctd.cases-unit = fg-bin.cases-unit.
                RUN fg/comprcpt.p (ROWID(fg-rctd)).                       
            END. /* for each tt-job-hdr*/
        END. /* for each pc-prdd.completed*/
        DELETE pc-prdd.
    END. /* for each tt-report, pc-prdd */

    IF lDCPostGL THEN 
    DO TRANSACTION:
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                 WHERE gl-ctrl.company EQ cCompany
                 NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN DO:
                ASSIGN 
                    v-trnum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = v-trnum
                    .
                FIND CURRENT gl-ctrl NO-LOCK.
                RUN pGLFromWork (1, v-trnum).
                RUN pGLFromWork (2, v-trnum).
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    END.

    FOR EACH w-job,
        FIRST job NO-LOCK
        WHERE job.company EQ cCompany
          AND job.job     EQ w-job.job
        :
        RUN jc/job-cls2.p (RECID(job)).
    END.

    RUN job_CloseJob_DCPost IN hJobProcs (cCompany, INPUT TABLE w-job).

END PROCEDURE.

PROCEDURE pUpdatePlateDie:
    DEFINE INPUT PARAMETER iprRowID      AS ROWID        NO-UNDO.
    DEFINE INPUT PARAMETER ipcUpdateType AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstType  LIKE est.est-type NO-UNDO.

    DEFINE BUFFER b-pc-prdd FOR pc-prdd.
    DEFINE BUFFER bf-job    FOR job.

    FIND FIRST b-pc-prdd NO-LOCK
         WHERE ROWID(b-pc-prdd) EQ iprRowID
         NO-ERROR.
    IF AVAILABLE b-pc-prdd THEN
    FOR FIRST bf-job NO-LOCK
        WHERE bf-job.company EQ b-pc-prdd.company
          AND bf-job.job     EQ b-pc-prdd.job
          AND bf-job.job-no  EQ b-pc-prdd.job-no
          AND bf-job.job-no2 EQ b-pc-prdd.job-no2,
        FIRST job-hdr NO-LOCK
        WHERE job-hdr.company   EQ bf-job.company
          AND job-hdr.job       EQ bf-job.job
          AND job-hdr.job-no    EQ bf-job.job-no
          AND job-hdr.job-no2   EQ bf-job.job-no2
          AND (job-hdr.frm      EQ b-pc-prdd.frm
           OR  ipiEstType      EQ 2)
        :
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cCompany
              AND itemfg.i-no    EQ job-hdr.i-no
            NO-ERROR.
        IF ipiEstType EQ 2 AND job.est-no NE "" AND
           AVAILABLE itemfg AND itemfg.isaset   THEN
            FOR EACH eb NO-LOCK
                WHERE eb.company EQ cCompany
                  AND eb.est-no  EQ bf-job.est-no
                  AND eb.form-no EQ b-pc-prdd.frm,
                FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ cCompany
                  AND itemfg.i-no    EQ eb.stock-no
                :
                LEAVE.
            END.
        IF AVAILABLE itemfg THEN DO:
            IF ipcUpdateType EQ "P" AND itemfg.plate-no NE "" THEN
                FIND FIRST prep EXCLUSIVE-LOCK
                    WHERE prep.company EQ cCompany
                      AND prep.code    EQ itemfg.plate-no
                    NO-ERROR.
            ELSE
                IF ipcUpdateType EQ "D" AND itemfg.die-no NE "" THEN
                FIND FIRST prep EXCLUSIVE-LOCK
                     WHERE prep.company EQ cCompany
                       AND prep.code    EQ itemfg.die-no
                     NO-ERROR.
            IF AVAILABLE prep THEN DO:
                ASSIGN 
                    prep.no-of-impressions = prep.no-of-impressions
                                           + b-pc-prdd.qty + b-pc-prdd.waste
                    prep.last-date         = b-pc-prdd.op-date
                    prep.last-job-no       = b-pc-prdd.job-no
                    prep.last-job-no2      = b-pc-prdd.job-no2
                    .
                RELEASE prep.
            END.
        END.
    END.

END PROCEDURE.
