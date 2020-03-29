
&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
"Structured Procedure File Template.

Use this template to create a new Structured Procedure file to compile and run PROGRESS 4GL code. You edit structured procedure files using the AB's Section Editor."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : purgeProcs.p
    Purpose     : Structured Procedure for purging records; created as super procedure
    Syntax      : RUN Purge (<quoted-table-name>,<record-rowid>, output lSuccess, output cMessage)
                  RUN prePurge (<quoted-table-name>,<record-rowid>, output lSuccess, output cMessage)
    Description : Purge procedure creates lists and deletes, prePurge ONLY creates lists
    Author(s)   : MYT
    Created     : Thu Sep 26 07:02:59 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF STREAM datafiles.
DEF STREAM listfile.
DEF VAR cOutDir AS CHAR NO-UNDO.
DEF VAR cListFile AS CHAR NO-UNDO.
DEF VAR lPurge AS LOG NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR locode AS CHAR NO-UNDO.
DEF VAR lVerbose AS LOG NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&IF DEFINED(EXCLUDE-fEndPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEndPurge Procedure
FUNCTION fEndPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fGetPurgeDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetPurgeDir Procedure
FUNCTION fGetPurgeDir RETURNS CHARACTER 
  ( INPUT cTable AS CHAR  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fStartPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStartPurge Procedure
FUNCTION fStartPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pfWriteLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pfWriteLine Procedure
FUNCTION pfWriteLine RETURNS LOGICAL PRIVATE
  ( INPUT cTable AS CHAR, INPUT cNames AS CHAR, INPUT cData AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF




/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pPurgeJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPurgeJob Procedure
PROCEDURE pPurgeJob PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job

    DEF INPUT PARAMETER iprRowid AS ROWID.
    DEF OUTPUT PARAMETER oplSuccess AS LOG INITIAL TRUE.
    DEF OUTPUT PARAMETER opcMessage AS CHAR INITIAL "OK".
    DEFINE BUFFER bjob FOR job.
    
    DISABLE TRIGGERS FOR LOAD OF job-farm.        
    DISABLE TRIGGERS FOR LOAD OF job-farm-rctd.        
    DISABLE TRIGGERS FOR LOAD OF job-hdr.        
    DISABLE TRIGGERS FOR LOAD OF job-mat.        
    DISABLE TRIGGERS FOR LOAD OF job-mch.        
    DISABLE TRIGGERS FOR LOAD OF job-prep.        
    DISABLE TRIGGERS FOR LOAD OF mat-act.        
    DISABLE TRIGGERS FOR LOAD OF mch-act.        
    DISABLE TRIGGERS FOR LOAD OF misc-act.        
/*    DISABLE TRIGGERS FOR LOAD OF job.*/
    DISABLE TRIGGERS FOR LOAD OF reftable.

    DO TRANSACTION:
        FIND FIRST bjob NO-LOCK WHERE
            ROWID(bjob) EQ iprRowid
            NO-ERROR.
        IF NOT AVAIL bjob THEN 
        DO:
            ASSIGN 
                oplSuccess = FALSE 
                opcMessage = "Job not found by rowid".
            RETURN.
        END.
        pfWriteLine("JOB","Head",STRING(bjob.company) + "-" + 
                                 STRING(bjob.job-no) + "-" +
                                 STRING(bjob.job-no2)). 
        IF lPurge THEN 
            RUN jc/jc-dall.p (RECID(bjob)).

&SCOPED-DEFINE cFileName job-hdr        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job     EQ bjob.job AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
            {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.
 
&SCOPED-DEFINE cFileName job-mat        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job     EQ bjob.job AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.


&SCOPED-DEFINE cFileName job-mch        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job     EQ bjob.job AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.

&SCOPED-DEFINE cFileName job-prep        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job     EQ bjob.job AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.

&SCOPED-DEFINE cFileName job-farm        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.

&SCOPED-DEFINE cFileName job-farm-rctd        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.

&SCOPED-DEFINE cFileName mat-act        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job     EQ bjob.job AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.

&SCOPED-DEFINE cFileName mch-act        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job     EQ bjob.job AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.

&SCOPED-DEFINE cFileName misc-act        
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        FOR EACH {&cFileName} EXCLUSIVE WHERE 
            {&cFileName}.company EQ bjob.company AND 
            {&cFileName}.job     EQ bjob.job AND 
            {&cFileName}.job-no  EQ bjob.job-no AND 
            {&cFileName}.job-no2 EQ bjob.job-no2:
            IF lPurge THEN 
            DO:
                {util/dljobkey.i}
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")). 
                EXPORT STREAM datafiles {&cFileName}.
                DELETE {&cFileName}.
            END.  
            ELSE        
                pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING({&cFileName}.company) + "|" + 
                    STRING({&cFileName}.job) + "|" +
                    STRING({&cFileName}.job-no) + "|" +
                    STRING({&cFileName}.job-no2,"99")).
        END.
        OUTPUT STREAM datafiles CLOSE.

        IF bjob.exported THEN 
        DO:
            bjob.stat = "X".
            RUN jc/kiwiexp2.p (RECID(bjob)).
        END.

&scoped-define cFileName job
        OUTPUT STREAM datafiles TO VALUE (cOutDir + "\{&cFileName}.d") APPEND.
        IF lPurge THEN 
        DO:
            FIND CURRENT bjob EXCLUSIVE.
            pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING(b{&cFileName}.company) + "|" + 
                STRING(b{&cFileName}.job) + "|" +
                STRING(b{&cFileName}.job-no) + "|" +
                STRING(b{&cFileName}.job-no2)).
            EXPORT STREAM datafiles b{&cFileName}.
            DELETE bjob.
        END.
        ELSE        
            pfWriteLine("{&cFileName}","company|job|job-no|job-no2",STRING(b{&cFileName}.company) + "|" + 
                STRING(b{&cFileName}.job) + "|" +
                STRING(b{&cFileName}.job-no) + "|" +
                STRING(b{&cFileName}.job-no2)).
        OUTPUT STREAM datafiles CLOSE.
        
    END. /* Transaction */ 
    
    PROCESS EVENTS.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-PrePurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrePurge Procedure
PROCEDURE PrePurge:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
    DEF INPUT PARAMETER iplVerbose AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    ASSIGN 
        lPurge = FALSE
        lVerbose = iplVerbose.
    
    CASE ipcTable:
        WHEN "job" THEN RUN pPurgeJob (iprRowid, OUTPUT oplSuccess, OUTPUT opcMessage).
    END CASE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Purge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Purge Procedure
PROCEDURE Purge:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
    DEF INPUT PARAMETER iplVerbose AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    ASSIGN 
        lPurge = TRUE
        lVerbose = iplVerbose.
    
    CASE ipcTable:
        WHEN "job" THEN RUN pPurgeJob (iprRowid, OUTPUT oplSuccess, OUTPUT opcMessage).
    END CASE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF



/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fEndPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEndPurge Procedure
FUNCTION fEndPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE result AS LOGICAL NO-UNDO.
    
    pfWriteLIne(cTable, "End", "").
    OUTPUT STREAM listfile CLOSE.

    RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fGetPurgeDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetPurgeDir Procedure
FUNCTION fGetPurgeDir RETURNS CHARACTER 
  ( INPUT cTable AS CHAR  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE crOutDir AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cOutDir = "C:\tmp\" + cTable + "purge" + STRING(YEAR(TODAY),"9999") + 
                                          STRING(MONTH(TODAY),"99") + 
                                          STRING(DAY(TODAY),"99") + 
                                          "-" + STRING(TIME)
        cListFile = cOutDir + "\purgelist.txt"
        crOutDir = cOutDir.

    RETURN crOutDir.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fStartPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fStartPurge Procedure
FUNCTION fStartPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE result AS LOGICAL NO-UNDO.
    
    OS-CREATE-DIR VALUE(cOutDir).
    OUTPUT STREAM listfile TO VALUE(cListFile).

    pfWriteLine("job", "Start", "").

    RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pfWriteLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pfWriteLine Procedure
FUNCTION pfWriteLine RETURNS LOGICAL PRIVATE
    ( INPUT cTable AS CHAR, INPUT cNames AS CHAR, INPUT cData AS CHAR  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cOutline AS CHAR NO-UNDO.
    DEF VAR cTime AS CHAR NO-UNDO.
    DEF VAR cAction AS CHAR NO-UNDO.
        
    ASSIGN 
        cTime = STRING(TIME,"HH:MM:SS") + "  "
        cAction = IF lPurge THEN "Deleting " ELSE "Simulate deleting ".
           
    IF cNames EQ "Start" THEN 
    DO:
        ASSIGN 
            cOutLine = cTime + "Beginning " + cTable + " purge.".
        PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
    END.
    ELSE IF cNames EQ "End" THEN 
    DO:
        ASSIGN 
            cOutLine = cTime + "Ending " + cTable + " purge.".
        PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
    END.
    ELSE IF cNames EQ "Head" THEN DO:
        ASSIGN 
            cOutLine = cTime + cAction + "records for " + cTable + ": " + cData.
        PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
    END.
    ELSE 
    DO:
        IF lVerbose THEN DO:
            ASSIGN 
                cOutline = cTime + "   " + cAction + cTable + " WHERE ".
            DO iCtr = 1 TO NUM-ENTRIES(cNames,"|"):
                ASSIGN 
                    cOutline = cOutline + ENTRY(iCtr,cNames,"|") + "=" + ENTRY(iCtr,cData,"|") + " AND ".
            END.
            ASSIGN 
                cOutline = SUBSTRING(cOutline, 1, LENGTH(cOutline) - 5) + ".".
            PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
        END. 
    END.
         
    RETURN TRUE.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

