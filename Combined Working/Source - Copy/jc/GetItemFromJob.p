&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : jc/GetItemFromjob.p
    Purpose     : Retrieves item # and name based on job inputs

    Syntax      : jc/GetItemFromJob.p (input company,
                                       input job-no,
                                       input job-no2,
                                       input form,
                                       input blank,
                                       output i-no,
                                       output i-name).

    Description :

    Author(s)   : BV
    Created     : 1/23/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipxJobNo LIKE job.job-no NO-UNDO.
DEFINE INPUT PARAMETER ipxJobNo2 LIKE job.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER ipxForm LIKE job-hdr.frm NO-UNDO.
DEFINE INPUT PARAMETER ipxBlank LIKE job-hdr.blank-no NO-UNDO.
DEFINE INPUT PARAMETER ipxMCode LIKE mach.m-code NO-UNDO.

DEFINE OUTPUT PARAMETER opcINo AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opcIName AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
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
DEFINE BUFFER bf-job FOR job.
DEFINE BUFFER bf-job-hdr FOR job-hdr.
DEFINE BUFFER bf-reftable FOR reftable.
DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-est FOR est.
DEFINE BUFFER bf-eb FOR eb.
DEFINE BUFFER bf-eb2 FOR eb.
DEFINE BUFFER bf-mach FOR mach.
DEFINE BUFFER bf-job-mch FOR job-mch.

DEFINE VARIABLE xEstType LIKE est.est-type NO-UNDO.


/*Get Job*/
FIND FIRST bf-job 
    WHERE bf-job.company EQ ipcCompany
      AND bf-job.job-no EQ ipxJobNo
      AND bf-job.job-no2 EQ ipxJobNo2
    NO-LOCK NO-ERROR.

/*Get Estimate*/
IF AVAIL bf-job THEN 
    FIND FIRST bf-est 
        WHERE bf-est.company EQ ipcCompany
          AND bf-est.est-no EQ bf-job.est-no
    NO-LOCK NO-ERROR.
IF AVAIL bf-est THEN DO:
    FIND FIRST bf-eb OF bf-est
        WHERE bf-eb.form-no EQ ipxForm
          AND bf-eb.blank-no EQ ipxBlank
    NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-eb 
        AND NOT CAN-FIND(
            FIRST bf-eb2 OF bf-est 
                WHERE bf-eb2.form-no EQ ipxForm 
                  AND bf-eb2.blank-no GE 2) THEN
        FIND FIRST bf-eb OF bf-est
            WHERE bf-eb.form-no EQ ipxForm
        NO-LOCK NO-ERROR.
    xEstType = bf-est.est-type.
END.

IF AVAIL bf-job THEN 
    /*Get Job-hdr */
    FIND FIRST bf-job-hdr
        WHERE bf-job-hdr.company EQ ipcCompany
          AND bf-job-hdr.job     EQ bf-job.job
          AND bf-job-hdr.job-no  EQ bf-job.job-no
          AND bf-job-hdr.job-no2 EQ bf-job.job-no2
          AND (bf-job-hdr.frm    EQ ipxForm OR
               xEstType EQ 2 OR xEstType EQ 6)
          AND (bf-job-hdr.blank-no EQ ipxBlank OR
               xEstType EQ 2 OR xEstType EQ 6)
        NO-LOCK NO-ERROR.
    
    /*Find matching reftable*/
IF AVAIL bf-job-hdr THEN DO:
    RELEASE bf-reftable.
    IF ipxMCode NE "" THEN
        FIND FIRST bf-mach
            WHERE bf-mach.company EQ ipcCompany
              AND bf-mach.m-code  EQ ipxMCode
            NO-LOCK NO-ERROR.
    IF (xEstType EQ 2 OR xEstType EQ 6)
        AND (NOT AVAIL bf-mach OR INDEX("AP",bf-mach.p-type) LE 0) THEN
        FIND FIRST bf-reftable
            WHERE bf-reftable.reftable EQ "jc/jc-calc.p"
              AND bf-reftable.company  EQ ipcCompany
              AND bf-reftable.loc      EQ ""
              AND bf-reftable.code     EQ STRING(bf-job-hdr.job,"999999999")
              AND bf-reftable.val[12]  EQ ipxForm
              AND (bf-reftable.val[13] EQ ipxBlank OR
                ipxBlank EQ 0)
            NO-LOCK NO-ERROR.
END.


opcINo = IF AVAILABLE bf-eb THEN bf-eb.stock-no ELSE "".
IF opcINo EQ "" THEN
    opcINo = IF AVAILABLE bf-reftable THEN bf-reftable.code2 ELSE "".
IF opcINo EQ "" THEN
    opcINo = IF AVAILABLE bf-job-hdr THEN bf-job-hdr.i-no ELSE "".

/* Get Name*/
IF opcINo NE "" THEN DO:
    FIND FIRST bf-itemfg 
        WHERE bf-itemfg.company EQ ipcCompany
          AND bf-itemfg.i-no    EQ opcINo
        NO-LOCK NO-ERROR.
    opcIName = IF AVAILABLE bf-itemfg THEN bf-itemfg.i-name ELSE "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


