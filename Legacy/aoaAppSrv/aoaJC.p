&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaJC.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ProdAnalysis.rpa */
DEFINE TEMP-TABLE tt-srt NO-UNDO LIKE mch-srt RCODE-INFORMATION 
    FIELD act-m-code    LIKE mach.m-code
    FIELD tot-run-hours   AS DECIMAL LABEL "Total Run Hrs"
    FIELD tot-mr-hours    AS DECIMAL LABEL "Total MR Hrs"
    FIELD qty-Ton         AS DECIMAL LABEL "Ton Qty"       FORMAT ">>,>>9.99"
    FIELD qty-msf         AS DECIMAL LABEL "MSF Qty"       FORMAT ">>,>>9.99"
    FIELD start-time      AS INTEGER LABEL "Start Time"
    FIELD start-date      AS DATE    LABEL "Start Date"    FORMAT "99/99/9999"
    FIELD i-no          LIKE mch-srt.job-no
        INDEX dept-idx  dept m-code job-no job-no2 frm blank-no
        INDEX job-idx   job-no job-no2
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fProdAnalysis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fProdAnalysis Procedure 
FUNCTION fProdAnalysis RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fProdAnalysis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fProdAnalysis Procedure 
FUNCTION fProdAnalysis RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  ProdAnalysis.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-srt.

    RETURN TEMP-TABLE tt-srt:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

