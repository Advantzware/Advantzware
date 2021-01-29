&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharshooter/smartobj/jobFilter.w

  Description: StandardJob Filter option

  Author: Mithun Porandla
  Created: 12/17/2020

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

USING jc.JobHeader.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJob      AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdJobDetails            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobDetailsWin         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs              AS HANDLE    NO-UNDO.
DEFINE VARIABLE cFormattedJobno         AS CHARACTER NO-UNDO.
DEFINE VARIABLE oJobHeader              AS JobHeader NO-UNDO.
DEFINE VARIABLE lScanNextJob            AS LOGICAL   NO-UNDO.

oJobHeader = NEW JobHeader().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 imJobLookup cbJobNo2 fiJobNo cbFormNo ~
cbBlankNo 
&Scoped-Define DISPLAYED-OBJECTS cbJobNo2 fiJobNoLabel fiJobNo cbFormNo ~
cbBlankNo fiFormNoLabel fiBlankNoLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btJobDetails 
     IMAGE-UP FILE "Graphics/32x32/form.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/form_disabled.ico":U
     LABEL "" 
     SIZE 11 BY 2.62 TOOLTIP "View Current Job Details".

DEFINE VARIABLE cbBlankNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1.48
     FONT 36 NO-UNDO.

DEFINE VARIABLE cbFormNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     FONT 36 NO-UNDO.

DEFINE VARIABLE cbJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1.48
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiBlankNoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Blank #:" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1.29 NO-UNDO.

DEFINE VARIABLE fiFormNoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Form #:" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.29 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.2 BY 1.29 NO-UNDO.

DEFINE VARIABLE fiJobNoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Job #:" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.29 NO-UNDO.

DEFINE IMAGE imJobLookup
     FILENAME "Graphics/32x32/magnifying_glass.ico":U
     STRETCH-TO-FIT RETAIN-SHAPE
     SIZE 5.4 BY 1.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 64 BY 3.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cbJobNo2 AT ROW 1.24 COL 44.2 COLON-ALIGNED NO-LABEL WIDGET-ID 162
     fiJobNoLabel AT ROW 1.29 COL 4.8 NO-LABEL WIDGET-ID 2
     fiJobNo AT ROW 1.29 COL 12.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     btJobDetails AT ROW 1.33 COL 66.4 WIDGET-ID 160
     cbFormNo AT ROW 2.71 COL 12.8 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     cbBlankNo AT ROW 2.71 COL 44.2 COLON-ALIGNED NO-LABEL WIDGET-ID 166
     fiFormNoLabel AT ROW 2.76 COL 2.6 NO-LABEL WIDGET-ID 16
     fiBlankNoLabel AT ROW 2.76 COL 33.6 NO-LABEL WIDGET-ID 20
     RECT-3 AT ROW 1.05 COL 1 WIDGET-ID 24
     imJobLookup AT ROW 1.14 COL 56.2 WIDGET-ID 182
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 17 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 3.33
         WIDTH              = 79.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btJobDetails IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiBlankNoLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiFormNoLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiJobNoLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btJobDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJobDetails s-object
ON CHOOSE OF btJobDetails IN FRAME F-Main
DO:
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF NOT VALID-HANDLE(hdJobDetails) THEN DO:         
        RUN inventory/job-details.w PERSISTENT SET hdJobDetails.

        RUN dispatch IN hdJobDetails (
            INPUT 'initialize':U
            ) NO-ERROR.
        
        hdJobDetailsWin = hdJobDetails:CURRENT-WINDOW.
    END.

    IF VALID-HANDLE(hdJobDetails) AND
        VALID-HANDLE(hdJobDetailsWin) THEN DO:        
        RUN pInit IN hdJobDetails (
            INPUT cCompany,
            INPUT cLocation,
            INPUT fiJobno:SCREEN-VALUE,
            INPUT INTEGER(cbJobno2:SCREEN-VALUE),
            INPUT INTEGER(cbFormno:SCREEN-VALUE),
            INPUT INTEGER(cbBlankno:SCREEN-VALUE)
            ) NO-ERROR.            

        IF hdJobDetailsWin:WINDOW-STATE EQ 2 THEN ASSIGN 
            hdJobDetailsWin:WINDOW-STATE = 3.
        
        hdJobDetailsWin:MOVE-TO-TOP().
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbBlankNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbBlankNo s-object
ON VALUE-CHANGED OF cbBlankNo IN FRAME F-Main
DO:
    RUN pValidateJob.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFormNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFormNo s-object
ON VALUE-CHANGED OF cbFormNo IN FRAME F-Main
DO:
    RUN pValidateJob.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbJobNo2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbJobNo2 s-object
ON VALUE-CHANGED OF cbJobNo2 IN FRAME F-Main
DO:
    DEFINE VARIABLE iFormNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo   AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE cFormNoListItems  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNoListItems AS CHARACTER NO-UNDO.
    
    RUN pGetFormNo (
        INPUT  cCompany,
        INPUT  fiJobNo:SCREEN-VALUE,
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        OUTPUT iFormNo,
        OUTPUT cFormNoListItems 
        ).

    RUN pGetBlankNo (
        INPUT  cCompany,
        INPUT  fiJobNo:SCREEN-VALUE,
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        OUTPUT iBlankNo,
        OUTPUT cBlankNoListItems 
        ).

    RUN pUpdateJobDetails(
        INPUT "FormNo",
        INPUT STRING(iFormNo),
        INPUT cFormNoListItems
        ).         

    RUN pUpdateJobDetails(
        INPUT "BlankNo",
        INPUT STRING(iBlankNo),
        INPUT cBlankNoListItems
        ).         
          
    RUN pValidateJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo s-object
ON ENTRY OF fiJobNo IN FRAME F-Main
DO:
    cJob = SELF:SCREEN-VALUE.  
    
    fiJobNo:SET-SELECTION(1, LENGTH(fiJobNo:SCREEN-VALUE) + 1) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo s-object
ON LEAVE OF fiJobNo IN FRAME F-Main
DO:
    DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iJobNo2    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo   AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE cJobNo2ListItems  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNoListItems  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNoListItems AS CHARACTER NO-UNDO.
    
    lScanNextJob = FALSE.
    
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.

    IF SELF:SCREEN-VALUE EQ cJob THEN DO:
        MESSAGE "The job '" + cJob + "' is already scanned." SKIP
            "Do you want scan the same job again?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.   
        
        IF NOT lChoice THEN
            RETURN. 
        
        /* If the job with options (job-no2, form and blank) is scanned, all the fields are updated with exact inputs including job-no.
           If just the job-no is scanned then first job-no2, form and blank is choosen.
           In the case of job is scanned with options and the next scan is the same job with just job then the other options might get updated 
           with first job-no2, form and blank no. To fix this issue screen-value of Job field is updated with entire job with options from
           previous scan */
        SELF:SCREEN-VALUE = cJob + "-" + cbJobNo2:SCREEN-VALUE 
                          + "-" + cbFormNo:SCREEN-VALUE + "-" + cbBlankNo:SCREEN-VALUE.
    END.
    
    RUN JobParser IN hdJobProcs (
        INPUT  SELF:SCREEN-VALUE,
        OUTPUT cJobNo,
        OUTPUT cJobNo2,
        OUTPUT cFormNo,
        OUTPUT cBlankNo,
        OUTPUT lParse,
        OUTPUT cMessage
        ).      

    IF cMessage NE "" THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    IF NOT lParse THEN
        cJobNo = SELF:SCREEN-VALUE.
        
    cFormattedJobno = DYNAMIC-FUNCTION (
                      "fAddSpacesToString" IN hdJobProcs, cJobNo, 6, TRUE
                      ).
    
    RUN pUpdateJobDetails (
        INPUT "JobNo",
        INPUT cFormattedJobNo,
        INPUT ""
        ).

    RUN pGetJobNo2 (
        INPUT  cCompany,
        INPUT  cFormattedJobno,
        OUTPUT iJobNo2,
        OUTPUT cJobNo2ListItems 
        ).
    IF lParse AND cJobNo2 NE "" AND INDEX(cJobNo2ListItems,STRING(INTEGER(cJobNo2),"99")) LE 0 THEN DO:
        MESSAGE "Invalid Job Scan, please scan a valid Job Number." 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.            
    END.
    
    IF cJobNo2 NE "" THEN
        iJobNo2 = INTEGER(cJobNo2).
        
    RUN pUpdateJobDetails (
        INPUT "JobNo2",
        INPUT STRING(iJobNo2),
        INPUT cJobNo2ListItems
        ).
    
    RUN pGetFormNo (
        INPUT  cCompany,
        INPUT  cFormattedJobno,
        INPUT  iJobNo2,
        OUTPUT iFormNo,  
        OUTPUT cFormNoListItems 
        ).  
    IF lParse AND cFormNo NE "" AND INDEX(cFormNoListItems,STRING(INTEGER(cFormNo),"99")) LE 0 THEN DO:
        MESSAGE "Invalid Job Scan, please scan a valid Job Number." 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.            
    END.

    IF cFormNo NE "" THEN
        iFormNo = INTEGER(cFormNo).
    
    RUN pUpdateJobDetails (
        INPUT "FormNo",
        INPUT STRING(iFormNo),
        INPUT cFormNoListItems
        ).

    RUN pGetBlankNo (
        INPUT  cCompany,
        INPUT  cFormattedJobno,
        INPUT  iJobNo2,
        OUTPUT iBlankNo,  
        OUTPUT cBlankNoListItems 
        ).  
    IF lParse AND cBlankNo NE "" AND INDEX(cBlankNoListItems,STRING(INTEGER(cBlankNo),"99")) LE 0 THEN DO:
        MESSAGE "Invalid Job Scan, please scan a valid Job Number." 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.            
    END.

    IF cBlankNo NE "" THEN
        iBlankNo = INTEGER(cBlankNo).
    
    RUN pUpdateJobDetails (
        INPUT "BlankNo",
        INPUT STRING(iBlankNo),
        INPUT cBlankNoListItems
        ).

    RUN pValidateJob.
    
    /* Progress doesn't have an option to apply entry to field once leave event is triggered.
       If a case is necessary where an entry is required right after leave trigger RETURN with NO-APPLY
       will solve the issue  */
    IF lScanNextJob THEN DO:
        cJob = fiJobNo:SCREEN-VALUE.
        
        fiJobNo:SET-SELECTION(1, LENGTH(fiJobNo:SCREEN-VALUE) + 1).

        RUN new-state (
            INPUT "job-invalid"
            ).
        
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imJobLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imJobLookup s-object
ON MOUSE-SELECT-CLICK OF imJobLookup IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        INPUT  "", 
        INPUT  "",  /* lookup field */
        INPUT  3,   /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT cFieldsValue, 
        OUTPUT cFoundValue, 
        OUTPUT recFoundRecID
        ).
    
    IF cFoundValue NE "" THEN
        fiJobNo:SCREEN-VALUE = cFoundValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

RUN pInit.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetJobDetails s-object 
PROCEDURE GetJobDetails :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcJobNo   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiFormNo  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBlankNo AS INTEGER   NO-UNDO.    
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    ASSIGN
        opcJobNo   = fiJobNo:SCREEN-VALUE
        opiJobNo2  = INTEGER(cbJobNo2:SCREEN-VALUE)
        opiFormNo  = INTEGER(cbFormNo:SCREEN-VALUE)
        opiBlankNo = INTEGER(cbBlankNo:SCREEN-VALUE)
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetJobHeader s-object 
PROCEDURE GetJobHeader :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoJobHeader AS JobHeader NO-UNDO.

    opoJobHeader = oJobHeader. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE No-Resize s-object 
PROCEDURE No-Resize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetBlankNo s-object 
PROCEDURE pGetBlankNo PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2           AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBlankNo          AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBlankNoListItems AS CHARACTER NO-UNDO.

    RUN GetBlanknoForJobHeader IN hdJobProcs (
        INPUT ipcCompany,
        INPUT ipcJobNo,
        INPUT ipiJobNo2,
        INPUT-OUTPUT opcBlankNoListItems
        ).

    IF opcBlankNoListItems EQ "" THEN
        opcBlankNoListItems = "00".
    ELSE
        opiBlankNo = INTEGER(ENTRY(1, opcBlankNoListItems)) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFormNo s-object 
PROCEDURE pGetFormNo PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2          AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiFormNo          AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFormNoListItems AS CHARACTER NO-UNDO.

    RUN GetFormnoForJobHeader IN hdJobProcs (
        INPUT ipcCompany,
        INPUT ipcJobNo,
        INPUT ipiJobNo2,
        INPUT-OUTPUT opcFormNoListItems
        ).

    IF opcFormNoListItems EQ "" THEN
        opcFormNoListItems = "00".
    ELSE
        opiFormNo = INTEGER(ENTRY(1, opcFormNoListItems)) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetJobNo2 s-object 
PROCEDURE pGetJobNo2 PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2          AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobno2ListItems AS CHARACTER NO-UNDO.
    
    RUN GetSecondaryJobForJob IN hdJobProcs (
        INPUT ipcCompany,
        INPUT ipcJobNo,
        INPUT-OUTPUT opcJobno2ListItems
        ).
    
    IF opcJobno2ListItems EQ "" THEN
        opcJobno2ListItems = "00".
    ELSE
        opiJobNo2 = INTEGER(ENTRY(1, opcJobno2ListItems)) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit s-object 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN spGetSessionParam ("Location", OUTPUT cLocation).
    
    ASSIGN
        fiJobNoLabel:SCREEN-VALUE   = "Job #:"
        fiFormNoLabel:SCREEN-VALUE  = "Form #:"
        fiBlankNoLabel:SCREEN-VALUE = "Blank #:"
        .      

    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobScan s-object 
PROCEDURE pJobScan PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER  oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER  opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidJob  AS LOGICAL   NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN new-state (
        INPUT "job-invalid"
        ).

    btJobDetails:SENSITIVE = FALSE.

    lValidJob = oJobHeader:SetContext (INPUT ipcCompany, INPUT ipcJobNo, INPUT ipiJobNo2, INPUT ipiFormNo, INPUT ipiBlankNo).
    
    btJobDetails:SENSITIVE = lValidJob.

    IF NOT lValidJob THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Invalid Job entry '" + ipcJobNo + "'"
            .
        RETURN.    
    END.
    
    RUN new-state (
        INPUT "job-valid"
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOnValueChangeOfJobDeails s-object 
PROCEDURE pOnValueChangeOfJobDeails PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValidJob AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    lValidJob = oJobHeader:SetContext (INPUT cCompany, INPUT cFormattedJobno, INPUT INTEGER(cbJobNo2:SCREEN-VALUE), INPUT INTEGER(cbFormNo:SCREEN-VALUE), INPUT INTEGER(cbBlankNo:SCREEN-VALUE)).
    
    IF NOT lValidJob THEN DO: 
        cMessage = "Invalid Job Entry".
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
    END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateJobDetails s-object 
PROCEDURE pUpdateJobDetails PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcWidget    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcListItems AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:        
    END.

    IF ipcWidget EQ "JobNo" THEN
        fiJobNo:SCREEN-VALUE = ipcValue.            
    ELSE IF ipcWidget EQ "JobNo2" THEN
        ASSIGN
            cbJobNo2:LIST-ITEMS    = ipcListItems
            cbJobNo2:SCREEN-VALUE  = STRING(INTEGER(ipcValue), "99")
            NO-ERROR.                    
    ELSE IF ipcWidget EQ "FormNo" THEN
        ASSIGN
            cbFormNo:LIST-ITEMS    = ipcListItems
            cbFormNo:SCREEN-VALUE  = STRING(INTEGER(ipcValue), "99")
            NO-ERROR.                    
    ELSE IF ipcWidget EQ "BlankNo" THEN
        ASSIGN
            cbBlankNo:LIST-ITEMS    = ipcListItems
            cbBlankNo:SCREEN-VALUE  = STRING(INTEGER(ipcValue), "99")
            NO-ERROR.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateJob s-object 
PROCEDURE pValidateJob PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN pJobScan (
        INPUT  cCompany,
        INPUT  fiJobNo:SCREEN-VALUE,
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT  INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT  INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lError,
        OUTPUT cMessage
        ).       
    IF lError THEN
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanNextJob s-object 
PROCEDURE ScanNextJob :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    lScanNextJob = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus s-object
PROCEDURE Set-Focus:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    APPLY "ENTRY" TO fiJobNo.    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

