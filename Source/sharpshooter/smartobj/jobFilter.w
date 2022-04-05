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

USING jc.*.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
DEFINE VARIABLE lShowErrorAsAlert  AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE cLocation          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJob               AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdJobDetails            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdJobDetailsWin         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdJobProcs              AS HANDLE      NO-UNDO.
DEFINE VARIABLE cFormattedJobno         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE oJobHeader              AS JobHeader   NO-UNDO.
DEFINE VARIABLE oJobMaterial            AS JobMaterial NO-UNDO.
DEFINE VARIABLE lScanNextJob            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidateSameJobScan    AS LOGICAL     NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lValidateJobClosed      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lAllowEmptyFormAndBlank AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lDisplayForMaterials    AS LOGICAL     NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE oKeyboard AS system.Keyboard NO-UNDO.

ASSIGN
    oJobHeader   = NEW JobHeader()
    oJobMaterial = NEW JobMaterial()
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS imJobLookup btnKeyboardJobNo cbJobNo2 ~
cbFormNo cbBlankNo fiJobNo btnJobDetailsText 
&Scoped-Define DISPLAYED-OBJECTS cbJobNo2 cbFormNo cbBlankNo fiJobNoLabel ~
fiJobNo fiFormNoLabel fiBlankNoLabel btnJobDetailsText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btJobDetails 
     IMAGE-UP FILE "Graphics\32x32\UDF.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/UDF_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "View Current Job Details".

DEFINE BUTTON btnKeyboardJobNo 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE VARIABLE cbBlankNo AS CHARACTER FORMAT "XX":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE cbFormNo AS CHARACTER FORMAT "XX":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE cbJobNo2 AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000" 
     DROP-DOWN-LIST
     SIZE 11.6 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE btnJobDetailsText AS CHARACTER FORMAT "X(256)":U INITIAL "DETAIL" 
      VIEW-AS TEXT 
     SIZE 13 BY 1.48 NO-UNDO.

DEFINE VARIABLE fiBlankNoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "BLANK:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.48 NO-UNDO.

DEFINE VARIABLE fiFormNoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "FORM:" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1.48 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.48
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiJobNoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "JOB:" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.48 NO-UNDO.

DEFINE IMAGE imJobLookup
     FILENAME "Graphics/32x32/search_new.png":U
     STRETCH-TO-FIT RETAIN-SHAPE
     SIZE 6.4 BY 1.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btJobDetails AT ROW 1 COL 129.6 WIDGET-ID 160
     btnKeyboardJobNo AT ROW 1.24 COL 138 WIDGET-ID 136 NO-TAB-STOP 
     cbJobNo2 AT ROW 1.24 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 162
     cbFormNo AT ROW 1.24 COL 72.4 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     cbBlankNo AT ROW 1.24 COL 96.8 COLON-ALIGNED NO-LABEL WIDGET-ID 166
     fiJobNoLabel AT ROW 1.29 COL 2 NO-LABEL WIDGET-ID 2
     fiJobNo AT ROW 1.29 COL 10 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiFormNoLabel AT ROW 1.29 COL 61.6 NO-LABEL WIDGET-ID 16
     fiBlankNoLabel AT ROW 1.29 COL 84.6 NO-LABEL WIDGET-ID 20
     btnJobDetailsText AT ROW 1.24 COL 116.6 NO-LABEL
     imJobLookup AT ROW 1.24 COL 109.6 WIDGET-ID 182
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


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
         HEIGHT             = 2.05
         WIDTH              = 146.2.
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
/* SETTINGS FOR FILL-IN btnJobDetailsText IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       btnKeyboardJobNo:HIDDEN IN FRAME F-Main           = TRUE.

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
        RUN sharpshooter/w-jobInquiry.w PERSISTENT SET hdJobDetails.

        RUN dispatch IN hdJobDetails (
            INPUT 'initialize':U
            ) NO-ERROR.
        
        hdJobDetailsWin = hdJobDetails:CURRENT-WINDOW.
    END.

    IF VALID-HANDLE(hdJobDetails) AND
        VALID-HANDLE(hdJobDetailsWin) THEN DO:        
        RUN ScanJob IN hdJobDetails (
            INPUT cCompany,
            INPUT cLocation,
            INPUT fiJobno:SCREEN-VALUE,
            INPUT INTEGER(cbJobno2:SCREEN-VALUE),
            INPUT INTEGER(cbFormno:SCREEN-VALUE),
            INPUT INTEGER(cbBlankno:SCREEN-VALUE)
            ).            

        IF hdJobDetailsWin:WINDOW-STATE EQ 2 THEN ASSIGN 
            hdJobDetailsWin:WINDOW-STATE = 3.
        
        hdJobDetailsWin:MOVE-TO-TOP().
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobDetailsText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobDetailsText s-object
ON MOUSE-SELECT-CLICK OF btnJobDetailsText IN FRAME F-Main
DO:
    IF btJobDetails:SENSITIVE THEN
    APPLY "CHOOSE":U TO btJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardJobNo s-object
ON CHOOSE OF btnKeyboardJobNo IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiJobNo.    
    
    oKeyboard:OpenKeyboardOverride (fiJobNo:HANDLE, "Qwerty"). 
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
    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.
    
    RUN pValidateJobClosed (
        OUTPUT lError
        ).
    IF lError THEN
        RETURN NO-APPLY.
            
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
ON ENTER OF fiJobNo IN FRAME F-Main
DO:
    APPLY 'LEAVE' TO SELF. 
    IF lScanNextJob THEN
        RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo s-object
ON ENTRY OF fiJobNo IN FRAME F-Main
DO:
    IF VALID-OBJECT (oKeyboard) AND NOT oKeyboard:IsKeyboardOpen() THEN
        cJob = SELF:SCREEN-VALUE.  
    
    fiJobNo:SET-SELECTION(1, -1).
    
    SELF:BGCOLOR = 30.

    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:OpenKeyboard (SELF, "Qwerty").
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
    DEFINE VARIABLE lChoice    AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cJobNo2ListItems  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNoListItems  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNoListItems AS CHARACTER NO-UNDO.
    
    lScanNextJob = FALSE.

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .
    
    /* If last key is not button choose or mouse click event */
    IF (LASTKEY EQ -1 OR (LASTKEY GE 609 AND LASTKEY LE 652)) AND NOT (VALID-OBJECT (oKeyboard) AND oKeyboard:IsKeyboardOpen()) THEN
        RETURN.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    IF SELF:SCREEN-VALUE EQ cJob AND lValidateSameJobScan THEN DO:
        cMessage = "THE JOB '" + cJob + "' IS ALREADY SCANNED. ~n"
                 + "DO YOU WANT SCAN THE SAME JOB AGAIN?".

        RUN sharpShooter/messageDialog.w (
            cMessage,
            YES,
            YES,
            NO,
            OUTPUT lChoice
            ).
                    
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
        RUN pSendError (cMessage).
        
        RETURN.
    END.
    
    IF NOT lParse THEN
        cJobNo = SELF:SCREEN-VALUE.
        
    cFormattedJobno = DYNAMIC-FUNCTION (
                      "fAddSpacesToString" IN hdJobProcs, cJobNo, 9, TRUE
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
    IF lParse AND cJobNo2 NE "" AND INDEX(cJobNo2ListItems,STRING(INTEGER(cJobNo2),"999")) LE 0 THEN DO:
        RUN pSendError ("INVALID JOB SCAN, PLEASE SCAN A VALID JOB NUMBER.").
        
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
        RUN pSendError ("INVALID JOB SCAN, PLEASE SCAN A VALID JOB NUMBER.").
         
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
        RUN pSendError ("INVALID JOB SCAN, PLEASE SCAN A VALID JOB NUMBER.").
        
        RETURN.            
    END.

    IF cBlankNo NE "" THEN
        iBlankNo = INTEGER(cBlankNo).
    
    RUN pUpdateJobDetails (
        INPUT "BlankNo",
        INPUT STRING(iBlankNo),
        INPUT cBlankNoListItems
        ).

    RUN pValidateJobClosed (
        OUTPUT lError
        ).
    IF lError THEN
        RETURN NO-APPLY.
        
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
    
    SELF:BGCOLOR = 15.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AllowEmptyFormAndBlank s-object 
PROCEDURE AllowEmptyFormAndBlank :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    lAllowEmptyFormAndBlank = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableAll s-object 
PROCEDURE DisableAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        DISABLE fiJobNo cbJobNo2 cbFormNo cbBlankNo imJobLookup btJobDetails.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableErrorAlerts s-object 
PROCEDURE DisableErrorAlerts :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    lShowErrorAsAlert = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayForMaterials s-object
PROCEDURE DisplayForMaterials:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    lDisplayForMaterials = TRUE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableAll s-object
PROCEDURE EnableAll:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ENABLE fiJobNo cbJobNo2 cbFormNo cbBlankNo imJobLookup btJobDetails.
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetJob s-object 
PROCEDURE GetJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER  opcJobno    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER  opiJobno2   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER  opiFormno   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER  opiBlankno  AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        opcJobNo   = fiJobNo:SCREEN-VALUE
        opiJobNo2  = IF cbJobNo2:SCREEN-VALUE EQ " " THEN ? ELSE INTEGER(cbJobNo2:SCREEN-VALUE)
        opiFormNo  = IF cbFormNo:SCREEN-VALUE EQ " " THEN ? ELSE INTEGER(cbFormNo:SCREEN-VALUE)
        opiBlankNo = IF cbBlankNo:SCREEN-VALUE EQ " " THEN ? ELSE INTEGER(cbBlankNo:SCREEN-VALUE)
        .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMessageAndType s-object 
PROCEDURE GetMessageAndType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiStatusMessageType AS INTEGER   NO-UNDO.
    
    ASSIGN
        opcStatusMessage     = cStatusMessage
        opiStatusMessageType = iStatusMessageType
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideFormAndBlank s-object 
PROCEDURE HideFormAndBlank :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        HIDE cbFormNo cbBlankNo fiFormNoLabel fiBlankNoLabel.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideJobDetails s-object 
PROCEDURE HideJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        HIDE btJobDetails btnJobDetailsText.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JobFGItemChanged s-object 
PROCEDURE JobFGItemChanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcItemID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFormNoList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNoList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN GetFormAndBlankFromJobAndFGItem IN hdJobProcs (
        INPUT  cCompany,
        INPUT  fiJobno:SCREEN-VALUE,
        INPUT  INTEGER(cbJobno2:SCREEN-VALUE),
        INPUT  ipcItemID,
        OUTPUT cFormNoList,
        OUTPUT cBlankNoList
        ).
    
    /* Raise an error if output form and blank are not found on the current form and blank list */
    IF LOOKUP(STRING(INTEGER(ENTRY (1, cFormNoList)) ,"99") , cbFormNo:LIST-ITEMS) EQ 0 OR LOOKUP(STRING(INTEGER(ENTRY (1, cBlankNoList)), "99") , cbBlankNo:LIST-ITEMS) EQ 0 THEN DO:
        RUN pSendError ("Invalid item '" + ipcItemID + "' for Job # '" + fiJobno:SCREEN-VALUE + "-" + cbJobno2:SCREEN-VALUE + "'").

        RETURN.
    END.
    
    ASSIGN
        cbFormNo:SCREEN-VALUE  = STRING(INTEGER(ENTRY (1, cFormNoList)) ,"99")
        cbBlankNo:SCREEN-VALUE = STRING(INTEGER(ENTRY (1, cBlankNoList)), "99")
        .
    
    RUN pValidateJob.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy s-object 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hdJobProcs) THEN
  DELETE PROCEDURE hdJobProcs.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
    
    IF lDisplayForMaterials THEN
        RUN GetBlanknoForJob IN hdJobProcs (
            INPUT ipcCompany,
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT-OUTPUT opcBlankNoListItems
            ).
    ELSE    
        RUN GetBlanknoForJobHeaderByStatus IN hdJobProcs (
            INPUT ipcCompany,
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT ?, /* Fetch both opened and closed jobs */
            INPUT-OUTPUT opcBlankNoListItems
            ).

    IF opcBlankNoListItems EQ "" THEN
        opcBlankNoListItems = "00".
    ELSE
        opiBlankNo = INTEGER(ENTRY(1, opcBlankNoListItems)) NO-ERROR.

    IF lAllowEmptyFormAndBlank THEN
        ASSIGN
            opcBlankNoListItems = " " + "," + opcBlankNoListItems
            opiBlankNo = ?
            NO-ERROR.
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

    IF lDisplayForMaterials THEN
        RUN GetFormnoForJob IN hdJobProcs (
            INPUT ipcCompany,
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT-OUTPUT opcFormNoListItems
            ).    
    ELSE
        RUN GetFormnoForJobHeaderByStatus IN hdJobProcs (
            INPUT ipcCompany,
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT ?, /* Fetch both opened and closed jobs */
            INPUT-OUTPUT opcFormNoListItems
            ).

    IF opcFormNoListItems EQ "" THEN
        opcFormNoListItems = "00".
    ELSE
        opiFormNo = INTEGER(ENTRY(1, opcFormNoListItems)) NO-ERROR.

    IF lAllowEmptyFormAndBlank THEN
        ASSIGN
            opcFormNoListItems = " " + "," + opcFormNoListItems
            opiFormNo = ?
            NO-ERROR.
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
    
    RUN GetSecondaryJobForJobByStatus IN hdJobProcs (
        INPUT ipcCompany,
        INPUT ipcJobNo,
        INPUT ?, /* Fetch both opened and closed jobs */
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
        fiJobNoLabel:SCREEN-VALUE   = "JOB:"
        fiFormNoLabel:SCREEN-VALUE  = "FORM:"
        fiBlankNoLabel:SCREEN-VALUE = "BLANK:"
        btnJobDetailsText:SCREEN-VALUE = "DETAIL"
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

    IF lDisplayForMaterials THEN
        lValidJob = oJobMaterial:SetContext (INPUT ipcCompany, INPUT ipcJobNo, INPUT ipiJobNo2, INPUT ipiFormNo, INPUT ipiBlankNo).
    ELSE
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
        RUN pSendError (cMessage).
    END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSendError s-object 
PROCEDURE pSendError :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcStatusMessage AS CHARACTER NO-UNDO.
    
    IF lShowErrorAsAlert THEN DO:
        MESSAGE ipcStatusMessage
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.
    
    ASSIGN
        cStatusMessage     = ipcStatusMessage
        iStatusMessageType = 3
        .
        
    RUN new-state (
        "job-error"
        ).

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .
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
            cbJobNo2:SCREEN-VALUE  = STRING(INTEGER(ipcValue), "999")
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
        RUN pSendError (cMessage).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateJobClosed s-object 
PROCEDURE pValidateJobClosed PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.

    DEFINE VARIABLE lJobClosed AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lResponse  AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF NOT lValidateJobClosed THEN
        RETURN.
        
    RUN IsJobClosed IN hdJobProcs (
        INPUT  cCompany,
        INPUT  fiJobNo:SCREEN-VALUE,
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        OUTPUT lJobClosed
        ).
    IF NOT lJobClosed THEN
        RETURN.
        
    RUN displayMessageQuestionDialog (
        INPUT  "67",
        OUTPUT lResponse
        ).
        
    oplError = NOT lResponse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset s-object 
PROCEDURE Reset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiJobNo:SCREEN-VALUE   = ""
        cbJobNo2:LIST-ITEMS    = " "
        cbJobNo2:SCREEN-VALUE  = " "
        cbFormNo:LIST-ITEMS    = " "
        cbFormNo:SCREEN-VALUE  = " "
        cbBlankNo:LIST-ITEMS   = " "
        cbBlankNo:SCREEN-VALUE = " "
        .            
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
PROCEDURE Set-Focus :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetJob s-object 
PROCEDURE SetJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiJobNo:SCREEN-VALUE   = ipcJobNo
        cbJobNo2:LIST-ITEMS    = STRING(ipiJobno2)
        cbJobNo2:SCREEN-VALUE  = STRING(ipiJobno2)
        cbFormNo:LIST-ITEMS    = STRING(ipiFormno)
        cbFormNo:SCREEN-VALUE  = STRING(ipiFormno)
        cbBlankNo:LIST-ITEMS   = STRING(ipiBlankno)        
        cbBlankNo:SCREEN-VALUE = STRING(ipiBlankno)        
        .       

    RUN pValidateJob.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetKeyboard s-object 
PROCEDURE SetKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipoKeyboard AS system.Keyboard NO-UNDO.
    
    oKeyboard = ipoKeyboard.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard s-object
PROCEDURE ShowKeyboard:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        btnKeyboardJobNo:VISIBLE   = TRUE
        btnKeyboardJobNo:SENSITIVE = TRUE
        .
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateJobClosed s-object 
PROCEDURE ValidateJobClosed :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplValidateJobClosed AS LOGICAL NO-UNDO.
    
    lValidateJobClosed = iplValidateJobClosed.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateSameJobScan s-object 
PROCEDURE ValidateSameJobScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplValidateSameJobScan AS LOGICAL NO-UNDO.
    
    lValidateSameJobScan = iplValidateSameJobScan.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

