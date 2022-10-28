&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/smartobj/f-job.w.

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

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
DEFINE VARIABLE cUser              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE oJobHeader          AS jc.JobHeader       NO-UNDO.
DEFINE VARIABLE oItemFG             AS fg.ItemFG          NO-UNDO.
DEFINE VARIABLE oCustomer           AS Inventory.Customer NO-UNDO.
DEFINE VARIABLE oLoadTag            AS Inventory.Loadtag  NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

DEFINE VARIABLE cSettingValue                AS CHARACTER NO-UNDO.
DEFINE VARIABLE glAutoCreateLoadtagOnJobScan AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glAutoPrintLoadtagOnJobScan  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giDefaultPrintCopies         AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE giNK1PrintCopies             AS INTEGER   NO-UNDO INITIAL 1.

oCustomer = NEW Inventory.Customer().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_fgfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_printcopies AS HANDLE NO-UNDO.
DEFINE VARIABLE h_qtyunits AS HANDLE NO-UNDO.
DEFINE VARIABLE h_userfields AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCreate 
     LABEL "CREATE" 
     SIZE 16 BY 2.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btCreate AT ROW 1 COL 205 WIDGET-ID 60
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 221.8 BY 15.24
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: PERSISTENT-ONLY
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
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 15.24
         WIDTH              = 222.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR BUTTON btCreate IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreate F-Frame-Win
ON CHOOSE OF btCreate IN FRAME F-Main /* CREATE */
DO:
    RUN new-state ("create-tags-job").
      
    IF glAutoPrintLoadtagOnJobScan THEN DO:
        RUN new-state ("print-tags").
        
        RUN state-changed (
            INPUT THIS-PROCEDURE,
            INPUT "job-invalid"
            ).
            
        /* Makes the cursor stay in Job No field in JobFilter.w */
        {methods/run_link.i "JOB-SOURCE" "ScanNextJob"}
    END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/jobfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobfilter ).
       RUN set-position IN h_jobfilter ( 1.24 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 143.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/printcopies.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_printcopies ).
       RUN set-position IN h_printcopies ( 1.24 , 151.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 52.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/fgfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_fgfilter ).
       RUN set-position IN h_fgfilter ( 3.14 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.43 , 83.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/qtyunits.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_qtyunits ).
       RUN set-position IN h_qtyunits ( 6.95 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.29 , 131.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/userfields.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_userfields ).
       RUN set-position IN h_userfields ( 6.95 , 136.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.19 , 68.00 ) */

       /* Links to SmartObject h_jobfilter. */
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'JOB':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_printcopies. */
       RUN add-link IN adm-broker-hdl ( h_printcopies , 'COPIES':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_fgfilter. */
       RUN add-link IN adm-broker-hdl ( h_fgfilter , 'FGITEM':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_fgfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_qtyunits. */
       RUN add-link IN adm-broker-hdl ( h_qtyunits , 'QTY':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_userfields. */
       RUN add-link IN adm-broker-hdl ( h_userfields , 'State':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_userfields , 'USERFIELD':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_jobfilter ,
             btCreate:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_printcopies ,
             h_jobfilter , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgfilter ,
             h_printcopies , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_qtyunits ,
             h_fgfilter , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_userfields ,
             h_qtyunits , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDesignConfig F-Frame-Win 
PROCEDURE GetDesignConfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoDesignConfig AS system.Config NO-UNDO.

    opoDesignConfig = system.ConfigLoader:Instance:GetConfig("SSLoadTagJobDesign").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDetails F-Frame-Win 
PROCEDURE GetDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcJobNo             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2            AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiFormNo            AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBlankNo           AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemID            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustID            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantity          AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantityInSubUnit AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCopies            AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserField1        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserField2        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserField3        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserFieldValue3   AS CHARACTER NO-UNDO.

    {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
    
    ASSIGN
        opcJobNo   = oJobHeader:GetValue("JobNo")
        opiJobNo2  = INTEGER(oJobHeader:GetValue("JobNo2"))
        opiFormNo  = INTEGER(oJobHeader:GetValue("FormNo"))
        opiBlankNo = INTEGER(oJobHeader:GetValue("BlankNo"))
        .
    
    {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
    
    opcItemID = oItemFG:GetValue("ItemID").
    
    {methods/run_link.i "QTY-SOURCE" "GetQuantities" "(OUTPUT opiQuantity, OUTPUT opiQuantityInSubUnit, OUTPUT opiSubUnitsPerUnit)"}
    
    {methods/run_link.i "COPIES-SOURCE" "GetCopies" "(OUTPUT opiCopies)"}
    
    {methods/run_link.i "USERFIELD-SOURCE" "GetUserFields" "(OUTPUT opcUserField1, OUTPUT opcUserField2, OUTPUT opcUserField3, OUTPUT opcUserFieldValue1, OUTPUT opcUserFieldValue2, OUTPUT opcUserFieldValue3)" }
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMessageAndType F-Frame-Win 
PROCEDURE GetMessageAndType :
/*------------------------------------------------------------------------------
 Purpose:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable F-Frame-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit F-Frame-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturnValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lShowKeyboard AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE oKeyboard AS system.Keyboard NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN spGetSettingByName ("AutoCreateLoadtagOnJobScan", OUTPUT cSettingValue).
    glAutoCreateLoadtagOnJobScan = LOGICAL (cSettingValue).

    RUN spGetSettingByName ("AutoPrintLoadtagOnJobScan", OUTPUT cSettingValue).
    glAutoPrintLoadtagOnJobScan = LOGICAL (cSettingValue).

    RUN spGetSettingByName ("DefaultPrintCopies", OUTPUT cSettingValue).
    giDefaultPrintCopies = INTEGER (cSettingValue).
    
    {methods/run_link.i "CONTAINER-SOURCE" "GetKeyboard" "(OUTPUT oKeyboard)"}
    {methods/run_link.i "CONTAINER-SOURCE" "ShowKeyboard" "(OUTPUT lShowKeyboard)"}
    
    IF lShowKeyboard THEN
        RUN ShowKeyboard.
        
    {methods/run_link.i "JOB-SOURCE" "SetKeyboard" "(INPUT oKeyboard)"}
    {methods/run_link.i "COPIES-SOURCE" "SetKeyboard" "(INPUT oKeyboard)"}
        
    btCreate:HIDDEN = glAutoCreateLoadtagOnJobScan.
    
    RUN sys/ref/nk1look.p (
        INPUT  cCompany,           /* Company Code */ 
        INPUT  "LOADTAG",          /* sys-ctrl name */
        INPUT  "I",                /* Output return value */
        INPUT  NO,                 /* Use ship-to */
        INPUT  NO,                 /* ship-to vendor */
        INPUT  "",                 /* ship-to vendor value */
        INPUT  "",                 /* shi-id value */
        OUTPUT cReturnValue, 
        OUTPUT lRecFound
        ).    
    IF lRecFound THEN 
        giNK1PrintCopies = INTEGER(cReturnValue).
    
    IF giNK1PrintCopies GT 0 THEN
        {methods/run_link.i "COPIES-SOURCE" "SetCopies" "(INPUT giNK1PrintCopies)"}.  

    {methods/run_link.i "JOB-SOURCE" "ValidateJobClosed" "(TRUE)"}
    {methods/run_link.i "JOB-SOURCE" "DisableErrorAlerts"}        
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobError F-Frame-Win 
PROCEDURE pJobError PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "JOB-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
    
    RUN pSendError.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSendError F-Frame-Win 
PROCEDURE pSendError PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset F-Frame-Win 
PROCEDURE Reset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    {methods/run_link.i "JOB-SOURCE" "Reset"}
    {methods/run_link.i "FGItem-SOURCE" "Reset"}
    {methods/run_link.i "QTY-SOURCE" "SetQuantities" "(0,0,0,0)"}
    {methods/run_link.i "COPIES-SOURCE" "SetCopies" "(1)"}
    {methods/run_link.i "USERFIELD-SOURCE" "SetUserFields" "("","","","","","")"}
    {methods/run_link.i "QTY-SOURCE" "DisableQuantities"}
    {methods/run_link.i "FGITEM-SOURCE" "DisableFGItem"}   
    {methods/run_link.i "USERFIELD-SOURCE" "DisableUserFields"}
    {methods/run_link.i "COPIES-SOURCE" "DisableCopies"}
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    
    btCreate:SENSITIVE = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartFrame, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus F-Frame-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard F-Frame-Win 
PROCEDURE ShowKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    {methods/run_link.i "JOB-SOURCE" "ShowKeyboard"}
    {methods/run_link.i "COPIES-SOURCE" "ShowKeyboard"}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemID  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustID  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQuantity          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInSubUnit AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCopies            AS INTEGER NO-UNDO.

    DEFINE VARIABLE cUserField1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField3      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue3 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dOvers AS DECIMAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    RUN new-state ("empty-message").
    
    CASE p-state:
        WHEN "job-invalid" THEN DO:
            {methods/run_link.i "QTY-SOURCE" "DisableQuantities"}
            {methods/run_link.i "FGITEM-SOURCE" "DisableFGItem"}   
            {methods/run_link.i "USERFIELD-SOURCE" "DisableUserFields"}
            {methods/run_link.i "COPIES-SOURCE" "DisableCopies"}
            
            btCreate:SENSITIVE = FALSE.         
        END.
        WHEN "job-valid" THEN DO:
            
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                cJobNo   = oJobHeader:GetValue("JobNo")
                iJobNo2  = INTEGER(oJobHeader:GetValue("JobNo2"))
                iFormNo  = INTEGER(oJobHeader:GetValue("FormNo"))
                iBlankNo = INTEGER(oJobHeader:GetValue("BlankNo"))
                cCustID  = oJobHeader:GetValue("Customer")
                .

            oCustomer:SetContext(cCompany, cCustID).
            
            IF VALID-OBJECT(oCustomer) THEN
                iCopies = INTEGER(oCustomer:GetValue("PrintCopies")).
                        
            IF iCopies LE 0 THEN
                iCopies = giNK1PrintCopies.
                
            IF iCopies LE 0 THEN
                iCopies = giDefaultPrintCopies.

            IF iCopies LE 0 THEN
                iCopies = 1.
                
            {methods/run_link.i "COPIES-SOURCE" "SetCopies" "(INPUT iCopies)"}

            {methods/run_link.i "FGITEM-SOURCE" "UpdateItemForJob" "(INPUT cCompany, INPUT cJobNo, INPUT iJobNo2, INPUT iFormNo, INPUT iBlankNo)"}
            
            {methods/run_link.i "FGITEM-SOURCE" "EnableFGItem"} 
            {methods/run_link.i "COPIES-SOURCE" "EnableCopies"}
        END.
        WHEN "fgitem-changed" THEN DO:
            {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
            
            cItemID = oItemFG:GetValue("ItemID").
            
            {methods/run_link.i "JOB-SOURCE" "JobFGItemChanged" "(INPUT cItemID)"}
        END.
        WHEN "fgitem-valid" THEN DO:
            
            {methods/run_link.i "QTY-SOURCE" "EnableQuantities"}
            {methods/run_link.i "USERFIELD-SOURCE" "EnableUserFields"}
            
            {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                iQuantityInSubUnit = INTEGER(oItemFG:GetValue("QuantityInSubUnit"))
                iSubUnitsPerUnit   = INTEGER(oItemFG:GetValue("SubUnitsPerUnit"))
/*                iQuantity        = INTEGER(oJobHeader:GetValue("Quantity"))*/
                iQuantity          = iQuantityInSubUnit * iSubUnitsPerUnit
                .
            
            {methods/run_link.i "QTY-SOURCE" "SetQuantities" "(INPUT iQuantity, INPUT iQuantityInSubUnit, INPUT iSubUnitsPerUnit, INPUT 0)"}
            {methods/run_link.i "QTY-SOURCE" "SetOvers" "(INPUT 0)"} 
            
            IF NOT glAutoCreateLoadtagOnJobScan THEN           
                btCreate:SENSITIVE = TRUE.
            ELSE DO:
                RUN new-state ("create-tags-job"). 

                IF glAutoPrintLoadtagOnJobScan THEN DO:
                    RUN new-state ("print-tags").
                    
                    RUN state-changed (
                        INPUT THIS-PROCEDURE,
                        INPUT "job-invalid"
                        ).
                        
                    /* Makes the cursor stay in Job No field in JobFilter.w */
                    {methods/run_link.i "JOB-SOURCE" "ScanNextJob"}
                END.                         
            END.
        END.
        WHEN "overs-changed" THEN DO:
            {methods/run_link.i "USERFIELD-SOURCE" "GetOvers" "(OUTPUT dOvers)" }
            
            {methods/run_link.i "QTY-SOURCE" "SetOvers" "(INPUT dOvers)"}
        END.
        WHEN "job-error" THEN DO:
            RUN pJobError.
        END.
    END CASE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateSameJobScan F-Frame-Win
PROCEDURE ValidateSameJobScan:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplValidateSameJobScan AS LOGICAL NO-UNDO.
    
    {methods/run_link.i "JOB-SOURCE" "ValidateSameJobScan" "(iplValidateSameJobScan)"}
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


