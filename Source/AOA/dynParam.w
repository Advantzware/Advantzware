&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: dynParam.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Paraeters: <none>

  Output Parameters: <none>

  History: Ron Stark
  Created: 2.24.2019
          
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

&Scoped-define program-id dynParam.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE iParamSetID        AS INTEGER NO-UNDO.
DEFINE VARIABLE iUserSecurityLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE lContinue          AS LOGICAL NO-UNDO.

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","ParameterBuilder", YES, OUTPUT lContinue).
&ELSE
lContinue = YES.
&ENDIF
SESSION:SET-WAIT-STATE("").
IF lContinue EQ NO THEN RETURN.

iUserSecurityLevel = DYNAMIC-FUNCTION("sfUserSecurityLevel").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnErrorCheck btnRestoreDefaults 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_param AS HANDLE NO-UNDO.
DEFINE VARIABLE h_paramSet AS HANDLE NO-UNDO.
DEFINE VARIABLE h_paramSetDtl AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnErrorCheck 
     IMAGE-UP FILE "Graphics/16x16/save.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Error Check Report" 
     SIZE 4.4 BY 1.05 TOOLTIP "Error Check Report".

DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY 1.1 TOOLTIP "Restore Defaults".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnErrorCheck AT ROW 1.1 COL 71 WIDGET-ID 294
     btnRestoreDefaults AT ROW 1.1 COL 67 HELP
          "Restore Defaults" WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dynamic Parameters and Sets"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Dynamic Parameters and Sets */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Dynamic Parameters and Sets */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  RUN pSaveSettings (USERID("ASI")).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-RESIZED OF W-Win /* Dynamic Parameters and Sets */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnErrorCheck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnErrorCheck W-Win
ON CHOOSE OF btnErrorCheck IN FRAME F-Main /* Error Check Report */
DO:
    RUN pErrorCheck.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults W-Win
ON CHOOSE OF btnRestoreDefaults IN FRAME F-Main /* Defaults */
DO:
    RUN pGetSettings ("_default").
    RUN select-page (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
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
             INPUT  'adm/objects/folder.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Sets|Set Details|Parameters' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 28.57 , 160.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aoa/paramSet.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_paramSet ).
       RUN set-position IN h_paramSet ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 26.95 , 158.00 ) */

    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aoa/paramSetDtl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_paramSetDtl ).
       RUN set-position IN h_paramSetDtl ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 26.95 , 158.00 ) */

    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aoa/param.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_param ).
       RUN set-position IN h_param ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 26.95 , 158.00 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE btnErrorCheck btnRestoreDefaults 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN pGetSettings (USERID("ASI")).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pGetParamSetDtl.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pErrorCheck W-Win 
PROCEDURE pErrorCheck :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCalcProc        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescriptionProc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInitializeProc  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValidateProc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDynamicProc     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lErrors          AS LOGICAL   NO-UNDO.
    
    RUN AOA/spDynDescriptionProc.p PERSISTENT SET hDynamicProc.
    cDescriptionProc = hDynamicProc:INTERNAL-ENTRIES + ",".
    DELETE PROCEDURE hDynamicProc.
    RUN AOA/spDynInitializeProc.p  PERSISTENT SET hDynamicProc.
    cInitializeProc = hDynamicProc:INTERNAL-ENTRIES + ",".
    DELETE PROCEDURE hDynamicProc.
    RUN AOA/spDynValidateProc.p    PERSISTENT SET hDynamicProc.
    cValidateProc = hDynamicProc:INTERNAL-ENTRIES + ",".
    DELETE PROCEDURE hDynamicProc.
    RUN AOA/spDynCalcField.p       PERSISTENT SET hDynamicProc.
    cCalcProc = hDynamicProc:INTERNAL-ENTRIES + ",".
    DELETE PROCEDURE hDynamicProc.
    
    OUTPUT TO c:\tmp\DynParamErrors.txt.
    FOR EACH dynParam NO-LOCK
        WITH FRAME fDynParam WIDTH 170 TITLE "*** Dynamic Parameter ***" STREAM-IO
        :
        IF NOT CAN-DO(cDescriptionProc, dynParam.descriptionProc) THEN DO:
            lErrors = YES.
            DISPLAY
                dynParam.paramID
                dynParam.paramName
                dynParam.descriptionProc
                .
        END. /* if can-do */
        IF NOT CAN-DO(cInitializeProc, dynParam.initializeProc) THEN DO:
            lErrors = YES.
            DISPLAY
                dynParam.paramID
                dynParam.paramName
                dynParam.initializeProc
                .
        END. /* if can-do */
        IF NOT CAN-DO(cValidateProc, dynParam.validateProc) THEN DO:
            lErrors = YES.
            DISPLAY
                dynParam.paramID
                dynParam.paramName
                dynParam.validateProc
                .
        END. /* if can-do */
    END. /* each dynparam */
    FOR EACH dynParamSetDtl NO-LOCK
        WITH FRAME fDynParamSetDtl WIDTH 170 TITLE "*** Dynamic Parameter Set Detail ***" STREAM-IO
        :
        IF NOT CAN-DO(cDescriptionProc, dynParamSetDtl.descriptionProc) THEN DO:
            lErrors = YES.
            DISPLAY
                dynParamSetDtl.paramSetID
                dynParamSetDtl.paramID
                dynParamSetDtl.paramName
                dynParamSetDtl.descriptionProc
                .
        END. /* if can-do */
        IF NOT CAN-DO(cInitializeProc, dynParamSetDtl.initializeProc) THEN DO:
            lErrors = YES.
            DISPLAY
                dynParamSetDtl.paramSetID
                dynParamSetDtl.paramID
                dynParamSetDtl.paramName
                dynParamSetDtl.initializeProc
                .
        END. /* if can-do */
        IF NOT CAN-DO(cValidateProc, dynParamSetDtl.validateProc) THEN DO:
            lErrors = YES.
            DISPLAY
                dynParamSetDtl.paramSetID
                dynParamSetDtl.paramID
                dynParamSetDtl.paramName
                dynParamSetDtl.validateProc
                .
        END. /* if can-do */
    END. /* each dynparam */
    FOR EACH dynSubjectColumn NO-LOCK
        WITH FRAME fDynSubjectColumn WIDTH 100 TITLE "*** Dynamic Subject Column ***" STREAM-IO
        :
        IF NOT CAN-DO(cCalcProc, dynSubjectColumn.calcProc) THEN DO:
            lErrors = YES.
            DISPLAY
                dynSubjectColumn.subjectID
                dynSubjectColumn.sortOrder
                dynSubjectColumn.fieldName
                dynSubjectColumn.fieldLabel
                dynSubjectColumn.calcProc
                .
        END. /* if can-do */
    END. /* each dynsubjectcolumn */
    FOR EACH dynParamValue NO-LOCK
        WITH FRAME fDynPAramValue WIDTH 170 TITLE "*** Dynamic Parameter Value ***" STREAM-IO
        :
        FOR EACH dynValueColumn NO-LOCK
            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
              AND dynValueColumn.user-id      EQ dynParamValue.user-id
              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
              AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
               BY dynValueColumn.sortOrder
            :
            IF NOT CAN-DO(cCalcProc, dynValueColumn.calcProc) THEN DO:
                lErrors = YES.
                DISPLAY
                    dynParamValue.subjectID
                    dynParamValue.user-id
                    dynParamValue.paramValueID
                    dynParamValue.paramDescription
                    dynValueColumn.sortOrder
                    dynValueColumn.colName
                    dynValueColumn.colLabel
                    dynValueColumn.calcProc
                    .
            END. /* if can-do */
        END. /* each dynvaluecolumn */
        
/*        /* rstark - remove when depricated */                             */
/*        DO idx = 1 TO EXTENT(dynParamValue.calcProc):                     */
/*            IF dynParamValue.colName[idx] EQ "" THEN LEAVE.               */
/*            IF NOT CAN-DO(cCalcProc, dynParamValue.calcProc[idx]) THEN DO:*/
/*                lErrors = YES.                                            */
/*                DISPLAY                                                   */
/*                    dynParamValue.subjectID                               */
/*                    dynParamValue.user-id                                 */
/*                    dynParamValue.paramValueID                            */
/*                    dynParamValue.paramDescription                        */
/*                    idx                                                   */
/*                    dynParamValue.colName[idx]                            */
/*                    dynParamValue.colLabel[idx]                           */
/*                    dynParamValue.calcProc[idx]                           */
/*                    .                                                     */
/*            END. /* if can-do */                                          */
/*        END. /* do idx */                                                 */
    END. /* each dynsubjectcolumn */
    OUTPUT CLOSE.
    MESSAGE 
        CAPS(STRING(lErrors)) "Errors Found, View Report?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lErrors.
    IF lErrors THEN
    OS-COMMAND NO-WAIT notepad.exe c:\tmp\DynParamErrors.txt.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCompany W-Win 
PROCEDURE pGetCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCompany AS CHARACTER NO-UNDO.
    
    opcCompany = g_company.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamSetDtl W-Win 
PROCEDURE pGetParamSetDtl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pGetParamSetID IN h_paramSetDtl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamSetID W-Win 
PROCEDURE pGetParamSetID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiParamSetID AS INTEGER NO-UNDO.

    opiParamSetID = iParamSetID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings W-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ g_company
                      AND user-print.program-id EQ "{&programID}"
                      AND user-print.user-id    EQ "_default") THEN
    RUN pSaveSettings ("_default").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavPanel W-Win 
PROCEDURE pNavPanel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    RUN pNavPanel IN h_paramSet (iphNavPanel).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings W-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "{&program-id}"
            user-print.user-id    = ipcUserID
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* not avail */
    ASSIGN
        user-print.next-date   = TODAY
        user-print.next-time   = TIME
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .
    FIND CURRENT user-print NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetParamSetID W-Win 
PROCEDURE pSetParamSetID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiParamSetID AS INTEGER NO-UNDO.
    
    iParamSetID = ipiParamSetID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            dHeight                            = FRAME {&FRAME-NAME}:HEIGHT
            dWidth                             = FRAME {&FRAME-NAME}:WIDTH
            .
        RUN set-size IN h_folder (dHeight, dWidth).
        ASSIGN
            dHeight = dHeight - 1.62
            dWidth  = dWidth  - 2
            .
        IF NOT VALID-HANDLE(h_paramSetDtl) THEN
        RUN init-pages ("2").
        RUN pWinReSize IN h_paramSetDtl (dHeight, dWidth).
        IF NOT VALID-HANDLE(h_param) THEN
        RUN init-pages ("3").
        RUN pWinReSize IN h_param (dHeight, dWidth).
        RUN pWinReSize IN h_paramSet (dHeight, dWidth).
        VIEW FRAME {&FRAME-NAME}.
    END. /* do with */
    RUN select-page (1).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

