&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: AOA/aoaParam.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:  company, location and parameter string

  Output Parameters: <none>

  History: Ron Stark - 3.7.2016
  Updated: Ron Stark - 12.5.2018 (Jasper Enabled)
          
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

DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcParamStr AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{AOA/includes/aoaParamDefs.i}

DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrv          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParamFrame      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hTasks           AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx              AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSecure          AS LOGICAL   NO-UNDO.

{AOA/includes/ttColumn.i}

RUN AOA\appServer\aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
RUN AOA\aoaJasper.p PERSISTENT SET hJasper.
SESSION:ADD-SUPER-PROCEDURE (hJasper).

DEFINE BUFFER jasperUserPrint FOR user-print.

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME paramFrame

/* Custom List Definitions                                              */
/* ScheduleFields,showFields,batchObjects,batchShowHide,columnObjects,jasperOptions */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGenerateInclude W-Win 
FUNCTION fGenerateInclude RETURNS LOGICAL
  ( iphFrame AS HANDLE, ipcType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCompany W-Win 
FUNCTION fGetCompany RETURNS CHARACTER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLocation W-Win 
FUNCTION fGetLocation RETURNS CHARACTER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetDescription W-Win 
FUNCTION fSetDescription RETURNS CHARACTER
  ( ipObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE b_aoataskparam AS HANDLE NO-UNDO.
DEFINE VARIABLE h_aoacolumns AS HANDLE NO-UNDO.
DEFINE VARIABLE h_aoapanel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_aoaParam AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.2 BY 18.62
         BGCOLOR 15 FGCOLOR 1 .


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
         TITLE              = "AdvantzwareOA"
         HEIGHT             = 18.62
         WIDTH              = 151.2
         MAX-HEIGHT         = 18.62
         MAX-WIDTH          = 151.2
         VIRTUAL-HEIGHT     = 18.62
         VIRTUAL-WIDTH      = 151.2
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
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
/* SETTINGS FOR FRAME paramFrame
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME paramFrame
/* Query rebuild information for FRAME paramFrame
     _Query            is NOT OPENED
*/  /* FRAME paramFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* AdvantzwareOA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* AdvantzwareOA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF VALID-HANDLE(hTasks) THEN
  RUN disable_UI IN hTasks.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

{&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " " + aoaType + " - " + aoaTitle.
RUN VALUE("AOA/appServer/aoa" + DYNAMIC-FUNCTION("fGetModule",aoaType,aoaProgramID) + ".p")
    PERSISTENT SET hAppSrv.

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
             INPUT  'gui/adm/objects/folder.w':U ,
             INPUT  FRAME paramFrame:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Parameters|Columns|Tasks' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 18.57 , 151.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'AOA/aoaPanel.w':U ,
             INPUT  FRAME paramFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_aoapanel ).
       RUN set-position IN h_aoapanel ( 1.00 , 68.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.10 , 55.40 ) */

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_aoapanel ,
             h_folder , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'AOA/aoaParamHolder.w':U ,
           &ELSE
             INPUT aoaParam ,
           &ENDIF
             INPUT  FRAME paramFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_aoaParam ).
       RUN set-position IN h_aoaParam ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.00 , 149.20 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_aoaParam ,
             h_aoapanel , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'AOA/aoaColumns.w':U ,
             INPUT  FRAME paramFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_aoacolumns ).
       RUN set-position IN h_aoacolumns ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.19 , 148.60 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_aoacolumns ,
             h_aoapanel , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'AOA/aoaTaskParam.w':U ,
             INPUT  FRAME paramFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT b_aoataskparam ).
       RUN set-position IN b_aoataskparam ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.24 , 148.40 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( b_aoataskparam ,
             h_aoapanel , 'AFTER':U ).
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

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
  VIEW FRAME paramFrame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pSetWinSize.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pPopulateOptions IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.
  RUN pGetUserPrint (?).
  RUN pInitColumnsPage.
  RUN pParamValuesOverride IN h_aoaParam NO-ERROR.
  RUN pInitialize IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcelCSV W-Win 
PROCEDURE pExcelCSV :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(h_aoaColumns) THEN
    RUN pExcelCSV IN h_aoaColumns.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcelXLS W-Win 
PROCEDURE pExcelXLS :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(h_aoaColumns) THEN
    RUN pExcelXLS IN h_aoaColumns.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateInclude W-Win 
PROCEDURE pGenerateInclude :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    DEFINE VARIABLE hFrame AS HANDLE NO-UNDO.
    
    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hFrame) THEN RETURN.

    IF NOT CAN-DO("ASI,NoSweat",USERID("ASI")) THEN RETURN.

    OUTPUT TO VALUE("AOA/includes/p" + REPLACE(aoaTitle," ","") + ".i") NO-ECHO.
    PUT UNFORMATTED
        "/* p" REPLACE(aoaTitle," ","") ".i - auto generated "
        STRING(TODAY,"99.99.9999") " @ " STRING(TIME,"hh:mm:ss am")
        " from AOA/aoaParam.w */"
        SKIP(1)
        "    ~{AOA/includes/aoaInputDefParams.i}" SKIP(1)
        "    /* parameter values loaded into these variables */" SKIP
        .    
    fGenerateInclude(hFrame,"DefVar").    
    PUT UNFORMATTED
        "    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO." SKIP
        "    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO." SKIP
        "    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO." SKIP(1)
        "    /* locate parameter values record */" SKIP
        "    RUN pGetUserPrint (ipcCompany, ~"" aoaProgramID "~", ipcUserID, ipiBatch)." SKIP(1)
        "    /* load parameter values from above record into variables */" SKIP
        "    ASSIGN" SKIP
        .    
    fGenerateInclude(hFrame,"DynFunc").
    PUT UNFORMATTED
        "        lSecure = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svSecure~") EQ ~"yes~"" SKIP
        "        cAvailableColumns = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svAvailableColumns~")" SKIP
        "        cSelectedColumns = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svSelectedColumns~")" SKIP
        "        ." SKIP(1)
        "    RUN pGetColumns (TEMP-TABLE tt" REPLACE(aoaTitle," ","") ":HANDLE, "
        "cAvailableColumns, "
        "cSelectedColumns"
        ")." SKIP(1)
        .
    fGenerateInclude(hFrame,"svAllGen").
    OUTPUT CLOSE.
    MESSAGE "AOA/includes/p" + REPLACE(aoaTitle," ","") + ".i" SKIP(1)
        "View Generated Code?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        TITLE "Auto Generated"
        UPDATE viewCode AS LOGICAL
        .
    IF viewCode THEN
    OS-COMMAND NO-WAIT notepad.exe VALUE("AOA/includes/p" + REPLACE(aoaTitle," ","") + ".i").
&ENDIF

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
    
    opcCompany = aoaCompany.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetContainerSize W-Win 
PROCEDURE pGetContainerSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiHeight AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiWidth  AS INTEGER NO-UNDO.
    
    ASSIGN
        opiHeight = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 35
        opiWidth  = FRAME {&FRAME-NAME}:WIDTH-PIXELS  - 10
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGethAppSrv W-Win 
PROCEDURE pGethAppSrv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ophAppSrv AS HANDLE NO-UNDO.
    
    ophAppSrv = hAppSrv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGethAppSrvBin W-Win 
PROCEDURE pGethAppSrvBin :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ophAppSrvBin AS HANDLE NO-UNDO.
    
    ophAppSrvBin = hAppSrvBin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetJasperUserPrintRowID W-Win 
PROCEDURE pGetJasperUserPrintRowID :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oprJasperUserPrintRowID AS ROWID NO-UNDO.
    
    RUN pGetJasperUserPrintRowID IN h_aoaColumns (OUTPUT oprJasperUserPrintRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetlColumns W-Win 
PROCEDURE pGetlColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplColumns AS LOGICAL NO-UNDO.
    
    oplColumns = aoaColumns.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetProgramID W-Win 
PROCEDURE pGetProgramID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcProgramID AS CHARACTER NO-UNDO.
    
    opcProgramID = aoaProgramID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSelectedColumns W-Win 
PROCEDURE pGetSelectedColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcSelectedColumns AS CHARACTER NO-UNDO.
    
    opcSelectedColumns = cSelectedColumns.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTitle W-Win 
PROCEDURE pGetTitle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcTitle AS CHARACTER NO-UNDO.
    
    opcTitle = aoaTitle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserID W-Win 
PROCEDURE pGetUserID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcUserID AS CHARACTER NO-UNDO.
    
    opcUserID = aoaUserID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrint W-Win 
PROCEDURE pGetUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

    DEFINE VARIABLE hFrame AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hChild AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx    AS INTEGER NO-UNDO.
    
    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hFrame) THEN RETURN.

    IF iprRowID NE ? THEN
    FIND user-print NO-LOCK WHERE ROWID(user-print) EQ iprRowID.
    ELSE DO:
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ aoaUserID
               AND user-print.prgmName   EQ ""
               AND user-print.batch      EQ ""
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ "_default"
               AND user-print.prgmName   EQ ""
               AND user-print.batch      EQ ""
             NO-ERROR.
    END. /* else */
    IF NOT AVAILABLE user-print THEN RETURN.

    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "" THEN LEAVE.
        IF user-print.field-name[idx] EQ "svSelectedColumns" THEN
        cSelectedColumns = user-print.field-value[idx].
    END. /* do idx */

    ASSIGN
        hChild = hFrame:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND (hChild:SENSITIVE OR hChild:TYPE EQ "COMBO-BOX") THEN DO:
            DO idx = 1 TO EXTENT(user-print.field-name):
                IF TRIM(user-print.field-name[idx]) EQ hChild:NAME THEN DO:
                    hChild:SCREEN-VALUE = user-print.field-value[idx].
                    LEAVE.
                END. /* found screen object */
            END. /* do idx */
        END. /* name <> ? */
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrintRowID W-Win 
PROCEDURE pGetUserPrintRowID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oprUserPrintRowID AS ROWID NO-UNDO.
    
    oprUserPrintRowID = ROWID(user-print).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitColumnsPage W-Win 
PROCEDURE pInitColumnsPage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF NOT VALID-HANDLE(h_aoaColumns) THEN DO:
        RUN init-object IN THIS-PROCEDURE (
               INPUT  'AOA/aoaColumns.w':U ,
               INPUT  FRAME paramFrame:HANDLE ,
               INPUT  '':U ,
               OUTPUT h_aoacolumns ).
        RUN set-position IN h_aoacolumns ( 2.43 , 2.00 ) NO-ERROR.
        RUN select-page IN THIS-PROCEDURE ( 2 ).
        RUN select-page IN THIS-PROCEDURE ( 1 ).
    END. /* not valid */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPassword W-Win 
PROCEDURE pPassword :
/*------------------------------------------------------------------------------
  Purpose:     prompt password for secured columns
  Parameters:  <none>
  Notes:       add additional secure programs/columns to pPassword.p
------------------------------------------------------------------------------*/
    RUN AOA/param/pPassword.p (
        aoaProgramID,
        cSelectedColumns,
        OUTPUT lSecure
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefreshColumnsPage W-Win 
PROCEDURE pRefreshColumnsPage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pRefreshColumnsPage IN h_aoaColumns.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunNow W-Win 
PROCEDURE pRunNow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTaskFormat AS CHARACTER NO-UNDO.
    
    DO TRANSACTION:
        CREATE Task.
        ASSIGN
            Task.company    = aoaCompany
            Task.user-id    = USERID("ASI")
            Task.programID  = aoaProgramID
            Task.taskName   = "Run Now " + aoaTitle
            Task.taskFormat = ipcTaskFormat
            Task.runNow     = YES
            .
        RELEASE Task.
    END. /* do trans */
    MESSAGE
        "Task ~"" + aoaTitle + "~" has been submitted."
    VIEW-AS ALERT-BOX TITLE "Run Now".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveJasperUserPrint W-Win 
PROCEDURE pSaveJasperUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplBatch AS LOGICAL NO-UNDO.
    
    RUN pSaveJasperUserPrint IN h_aoaColumns (iplBatch, ROWID(user-print)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveUserPrint W-Win 
PROCEDURE pSaveUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* number of reserved parameter fields needed */
    &SCOPED-DEFINE reserved 13
    
    DEFINE INPUT PARAMETER iplBatch AS LOGICAL NO-UNDO.

    DEFINE VARIABLE hFrame   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hChild   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cnt      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cColumns AS CHARACTER NO-UNDO.
    
    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hFrame) THEN RETURN.

    RUN pPassword.

    DO TRANSACTION:
        IF iplBatch THEN DO:
            CREATE user-print.
            ASSIGN
                user-print.company    = aoaCompany
                user-print.program-id = aoaProgramID
                user-print.user-id    = aoaUserID
                user-print.batch      = "Batch"
                user-print.last-date  = TODAY
                user-print.last-time  = TIME
                .
        END. /* if batch */
        ELSE IF NOT iplBatch THEN DO:
            FIND FIRST user-print EXCLUSIVE-LOCK
                 WHERE user-print.company    EQ aoaCompany
                   AND user-print.program-id EQ aoaProgramID
                   AND user-print.user-id    EQ aoaUserID
                   AND user-print.prgmName   EQ ""
                   AND user-print.batch      EQ ""
                 NO-ERROR.
            IF NOT AVAILABLE user-print THEN DO:
                CREATE user-print.
                ASSIGN
                    user-print.company    = aoaCompany
                    user-print.program-id = aoaProgramID
                    user-print.user-id    = aoaUserID
                    user-print.last-date  = TODAY
                    user-print.last-time  = TIME
                    .
            END. /* not avail */
        END. /* not batch, must be view now request */

        /* parameter values, currently up to 87 */
        ASSIGN
            user-print.next-date   = TODAY
            user-print.next-time   = TIME
            user-print.field-name  = ""
            user-print.field-value = ""
            user-print.field-label = ""
            hChild = hFrame:FIRST-CHILD
            hChild = hChild:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hChild):
            IF hChild:NAME NE ? AND
              (hChild:SENSITIVE OR
               hChild:TYPE EQ "COMBO-BOX") AND
               hChild:TYPE NE "BUTTON" THEN DO:
                ASSIGN
                    idx = idx + 1
                    user-print.field-name[idx]  = hChild:NAME
                    user-print.field-label[idx] = hChild:LABEL
                    user-print.field-value[idx] = hChild:SCREEN-VALUE
                    .
                /* if a date field and not fixed date, clear value so doesn't show wrong dates */
                /* values when showing parameter values in report header, especially batch run */
                IF hChild:PRIVATE-DATA NE ? AND hChild:PRIVATE-DATA NE "Fixed Date" THEN
                user-print.field-value[idx] = "".
            END. /* enabled field */
            hChild = hChild:NEXT-SIBLING.
            IF idx EQ EXTENT(user-print.field-name) - {&reserved} THEN LEAVE.
        END. /* do while */
         
        /* reserve 2, 1 for security, another for title */
        IF idx LE EXTENT(user-print.field-name) - {&reserved} THEN
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = "svSecure"
            user-print.field-label[idx] = "Secure"
            user-print.field-value[idx] = STRING(lSecure)
            idx = idx + 1
            user-print.field-name[idx]  = "svTitle"
            user-print.field-label[idx] = "Title"
            user-print.field-value[idx] = aoaTitle
            .

        RUN pSaveUserPrint IN h_aoaColumns (ROWID(user-print),INPUT-OUTPUT idx).
    END. /* do trans */
    IF AVAILABLE user-print THEN
    FIND CURRENT user-print NO-LOCK.  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSelectPage W-Win 
PROCEDURE pSelectPage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiPageNo AS INTEGER NO-UNDO.
    
    RUN select-page IN THIS-PROCEDURE ( ipiPageNo ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSethTasks W-Win 
PROCEDURE pSethTasks :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphTasks AS HANDLE NO-UNDO.
    
    hTasks = iphTasks.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetWinSize W-Win 
PROCEDURE pSetWinSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iWidth AS INTEGER NO-UNDO.

    IF NOT VALID-HANDLE(h_aoaParam) THEN RETURN.

    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hParamFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hParamFrame) THEN RETURN.

    DO WITH FRAME {&FRAME-NAME}:
        iWidth = hParamFrame:WIDTH-PIXELS.
        IF iWidth LT 748 THEN
        iWidth = 748.
        ASSIGN
            {&WINDOW-NAME}:WIDTH-PIXELS               = iWidth + 7
            {&WINDOW-NAME}:HEIGHT-PIXELS              = hParamFrame:HEIGHT-PIXELS + 35
            {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS      = {&WINDOW-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS       = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:WIDTH-PIXELS          = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:HEIGHT-PIXELS         = {&WINDOW-NAME}:HEIGHT-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
            .
        RUN set-size IN h_folder (
            FRAME {&FRAME-NAME}:HEIGHT - .04,
            FRAME {&FRAME-NAME}:WIDTH - .2
            ).
    END. /* with frame  */
    IF 1024 - {&WINDOW-NAME}:WIDTH-PIXELS  LT 0 OR
        768 - {&WINDOW-NAME}:HEIGHT-PIXELS LT 0 THEN
    MESSAGE
        "Width:" {&WINDOW-NAME}:WIDTH-PIXELS
        "(" 1024 - {&WINDOW-NAME}:WIDTH-PIXELS ")"
        SKIP 
        "Height:" {&WINDOW-NAME}:HEIGHT-PIXELS
        "(" 768 - {&WINDOW-NAME}:HEIGHT-PIXELS ")"    
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pURL W-Win 
PROCEDURE pURL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE fieldValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE testDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.

    aoaURL = "http://" + aoaHost + ":80/AdvantzwareOA/"
           + aoaType + ".html?ID=" + aoaID
           + "^&svCompany=" + aoaCompany
           + "^&svBatch=0"
           + "^&svUserID=" + aoaUserID
           .
    IF aoaType EQ "Report" THEN
    ASSIGN aoaURL = aoaURL + "^&refresh=true^&connection=AdvantzwareOA".

    OS-COMMAND NO-WAIT start VALUE(aoaURL).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGenerateInclude W-Win 
FUNCTION fGenerateInclude RETURNS LOGICAL
  ( iphFrame AS HANDLE, ipcType AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    DEFINE VARIABLE hChild      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hRange      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAllRange   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartRange AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndRange   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPreFix     AS CHARACTER NO-UNDO INITIAL "c,d,dt,i,l".
    DEFINE VARIABLE cTypes      AS CHARACTER NO-UNDO INITIAL "CHARACTER,DECIMAL,DATE,INTEGER,LOGICAL".
    DEFINE VARIABLE cStartList  AS CHARACTER NO-UNDO INITIAL "CHR(32),0,1/1/1950,0".
    DEFINE VARIABLE cEndList    AS CHARACTER NO-UNDO INITIAL "CHR(254),99999999.99,12/31/2049,99999999".
    DEFINE VARIABLE cStartValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList   AS LOGICAL   NO-UNDO.

    ASSIGN
        hChild = iphFrame:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND hChild:SENSITIVE THEN DO:
            IF hChild:TYPE NE "Button" THEN DO:
                IF hChild:NAME NE "svCompany" THEN DO:
                    idx = LOOKUP(hChild:DATA-TYPE,cTypes).
                    CASE ipcType:
                        WHEN "DefVar" THEN
                        PUT UNFORMATTED
                            "    DEFINE VARIABLE " REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix))
                            " AS " hChild:DATA-TYPE " NO-UNDO."
                            SKIP
                            .
                        WHEN "DynFunc" THEN DO:
                            PUT UNFORMATTED
                                "        " REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix))
                                " = "
                                IF ENTRY(idx,cPreFix) EQ "dt" THEN "DATE(" ELSE ""
                                "DYNAMIC-FUNCTION(~"fGetParamValue~",~"" hChild:NAME "~")"
                                IF ENTRY(idx,cPreFix) EQ "dt" THEN ")" ELSE ""
                                IF ENTRY(idx,cPreFix) EQ "l" THEN " EQ ~"yes~"" ELSE ""
                                SKIP
                                .
                            IF INDEX(hChild:NAME,"DateOption") NE 0 THEN
                            PUT UNFORMATTED
                                "        "
                                REPLACE(REPLACE(hChild:NAME,"sv","dt"),"Option","")
                                " = DYNAMIC-FUNCTION(~"fDateOptionDate~","
                                REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix)) ","
                                REPLACE(REPLACE(hChild:NAME,"sv","dt"),"Option","") ")"
                                SKIP
                                .
                        END. /* dynfunc */
                        WHEN "svAllGen" THEN DO:
                            IF hChild:NAME BEGINS "svAll" THEN DO:
                                ASSIGN
                                    cAllRange = REPLACE(hChild:NAME,"svAll","")
                                    hRange    = iphFrame:FIRST-CHILD
                                    hRange    = hRange:FIRST-CHILD
                                    .
                                DO WHILE VALID-HANDLE(hRange):
                                    IF hRange:NAME NE ? AND hRange:SENSITIVE THEN DO:
                                        IF hRange:TYPE NE "Button" THEN DO:
                                            IF hRange:NAME NE "svCompany" THEN DO:
                                                IF hRange:NAME EQ "svStart" + cAllRange THEN
                                                ASSIGN
                                                    idx = LOOKUP(hRange:DATA-TYPE,cTypes)
                                                    cStartRange = REPLACE(hRange:NAME,"sv",ENTRY(idx,cPreFix))
                                                    cStartValue = ENTRY(idx,cStartList)
                                                    .
                                                IF hRange:NAME EQ "svEnd" + cAllRange THEN
                                                ASSIGN
                                                    idx = LOOKUP(hRange:DATA-TYPE,cTypes)
                                                    cEndRange = REPLACE(hRange:NAME,"sv",ENTRY(idx,cPreFix))
                                                    cEndValue = ENTRY(idx,cEndList)
                                                    .
                                            END. /* not svcompany */
                                        END. /* not button */
                                    END. /* name <> ? */
                                    hRange = hRange:NEXT-SIBLING.
                                END. /* do while */
                                idx = LOOKUP(hChild:DATA-TYPE,cTypes).
                                PUT UNFORMATTED
                                    "    IF " REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix)) " THEN" SKIP
                                    "    ASSIGN" SKIP
                                    "        " cStartRange " = " cStartValue SKIP
                                    "        " cEndRange "   = " cEndValue   SKIP
                                    "        ." SKIP(1)
                                    .
                            END. /* if svall */
                            IF hChild:NAME EQ "svCustList" THEN
                            lCustList = YES.
                        END.
                    END CASE.
                END. /* not svcompany */
            END. /* not button */
        END. /* name <> ? */
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    IF lCustList THEN
    PUT UNFORMATTED
        "    IF lCustList THEN" SKIP
        "    RUN pBuildCustList (ipcCompany, "
        "~"" aoaCustListForm "~", "
        "OUTPUT cStartCustNo, "
        "OUTPUT cEndCustNo, "
        "OUTPUT lCustList"
        ")." SKIP
        .
&ENDIF
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCompany W-Win 
FUNCTION fGetCompany RETURNS CHARACTER:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN aoaCompany.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLocation W-Win 
FUNCTION fGetLocation RETURNS CHARACTER:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN aoaLocation.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetDescription W-Win 
FUNCTION fSetDescription RETURNS CHARACTER
  ( ipObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  return descriptions for entered parameter values
    Notes:  add additional parameter fields to fSetDescription.p
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    
    RUN AOA/param/fSetDescription.p (ipObject:HANDLE, aoaCompany, OUTPUT cDescription).

    RETURN cDescription.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

