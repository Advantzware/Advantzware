&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: AOA/aoaPanel.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author:  Ron Stark
  Created: 12.5.2018

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEFINE VARIABLE lJasperStarter AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hTasks         AS HANDLE    NO-UNDO.
DEFINE VARIABLE char-hdl       AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle        AS HANDLE    NO-UNDO.
DEFINE VARIABLE iHeightPixels  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWidthPixels   AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnPrint btnHTML btnView btnPDF btnDOCX ~
btnSave btnDataPA btnCSV btnXLS 

/* Custom List Definitions                                              */
/* jasperObjects,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define jasperObjects btnTasks btnPrint btnHTML btnView btnPDF ~
btnDOCX 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCSV 
     IMAGE-UP FILE "AOA/images/aoaexcelcsv.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 4.4 BY 1 TOOLTIP "Excel CSV".

DEFINE BUTTON btnDataPA 
     IMAGE-UP FILE "AOA/images/aoaview.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Web Browser".

DEFINE BUTTON btnDOCX 
     IMAGE-UP FILE "AOA/images/aoaword.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Word DOCX".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "AOA/images/html_tag.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "HTML".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "AOA/images/aoapdf.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "PDF".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "AOA/images/printer.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.1 TOOLTIP "Print".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "AOA/images/floppy_disk.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Save".

DEFINE BUTTON btnTasks 
     IMAGE-UP FILE "AOA/images/aoaTasks.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Tasks".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "AOA/images/jrxml_icon.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnXLS 
     IMAGE-UP FILE "AOA/images/aoaexcel.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Excel XLS".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnTasks AT ROW 1.1 COL 52 HELP
          "Tasks" WIDGET-ID 42
     btnPrint AT ROW 1 COL 40 HELP
          "Print" WIDGET-ID 40
     btnHTML AT ROW 1.1 COL 33 HELP
          "HTML" WIDGET-ID 38
     btnView AT ROW 1.1 COL 45 HELP
          "Jasper Viewer" WIDGET-ID 32
     btnPDF AT ROW 1.1 COL 28 HELP
          "PDF" WIDGET-ID 36
     btnDOCX AT ROW 1.1 COL 23 HELP
          "Word DOCX" WIDGET-ID 34
     btnSave AT ROW 1.1 COL 1 HELP
          "Save" WIDGET-ID 24
     btnDataPA AT ROW 1.1 COL 7 HELP
          "Browser" WIDGET-ID 28
     btnCSV AT ROW 1.1 COL 13 HELP
          "Excel CSV" WIDGET-ID 30
     btnXLS AT ROW 1.1 COL 18 HELP
          "Excel XLS" WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 .


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
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 1.1
         WIDTH              = 55.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnDOCX IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnHTML IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnPDF IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnPrint IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnTasks IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON btnView IN FRAME F-Main
   1                                                                    */
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

&Scoped-define SELF-NAME btnCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCSV sObject
ON CHOOSE OF btnCSV IN FRAME F-Main /* csv */
DO:
    RUN pPanelSelection ("CSV").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataPA sObject
ON CHOOSE OF btnDataPA IN FRAME F-Main
DO:
    RUN pPanelSelection ("DataPA").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDOCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOCX sObject
ON CHOOSE OF btnDOCX IN FRAME F-Main
DO:
    RUN pPanelSelection ("DOCX").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML sObject
ON CHOOSE OF btnHTML IN FRAME F-Main
DO:
    RUN pPanelSelection ("HTML").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF sObject
ON CHOOSE OF btnPDF IN FRAME F-Main
DO:
    RUN pPanelSelection ("PDF").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint sObject
ON CHOOSE OF btnPrint IN FRAME F-Main
DO:
    RUN pPanelSelection ("Print -d").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON CHOOSE OF btnSave IN FRAME F-Main
DO:
    RUN pPanelSelection ("Save").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON RIGHT-MOUSE-CLICK OF btnSave IN FRAME F-Main
DO:
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    {methods/run_link.i "CONTAINER" "pGenerateInclude"}
&ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTasks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTasks sObject
ON CHOOSE OF btnTasks IN FRAME F-Main
DO:
    RUN pPanelSelection ("Tasks").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView sObject
ON CHOOSE OF btnView IN FRAME F-Main
DO:
    RUN pPanelSelection ("View").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnXLS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnXLS sObject
ON CHOOSE OF btnXLS IN FRAME F-Main
DO:
    RUN pPanelSelection ("XLS").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize sObject 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/run_link.i "CONTAINER" "pGetContainerSize" "(OUTPUT iHeightPixels, OUTPUT iWidthPixels)"}
  RUN pSetWinSize.
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lJasperStarter = INDEX(OS-GETENV("Path"),"jasperstarter") NE 0.
    
  IF lJasperStarter EQ NO THEN
  DO WITH FRAME {&FRAME-NAME}:
      HIDE {&jasperObjects}.
  END. /* do with */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPanelSelection sObject 
PROCEDURE pPanelSelection :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPanelSelection AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE rUserPrintRowID AS ROWID  NO-UNDO.
    DEFINE VARIABLE hAppSrv         AS HANDLE NO-UNDO.
    DEFINE VARIABLE hAppSrvBin      AS HANDLE NO-UNDO.
    
    CASE ipcPanelSelection:
        WHEN "DataPA" THEN DO:
            {methods/run_link.i "CONTAINER" "pSaveUserPrint" "(NO)"}
            {methods/run_link.i "CONTAINER" "pSaveJasperUserPrint" "(NO)"}
            {methods/run_link.i "CONTAINER" "pURL"}
            RETURN.
        END.
        WHEN "Save" THEN DO:
            {methods/run_link.i "CONTAINER" "pSaveUserPrint" "(NO)"}
            {methods/run_link.i "CONTAINER" "pSaveJasperUserPrint" "(NO)"}
            MESSAGE
                "Parameter & Column Values Saved"
            VIEW-AS ALERT-BOX.
            RETURN.
        END.
        WHEN "Tasks" THEN DO:
            RUN AOA/aoaTasks.w PERSISTENT SET hTasks.
            {methods/run_link.i "CONTAINER" "pSethTasks" "(hTasks)"}
            RETURN.
        END.
    END CASE.
    
    IF lJasperStarter THEN DO:
        {methods/run_link.i "CONTAINER" "pSaveUserPrint" "(NO)"}
        {methods/run_link.i "CONTAINER" "pSaveJasperUserPrint" "(NO)"}
        {methods/run_link.i "CONTAINER" "pGetUserPrintRowID" "(OUTPUT rUserPrintRowID)"}
        {methods/run_link.i "CONTAINER" "pGethAppSrv" "(OUTPUT hAppSrv)"}
        {methods/run_link.i "CONTAINER" "pGethAppSrvBin" "(OUTPUT hAppSrvBin)"}
        RUN pJasper (ipcPanelSelection, rUserPrintRowID, hAppSrv, hAppSrvBin).
    END. /* if jasper */
    ELSE
    CASE ipcPanelSelection:
        WHEN "CSV" THEN
            {methods/run_link.i "CONTAINER" "pExcelCSV"}
        WHEN "XLS" THEN
            {methods/run_link.i "CONTAINER" "pExcelXLS"}
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetWinSize sObject 
PROCEDURE pSetWinSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    iWidthPixels = iWidthPixels - 328.
    IF iWidthPixels LT 277 THEN
    iWidthPixels = 277.
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = iWidthPixels.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

