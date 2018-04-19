&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.
 

DEFINE VARIABLE lFoundIni AS LOGICAL NO-UNDO.
DEFINE VARIABLE cIniFileLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCtr        AS INTEGER NO-UNDO.
DEFINE VARIABLE cIniLoc     AS CHARACTER NO-UNDO.
       
DEFINE VARIABLE cReportDbName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReportDbPort AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHostName     AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fCompany tCompany fcust tcust fitem ~
titem fpcat tpcat fdate tdate tbOverwriteRecords btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fCompany tCompany fcust tcust fitem titem ~
fpcat tpcat fdate tdate tbOverwriteRecords 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fCompany AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Company" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE fcust AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE fdate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/80 
     LABEL "From Inv Date" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE fitem AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Item" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fpcat AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Category" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE tCompany AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZ" 
     LABEL "To Company" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE tcust AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzz" 
     LABEL "To Customer" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE tdate AS CHARACTER FORMAT "X(256)":U INITIAL "12/31/2050" 
     LABEL "To Inv Date" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE titem AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZ" 
     LABEL "To Item" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tpcat AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZZ" 
     LABEL "To Category" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.76.

DEFINE VARIABLE tbOverwriteRecords AS LOGICAL INITIAL no 
     LABEL "Overwrite Existing Records" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fCompany AT ROW 2.91 COL 18 COLON-ALIGNED WIDGET-ID 20
     tCompany AT ROW 2.91 COL 64 COLON-ALIGNED WIDGET-ID 22
     fcust AT ROW 3.86 COL 18 COLON-ALIGNED WIDGET-ID 2
     tcust AT ROW 3.86 COL 64 COLON-ALIGNED WIDGET-ID 4
     fitem AT ROW 4.81 COL 18 COLON-ALIGNED WIDGET-ID 6
     titem AT ROW 4.81 COL 64 COLON-ALIGNED WIDGET-ID 8
     fpcat AT ROW 5.76 COL 18 COLON-ALIGNED WIDGET-ID 10
     tpcat AT ROW 5.76 COL 64 COLON-ALIGNED WIDGET-ID 12
     fdate AT ROW 6.71 COL 18 COLON-ALIGNED WIDGET-ID 14
     tdate AT ROW 6.71 COL 64 COLON-ALIGNED WIDGET-ID 16
     tbOverwriteRecords AT ROW 7.71 COL 20 WIDGET-ID 24
     btn-process AT ROW 11.48 COL 19
     btn-cancel AT ROW 11.48 COL 55
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
          FONT 4
     RECT-17 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.4 BY 12.62
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Export Invoice Data"
         HEIGHT             = 12.95
         WIDTH              = 95.6
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Export Invoice Data */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Export Invoice Data */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:


    ASSIGN {&DISPLAYED-OBJECTS}.
  END.



  RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  RUN enable_UI.



  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fCompany tCompany fcust tcust fitem titem fpcat tpcat fdate tdate 
          tbOverwriteRecords 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fCompany tCompany fcust tcust fitem titem fpcat tpcat fdate 
         tdate tbOverwriteRecords btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPortNumber C-Win 
PROCEDURE getPortNumber :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opPort   AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opDbName AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opHost   AS CHAR NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetIniFile C-Win 
PROCEDURE ipGetIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        lFoundIni = FALSE
        cIniFileLoc = "admin\advantzware.ini".
        
    IF SEARCH("N:\admin\advantzware.ini") NE ? THEN 
      ASSIGN cIniLoc = "N:\admin\advantzware.ini"
             cIniFileLoc = cIniLoc
             lFoundIni = true.
    ELSE DO:        
        IF SEARCH(cIniFileLoc) = ? THEN DO WHILE SEARCH(cIniFileLoc) = ?:
            ASSIGN 
                iCtr = iCtr + 1
                cIniFileLoc = "..\" + cIniFileLoc.
            IF iCtr > 4 THEN LEAVE.
        END.
        IF NOT SEARCH(cIniFileLoc) = ? THEN ASSIGN
            file-info:file-name = SEARCH(cIniFileLoc)
            cIniLoc = file-info:full-pathname
            lFoundIni = TRUE.
    END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadIniFile C-Win 
PROCEDURE ipReadIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lCorrupt AS LOGICAL NO-UNDO.
DEFINE VARIABLE iCtr     AS INTEGER NO-UNDO.
DEFINE VARIABLE cRaw     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIniLine AS CHARACTER NO-UNDO.
    ASSIGN 
        lCorrupt = FALSE
        iCtr = 1.
        
    message "reading ini" cIniLoc view-as alert-box.
    INPUT FROM VALUE(SEARCH(cIniLoc)).
    REPEAT:
        IMPORT UNFORMATTED cIniLine.
        
        ASSIGN iCtr = iCtr + 1
               cRaw = cIniLine
               .
        IF INDEX(cIniLine,"=") NE 0 THEN ASSIGN
            cName  = ENTRY(1,cIniLine,"=")
            cValue = ENTRY(2,cIniLine,"=").
            if cName begins "rpt" then 
             message cname cValue view-as alert-box.
        IF cName EQ "RptDbName" THEN 
          cReportDbName = cValue.
        IF cName EQ "RptDbPort" THEN 
          cReportDbPort = cValue.
        IF cName EQ "hostname" THEN 
          cHostName = cValue.
          
    END.
    INPUT CLOSE.

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR lv-rec_key LIKE itemfg.rec_key NO-UNDO.
  DEFINE VARIABLE connectStatement AS CHARACTER NO-UNDO.
  
  RUN ipGetiniFile.
  IF lFoundIni THEN 
  RUN ipReadIniFile.
  IF cReportDbPort ne "" THEN
  connectStatement = "-db " + cReportDbName + 
                     " -H " + chostName +
                     " -S " + cReportDbPort 
                     .
  ELSE 
    MESSAGE "Port for database not specified" skip
      "Database: " cReportDbName skip
      "Host: " cHostname skip
      "Port: " cReportDbPort SKIP
      "Ini Found?" lFoundIni skip
      VIEW-AS ALERT-BOX.
                           
  IF connectStatement NE "" and NOT CONNECTED(cReportDbName) THEN
    CONNECT VALUE(connectStatement).                           
  IF CONNECTED(cReportDbName) THEN DO:
  
    SESSION:SET-WAIT-STATE("general").
  
    run util/createTempInv.p
      (input tbOverWriteRecords,
       input fCompany,
       input tCompany,
       input fCust,
       input tCust,
       input fItem,
       input tItem,
       input fpcat,
       input tpcat,
       input fDate,
       input tDate).
  
    SESSION:SET-WAIT-STATE("").
    IF CONNECTED(cReportDbName) THEN
      disconnect value(cReportDbName).
    
    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
    
  END.
  ELSE MESSAGE "Could not run extract - could not connect database" 
       VIEW-AS ALERT-BOX.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

