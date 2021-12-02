&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: DEVA$!

  Created: 

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
DEFINE TEMP-TABLE ttVariable NO-UNDO
    FIELD cSegment  AS CHARACTER
    FIELD cVariable AS CHARACTER
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cbAPI btSelect fiSourcePath fiExportPath ~
btExport 
&Scoped-Define DISPLAYED-OBJECTS cbAPI fiSourcePath fiExportPath 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExport 
     LABEL "Export" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btSelect 
     LABEL "Browse" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbAPI AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select API" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE fiExportPath AS CHARACTER FORMAT "X(256)":U 
     LABEL "ExportFilePath" 
     VIEW-AS FILL-IN 
     SIZE 84 BY 1 NO-UNDO.

DEFINE VARIABLE fiSourcePath AS CHARACTER FORMAT "X(256)":U INITIAL "../../Repositories/Advantzware/Source" 
     LABEL "Enter Source Code Path" 
     VIEW-AS FILL-IN 
     SIZE 84 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbAPI AT ROW 1.48 COL 31 COLON-ALIGNED WIDGET-ID 2
     btSelect AT ROW 2.67 COL 120 WIDGET-ID 8
     fiSourcePath AT ROW 2.71 COL 31 COLON-ALIGNED WIDGET-ID 4
     fiExportPath AT ROW 4 COL 31 COLON-ALIGNED WIDGET-ID 10
     btExport AT ROW 5.24 COL 32.8 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 135.8 BY 5.62
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


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
         TITLE              = "API Variable Export"
         HEIGHT             = 5.62
         WIDTH              = 135.8
         MAX-HEIGHT         = 25.48
         MAX-WIDTH          = 157.6
         VIRTUAL-HEIGHT     = 25.48
         VIRTUAL-WIDTH      = 157.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Variable Export */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Variable Export */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExport C-Win
ON CHOOSE OF btExport IN FRAME DEFAULT-FRAME /* Export */
DO:
    DEFINE VARIABLE cLine     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSegment  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVariable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSource   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttVariable. 

    RUN FileSys_ValidateFile (
        INPUT  fiSourcePath:SCREEN-VALUE + "/api/" + cbAPI:SCREEN-VALUE + ".p",
        OUTPUT lValid,
        OUTPUT cMessage
        ).
    
    IF NOT lValid THEN DO:
        MESSAGE cMessage
        VIEW-AS ALERT-BOX ERROR.
    END.
              
    INPUT FROM VALUE(fiSourcePath:SCREEN-VALUE + "/api/" + cbAPI:SCREEN-VALUE + ".p").
    REPEAT:
        IMPORT UNFORMATTED cLine.
        IF INDEX(cLine, " updateRequestData") GT 0 THEN 
        DO:
            ASSIGN
                cSegment  = ENTRY(2, cLine, "(")
                cSegment  = ENTRY(1, cSegment, ",")
                cSegment  = ENTRY(2, cSegment, " ")
                cSegment  = TRIM(cSegment)
                cSegment  = IF cSegment EQ "ioplcRequestData" THEN
                                cbAPI:SCREEN-VALUE
                            ELSE
                                cSegment
                cSegment  = REPLACE(cSegment, "lc", "")
                cSegment  = REPLACE(cSegment, "data", "")                                  
                cVariable = ENTRY(2, cLine, "(")
                cVariable = ENTRY(2, cVariable, ",")
                cVariable = TRIM(TRIM(cVariable),'"')
                NO-ERROR
                .
            
            IF NOT CAN-FIND(FIRST ttVariable 
                            WHERE ttVariable.cSegment EQ cSegment
                              AND ttVariable.cVariable EQ cVariable) THEN DO:
                CREATE ttVariable.
                ASSIGN
                    ttVariable.cSegment  = cSegment
                    ttVariable.cVariable = cVariable
                    .
            END.
        END.
    END.
    INPUT CLOSE.

    OUTPUT TO VALUE(fiExportPath:SCREEN-VALUE).
    FOR EACH ttVariable:
        EXPORT DELIMITER "," ttVariable.
    END.
    OUTPUT CLOSE. 
    
    RUN OS_RunFile (
        INPUT  fiExportPath:SCREEN-VALUE,
        OUTPUT lValid,
        OUTPUT cMessage
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelect C-Win
ON CHOOSE OF btSelect IN FRAME DEFAULT-FRAME /* Browse */
DO:
    DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOKpressed AS LOGICAL   NO-UNDO INITIAL TRUE.
 
    REPEAT:
        SYSTEM-DIALOG GET-DIR cFileName
        TITLE   "Select source path directory ..."               
        UPDATE lOKpressed.

        IF lOKpressed = TRUE THEN
           fiSourcePath:SCREEN-VALUE = cFileName.
           LEAVE.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAPI C-Win
ON VALUE-CHANGED OF cbAPI IN FRAME DEFAULT-FRAME /* Select API */
DO:
    fiExportPath:SCREEN-VALUE = "C:\temp\" + cbAPI:SCREEN-VALUE + ".txt".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
    RUN pInit.
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
  DISPLAY cbAPI fiSourcePath fiExportPath 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cbAPI btSelect fiSourcePath fiExportPath btExport 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAPIList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FOR EACH apiOutbound NO-LOCK
        WHERE apiOutbound.company = "":
        IF LOOKUP(apiOutbound.apiID, cAPIList) EQ 0 THEN            
            cAPIList = cAPIList + "," + apioutbound.apiID.  
    END.
    
    cbAPI:LIST-ITEMS = cAPIList.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

