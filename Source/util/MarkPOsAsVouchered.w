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

  Author: 

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
DEFINE VARIABLE cFileToProcess AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btStartProcess RECT-15 btExit fiFileName ~
cbCompany 
&Scoped-Define DISPLAYED-OBJECTS fiFileName cbCompany tSelect 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 7.2 BY 1.71.

DEFINE BUTTON btStartProcess  NO-FOCUS FLAT-BUTTON
     LABEL "Start Process" 
     SIZE 16 BY 1.14.

DEFINE VARIABLE cbCompany AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64.8 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE tSelect AS CHARACTER FORMAT "X(256)":U INITIAL "Press F1 to Select CSV file to Process" 
      VIEW-AS TEXT 
     SIZE 38 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 96 BY 1.91
     BGCOLOR 21 FGCOLOR 21 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btStartProcess AT ROW 6.95 COL 78 WIDGET-ID 12
     btExit AT ROW 1.14 COL 88.8 WIDGET-ID 24
     fiFileName AT ROW 4.1 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     cbCompany AT ROW 5.52 COL 15 COLON-ALIGNED WIDGET-ID 28
     tSelect AT ROW 3.14 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 26 NO-TAB-STOP 
     RECT-15 AT ROW 1 COL 1 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 7.52 WIDGET-ID 100.


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
         TITLE              = "Mark POs as Vouchered"
         HEIGHT             = 7.52
         WIDTH              = 96
         MAX-HEIGHT         = 16.62
         MAX-WIDTH          = 140.8
         VIRTUAL-HEIGHT     = 16.62
         VIRTUAL-WIDTH      = 140.8
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
ASSIGN 
       fiFileName:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN tSelect IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tSelect:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mark POs as Vouchered */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mark POs as Vouchered */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStartProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStartProcess C-Win
ON CHOOSE OF btStartProcess IN FRAME DEFAULT-FRAME /* Start Process */
DO:
    RUN pRunProcess.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFileName C-Win
ON HELP OF fiFileName IN FRAME DEFAULT-FRAME
DO:
    DEF VAR OKPressed AS LOG NO-UNDO INITIAL TRUE.
    SYSTEM-DIALOG GET-FILE cFiletoProcess
        TITLE "Select File to Process..."
        FILTERS "CSV Files (*.csv)" "*.csv"
        MUST-EXIST 
        USE-FILENAME 
        UPDATE OKpressed.

    IF OKPressed THEN DO:
        IF SEARCH(cFileToProcess) NE ? THEN ASSIGN 
            fiFileName:SCREEN-VALUE = cFileToProcess.
        ELSE DO:
            MESSAGE 
                "Unable to locate selected file. Please try again."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
       
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
  
  ASSIGN 
    cbCompany:LIST-ITEMS = "".
  FOR EACH company NO-LOCK:
      cbCompany:ADD-LAST(company.company).
  END.
  ASSIGN 
    cbCompany:SCREEN-VALUE = ENTRY(1,cbCompany:LIST-ITEMS).
    
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
  DISPLAY fiFileName cbCompany tSelect 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btStartProcess RECT-15 btExit fiFileName cbCompany 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cRaw AS CHAR NO-UNDO.
DEF VAR ict AS INT NO-UNDO.
DEF VAR jct AS INT NO-UNDO.
DEF VAR iColumn AS INT NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF po-ord.
DISABLE TRIGGERS FOR LOAD OF po-ordl.

INPUT FROM VALUE(cFileToProcess).

IMPORT UNFORMATTED cRaw.
DO ict = 1 TO NUM-ENTRIES(cRaw):
    IF ENTRY(iCt,cRaw) EQ "PO#" 
    OR ENTRY(iCt,cRaw) EQ "PO #" THEN ASSIGN 
        iColumn = iCt.
END.

ASSIGN 
    iCt = 0
    jct = 0.
    
REPEAT:
    IMPORT UNFORMATTED cRaw.
    FIND FIRST po-ord WHERE 
    po-ord.company EQ cbCompany:SCREEN-VALUE IN FRAME {&frame-name} AND
    po-ord.po-no = integer(ENTRY(iColumn,cRaw,",")) EXCLUSIVE NO-ERROR.
    IF AVAIL po-ord THEN DO:
        ASSIGN 
            po-ord.excludefromvoucher = TRUE
            ict = ict + 1.
        FOR EACH po-ordl WHERE 
            po-ordl.company EQ po-ord.company AND 
            po-ordl.po-no EQ po-ord.po-no:
            ASSIGN 
                po-ordl.excludeFromVoucher = TRUE.
        END.
    END.
    ASSIGN jct = jct + 1.
END.
INPUT CLOSE.
MESSAGE  
    "Process complete." SKIP 
    "Processed " + STRING(ict) + " of " + STRING(jCt) + " records."
    VIEW-AS ALERT-BOX.
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

