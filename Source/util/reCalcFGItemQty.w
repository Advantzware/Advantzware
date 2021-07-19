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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER INITIAL "c:~\tmp~\RecalculateFGItemQty.csv"  NO-UNDO.

DEFINE TEMP-TABLE ttReCalculateFgItem 
       FIELD itemId AS CHARACTER  LABEL "FG Item" 
       FIELD itemLoc AS CHARACTER  LABEL "Location"       
       FIELD calcOhHand AS INTEGER  LABEL "Calculated On Hand"
       FIELD calcOnOrder AS INTEGER  LABEL "Calculated On Order"
       FIELD calcBackOrder AS INTEGER LABEL "Calculated Backordered"
       FIELD calcAvailable AS INTEGER LABEL "Calculated Available"
       FIELD calcAllocated AS INTEGER LABEL "Calculated Allocated"
       FIELD actualOhHand AS INTEGER  LABEL "Actual On Hand"
       FIELD actualOnOrder AS INTEGER LABEL "Actual On Order"
       FIELD actualBackOrder AS INTEGER LABEL "Actual Backordered"
       FIELD actualAvailable AS INTEGER LABEL "Actual Available"
       FIELD actualAllocated AS INTEGER LABEL "Actual Allocated"
       FIELD diffOhHand AS INTEGER  LABEL "Variance On Hand"
       FIELD diffOnOrder AS INTEGER LABEL "Variance On Order"
       FIELD diffBackOrder AS INTEGER LABEL "Variance Backordered"
       FIELD diffAvailable AS INTEGER LABEL "Variance Available"
       FIELD diffAllocated AS INTEGER LABEL "Variance Allocated" .         

{sys/inc/var.i new shared}

assign   
 locode = "Main".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 enter_company begin_i-no end_i-no ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS enter_company begin_i-no end_i-no 

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

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning FG Item #" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending FG Item #" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE enter_company AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     enter_company AT ROW 2.86 COL 23.8 COLON-ALIGNED HELP
          "Enter Company"
     begin_i-no AT ROW 4.71 COL 23.8 COLON-ALIGNED HELP
          "Enter FG Item"
     end_i-no AT ROW 4.71 COL 69 COLON-ALIGNED HELP
          "Enter FG Item" WIDGET-ID 2
     btn-process AT ROW 9.62 COL 26.8
     btn-cancel AT ROW 9.62 COL 58.8
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.71 COL 5
     RECT-17 AT ROW 1.19 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.4 BY 11.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Recalculate FGItem Qty"
         HEIGHT             = 11.52
         WIDTH              = 99
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 99
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 99
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
ON END-ERROR OF C-Win /* Recalculate FGItem Qty */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Recalculate FGItem Qty */
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
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR ll-process AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  
  FIND FIRST company NO-LOCK
       WHERE  company.company EQ enter_company:SCREEN-VALUE NO-ERROR .
  IF NOT AVAIL company THEN
  DO:
      MESSAGE "Please enter valid company" VIEW-AS ALERT-BOX ERROR . 
      RETURN NO-APPLY .
  END.    
 
    MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " within the " +
            "selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-process.

    IF ll-process THEN RUN run-process.
  END.
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
  /* check security */
  
  RUN enable_UI.
 
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
  DISPLAY enter_company begin_i-no end_i-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 enter_company begin_i-no end_i-no btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunReport C-Win 
PROCEDURE pRunReport PRIVATE :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE hdOutput    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTempTable AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
  
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    ASSIGN 
        hdTempTable = TEMP-TABLE ttReCalculateFgItem:HANDLE.
    
    RUN Output_TempTableToCSV IN hdOutput (hdTempTable, ipcFile, YES, INPUT TRUE /* Auto increment File name */, OUTPUT lSuccess, OUTPUT cMessage).
  
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* -------------------------------------------------                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
DEFINE BUFFER bf-itemfg FOR itemfg.

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

EMPTY TEMP-TABLE ttReCalculateFgItem.
cocode = enter_company.

FOR EACH bf-itemfg NO-LOCK
    WHERE bf-itemfg.company EQ cocode
    AND bf-itemfg.i-no GE begin_i-no
    AND bf-itemfg.i-no LE end_i-no:
    
    FOR EACH itemfg-loc NO-LOCK
        WHERE itemfg-loc.company EQ bf-itemfg.company
          AND itemfg-loc.i-no    EQ bf-itemfg.i-no,
        FIRST loc NO-LOCK
        WHERE loc.company EQ itemfg-loc.company
          AND loc.loc     EQ itemfg-loc.loc
        :
        CREATE ttReCalculateFgItem.
        ASSIGN
        ttReCalculateFgItem.itemId          = bf-itemfg.i-no
        ttReCalculateFgItem.itemLoc         = itemfg-loc.loc
        ttReCalculateFgItem.actualOhHand    = itemfg-loc.q-onh
        ttReCalculateFgItem.actualOnOrder   = itemfg-loc.q-ono
        ttReCalculateFgItem.actualBackOrder = itemfg-loc.q-back
        ttReCalculateFgItem.actualAvailable = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
        ttReCalculateFgItem.actualAllocated = itemfg-loc.q-alloc .
   END.  
   
   run fg/fg-reset.p (recid(bf-itemfg)).
   
   FOR EACH itemfg-loc NO-LOCK
        WHERE itemfg-loc.company EQ bf-itemfg.company
          AND itemfg-loc.i-no    EQ bf-itemfg.i-no,
        FIRST loc NO-LOCK
        WHERE loc.company EQ itemfg-loc.company
          AND loc.loc     EQ itemfg-loc.loc
        :
        FIND FIRST ttReCalculateFgItem NO-LOCK
             WHERE ttReCalculateFgItem.itemId EQ bf-itemfg.i-no
             AND ttReCalculateFgItem.itemLoc EQ  itemfg-loc.loc NO-ERROR.
        IF AVAIL ttReCalculateFgItem THEN
        DO:
            ASSIGN             
            ttReCalculateFgItem.calcOhHand    = itemfg-loc.q-onh
            ttReCalculateFgItem.calcOnOrder   = itemfg-loc.q-ono
            ttReCalculateFgItem.calcBackOrder = itemfg-loc.q-back
            ttReCalculateFgItem.calcAvailable = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
            ttReCalculateFgItem.calcAllocated = itemfg-loc.q-alloc .
            
        END.        
   END.       
END.

FOR EACH ttReCalculateFgItem  :
  ASSIGN             
    ttReCalculateFgItem.diffOhHand    = ttReCalculateFgItem.calcOhHand - ttReCalculateFgItem.actualOhHand 
    ttReCalculateFgItem.diffOnOrder   = ttReCalculateFgItem.calcOnOrder - ttReCalculateFgItem.actualOnOrder 
    ttReCalculateFgItem.diffBackOrder = ttReCalculateFgItem.calcBackOrder - ttReCalculateFgItem.actualBackOrder
    ttReCalculateFgItem.diffAvailable = ttReCalculateFgItem.calcAvailable - ttReCalculateFgItem.actualAvailable
    ttReCalculateFgItem.diffAllocated = ttReCalculateFgItem.calcAllocated - ttReCalculateFgItem.actualAllocated.       
END.      

message trim(c-win:title) + " Process Is Completed." view-as alert-box.

RUN pRunReport(cFileName).

STATUS INPUT "Opening file for review...".
            
OS-COMMAND SILENT VALUE ("START " + cFileName ).

STATUS INPUT "".

/* end ---------------------------------- copr. 2004  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

