&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
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
     
USING system.SharedConfig.

CREATE WIDGET-POOL.     

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{custom/globdefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

cocode = g_company.

DEFINE TEMP-TABLE tt-glhist NO-UNDO LIKE glhist
   FIELD glhist-flag AS LOG
   FIELD acct-dscr LIKE account.dscr
   FIELD tt-rowid AS ROWID
   INDEX tt-glhist glhist-flag DESC actnum ASC.
   
DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-glhist

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-glhist.actnum tt-glhist.acct-dscr tt-glhist.jrnl tt-glhist.tr-dscr tt-glhist.tr-date tt-glhist.tr-amt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-glhist.tr-amt   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-glhist
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-glhist
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-glhist
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-glhist.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-glhist
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-glhist


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_run-no BUTTON-1 btn_update btn_add BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS begin_run-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Populate Browser" 
     SIZE 22 BY 1.14.

DEFINE BUTTON btn_update 
     LABEL "Update" 
     SIZE 15 BY 1.14.
     
DEFINE BUTTON btn_add 
     LABEL "Add" 
     SIZE 15 BY 1.14.
     
DEFINE VARIABLE begin_run-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Run#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-glhist SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-glhist.actnum FORMAT "X(14)" COLUMN-LABEL "Account Number"
 tt-glhist.acct-dscr FORMAT "X(28)" COLUMN-LABEL "Account Description"
 tt-glhist.jrnl COLUMN-LABEL "Journal"
 tt-glhist.tr-dscr COLUMN-LABEL "Reference" WIDTH 50
 tt-glhist.tr-date COLUMN-LABEL "Date"
 tt-glhist.tr-amt COLUMN-LABEL "Balance" FORMAT "->,>>>,>>9.99"

 ENABLE tt-glhist.tr-amt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 145 BY 12.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_run-no AT ROW 1.48 COL 8 COLON-ALIGNED HELP
          "Enter Beginning Run Number"
     BUTTON-1 AT ROW 1.48 COL 29
     btn_update AT ROW 1.48 COL 59
     btn_add AT ROW 1.48 COL 74     
     BROWSE-1 AT ROW 3.14 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 15.38.


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
         TITLE              = "Fix G/L History"
         HEIGHT             = 15.38
         WIDTH              = 150
         MAX-HEIGHT         = 15.38
         MAX-WIDTH          = 150
         VIRTUAL-HEIGHT     = 15.38
         VIRTUAL-WIDTH      = 150
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
/* BROWSE-TAB BROWSE-1 BUTTON-1 FRAME-A */
ASSIGN 
       begin_run-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-glhist.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix G/L History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix G/L History */
DO:
  /* This event will close the window and terminate the procedure.  */
  DEF VAR confirm AS LOG NO-UNDO.

  MESSAGE "Do you wish to update G/L Account Balances?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE confirm.

  IF confirm THEN
     RUN util\fxacctg3.p.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_run-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_run-no C-Win
ON HELP OF begin_run-no IN FRAME FRAME-A /* Run# */
DO:
  DEF VAR lv AS CHAR NO-UNDO.
  lv = {&self-name}:SCREEN-VALUE.
  RUN run-no-help (INPUT-OUTPUT lv).
  {&self-name}:SCREEN-VALUE = lv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_run-no C-Win
ON LEAVE OF begin_run-no IN FRAME FRAME-A /* Run# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_update C-Win
ON CHOOSE OF btn_update IN FRAME FRAME-A /* Update*/
DO:
   DEFINE VARIABLE riRowid AS ROWID NO-UNDO.
   DEFINE VARIABLE cOldAccount AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dOldAmt AS DECIMAL NO-UNDO.
   DEFINE BUFFER bf-glhist FOR glhist.
   DO WITH FRAME {&FRAME-NAME}:

       IF AVAIL tt-glhist THEN
       DO:
         cOldAccount = tt-glhist.actnum .
         dOldAmt = tt-glhist.tr-amt.
         
         RUN gl/d-glinq.w (INPUT cocode,INPUT tt-glhist.tt-rowid,INPUT "update",OUTPUT riRowid) .
         
         FIND FIRST bf-glhist NO-LOCK
              WHERE rowid(bf-glhist) EQ riRowid NO-ERROR.
         IF AVAIL bf-glhist  THEN
         DO:
            IF bf-glhist.actnum NE cOldAccount THEN
            DO:
                FIND FIRST account EXCLUSIVE-LOCK 
                     WHERE account.company EQ  cocode
                     AND account.actnum EQ cOldAccount NO-ERROR.
                
                IF avail account THEN
                 account.cyr[bf-glhist.period] = account.cyr[bf-glhist.period] - dOldAmt .                    
                 
                 FIND FIRST account EXCLUSIVE-LOCK 
                     WHERE account.company EQ  cocode
                     AND account.actnum EQ bf-glhist.actnum NO-ERROR.
                 IF avail account THEN
                 account.cyr[bf-glhist.period] = account.cyr[bf-glhist.period] + bf-glhist.tr-amt .
            END.
            ELSE IF bf-glhist.tr-amt NE dOldAmt THEN
            DO:
                 FIND FIRST account EXCLUSIVE-LOCK 
                     WHERE account.company EQ  cocode
                     AND account.actnum EQ bf-glhist.actnum NO-ERROR.
                 IF avail account THEN
                 account.cyr[bf-glhist.period] = account.cyr[bf-glhist.period] + (bf-glhist.tr-amt - dOldAmt) .                  
            END.
             
         END.
         RELEASE account.
         PAUSE 0.         
         APPLY "choose" TO BUTTON-1.
       END.
      
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME   

&Scoped-define SELF-NAME btn_add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_add C-Win
ON CHOOSE OF btn_add IN FRAME FRAME-A /* Add*/
DO:
   DEFINE VARIABLE riRowid AS ROWID NO-UNDO.
   DEFINE VARIABLE riNewRowid AS ROWID NO-UNDO.
   DEFINE BUFFER bf-glhist FOR glhist .
   DO WITH FRAME {&FRAME-NAME}:

    IF AVAIL tt-glhist THEN
    DO:
        CREATE glhist .
        BUFFER-COPY tt-glhist EXCEPT rec_key  TO glhist.
        riNewRowid = ROWID(glhist).
        RELEASE glhist.
        
        RUN gl/d-glinq.w (INPUT cocode,INPUT riNewRowid,INPUT "update",OUTPUT riRowid) .
         
         FIND FIRST bf-glhist NO-LOCK
              WHERE rowid(bf-glhist) EQ riRowid NO-ERROR.
         IF AVAIL bf-glhist THEN
         DO:            
           FIND FIRST account EXCLUSIVE-LOCK 
                     WHERE account.company EQ  cocode
                     AND account.actnum EQ bf-glhist.actnum NO-ERROR.
                 IF avail account THEN
                 account.cyr[bf-glhist.period] = account.cyr[bf-glhist.period] + bf-glhist.tr-amt  .              
         END.
         IF NOT AVAIL bf-glhist THEN
         DO:
             FIND FIRST bf-glhist EXCLUSIVE-LOCK
                  WHERE rowid(bf-glhist) EQ riNewRowid NO-ERROR.
             IF avail bf-glhist THEN DELETE bf-glhist.             
         END.
         RELEASE account.
         RELEASE bf-glhist.
         PAUSE 0.         
         APPLY "choose" TO BUTTON-1.
    END.
      
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME FRAME-A /* Populate Browser */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN begin_run-no.

      EMPTY TEMP-TABLE tt-glhist.

      FOR EACH glhist WHERE
          glhist.company EQ cocode AND
          glhist.tr-num   EQ begin_run-no
          NO-LOCK,
          FIRST account fields(dscr) WHERE
                account.company EQ cocode AND
                account.actnum  EQ glhist.actnum
                NO-LOCK:

          CREATE tt-glhist.
          BUFFER-COPY glhist TO tt-glhist
             ASSIGN tt-glhist.glhist-flag = YES
                    tt-glhist.acct-dscr = account.dscr
                    tt-glhist.tt-rowid = ROWID(glhist).
          RELEASE tt-glhist.
      END.      

      OPEN QUERY browse-1 FOR EACH tt-glhist.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
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

SESSION:DATA-ENTRY-RETURN = YES.

ON 'leave':U OF tt-glhist.tr-amt DO:
  IF LASTKEY NE -1 THEN DO:
    IF AVAIL tt-glhist AND
       DEC(tt-glhist.tr-amt:SCREEN-VALUE IN BROWSE {&browse-name}) NE
       tt-glhist.tr-amt THEN DO:
        
       IF tt-glhist.glhist-flag THEN
       DO:
          FIND FIRST glhist WHERE
               ROWID(glhist) EQ tt-glhist.tt-rowid
               EXCLUSIVE-LOCK.

          glhist.tr-amt = DEC(tt-glhist.tr-amt:SCREEN-VALUE IN BROWSE {&browse-name}).
               
          FIND CURRENT glhist NO-LOCK.
       END.
       
    END.
    
  END.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:

DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  
  ASSIGN 
        scInstance   = SharedConfig:instance
        begin_run-no =  INTEGER(scInstance:GetValue("TransTrNumber")) NO-ERROR. 
        begin_run-no:SCREEN-VALUE =  STRING(scInstance:GetValue("TransTrNumber")) NO-ERROR.
        IF begin_run-no NE 0 THEN
        DO:
             APPLY "choose" TO BUTTON-1.
        END.          

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
  DISPLAY begin_run-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_run-no BUTTON-1 btn_update btn_add BROWSE-1 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

