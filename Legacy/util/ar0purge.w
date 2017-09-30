&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\ar0purge.w

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
DEF STREAM out1.
DEF STREAM out2.
DEF STREAM out3.
DEF STREAM out4.
DEF STREAM out5.

DEF VAR cDestroy AS CHAR NO-UNDO.
DEF VAR iPurgeCount AS INT NO-UNDO.
DEF VAR iPurgeCount2 AS INT NO-UNDO.
DEF VAR iPurgeCount3 AS INT NO-UNDO.
DEF VAR iPurgeCount4 AS INT NO-UNDO.
DEF VAR iPurgeCount5 AS INT NO-UNDO.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EDITOR-1 tbPurgeMore btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 tbPurgeMore end_date ~
begin_cust-no end_cust-no 

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
     LABEL "&Purge" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 65 BY 4 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Purge records through (date)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE tbPurgeMore AS LOGICAL INITIAL no 
     LABEL "Purge non-zero transactions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     EDITOR-1 AT ROW 5.29 COL 15 NO-LABEL WIDGET-ID 2 NO-TAB-STOP 
     tbPurgeMore AT ROW 9.81 COL 22 WIDGET-ID 4
     end_date AT ROW 11 COL 51 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_cust-no AT ROW 12.43 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 12.43 COL 59 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     btn-process AT ROW 14.57 COL 21
     btn-cancel AT ROW 14.57 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "and may take hours to complete!" VIEW-AS TEXT
          SIZE 46 BY .95 AT ROW 3.62 COL 21
          FONT 17
     "You MUST perform a database backup before running this" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.24 COL 4
          FONT 17
     "procedure.  Please note this process is VERY time intensive" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 2.43 COL 4 WIDGET-ID 2
          FONT 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 14 FGCOLOR 12 .


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
         TITLE              = "Purge Posted Cash Transactions"
         HEIGHT             = 17.71
         WIDTH              = 90
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN begin_cust-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN end_cust-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_date IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Posted Cash Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Posted Cash Transactions */
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
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Purge */
DO:
    MESSAGE 
        "Are you sure you want to permanently delete the selected records?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lProcess AS LOG.

    IF lProcess 
    AND tbPurgeMore:checked in frame {&frame-name} THEN DO:
        UPDATE 
            cDestroy LABEL "Enter 'DESTROY' to confirm purge"
            WITH FRAME b VIEW-AS DIALOG-BOX THREE-D.
        IF cDestroy NE "DESTROY" THEN DO:
            MESSAGE
                "Purge Cancelled."
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    END.
    ELSE IF NOT lProcess THEN DO:
        MESSAGE
            "Purge Cancelled."
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    
    RUN ipRunPurge.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Purge records through (date) */
DO:
    IF DATE(SELF:SCREEN-VALUE) > DATE(STRING(MONTH(TODAY),"99") + "/" +
                                STRING(DAY(TODAY),"99") + "/" +
                                STRING(YEAR(TODAY) - 7,"9999")) THEN DO:
        MESSAGE
            "It is unusual to purge financial data more" SKIP
            "recent than 7 years.  Are you sure?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE lSure AS LOG.
        IF NOT lSure THEN DO:
            ASSIGN
                SELF:SCREEN-VALUE = STRING(MONTH(TODAY),"99") + "/" +
                                    STRING(DAY(TODAY),"99") + "/" +
                                    STRING(YEAR(TODAY) - 7,"9999").
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbPurgeMore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbPurgeMore C-Win
ON VALUE-CHANGED OF tbPurgeMore IN FRAME FRAME-A /* Purge non-zero transactions? */
DO:
    IF SELF:CHECKED THEN ASSIGN
        end_date:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
        begin_cust-no:SENSITIVE = TRUE
        end_cust-no:SENSITIVE = TRUE.
    ELSE ASSIGN
        end_date:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        begin_cust-no:SENSITIVE = FALSE
        end_cust-no:SENSITIVE = FALSE.
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
    IF access-close THEN DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN.
    END.

    FIND ap-ctrl NO-LOCK WHERE 
        ap-ctrl.company = gcompany 
        NO-ERROR.

  RUN enable_UI.
  
    ASSIGN
        editor-1:SCREEN-VALUE = "This process will purge ALL paid-on-account " +
                                "transactions that have a zero dollar transaction " +
                                "amount.  Optionally, you can choose a range of " +
                                "customers and/or dates to purge additional (non-zero) " +
                                "transactions.  If the cash record is not marked as " +
                                "POSTED it will NOT be purged."
        end_date:SCREEN-VALUE = STRING(MONTH(TODAY),"99") + "/" +
                                STRING(DAY(TODAY),"99") + "/" +
                                STRING(YEAR(TODAY) - 7,"9999").
    APPLY 'entry' to tbPurgeMore.                            
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
  DISPLAY EDITOR-1 tbPurgeMore end_date begin_cust-no end_cust-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE EDITOR-1 tbPurgeMore btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRunPurge C-Win 
PROCEDURE ipRunPurge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT STREAM out1 TO VALUE("c:\tmp\ar-cashl" + 
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d").
    OUTPUT STREAM out2 TO VALUE("c:\tmp\ar-cash" +
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d").
    
    DEF BUFFER b-ar-cashl FOR ar-cashl.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    SESSION:SET-WAIT-STATE ("general").

    DISABLE TRIGGERS FOR LOAD OF ar-cash.
    DISABLE TRIGGERS FOR LOAD OF ar-cashl.

    FOR EACH ar-cashl WHERE 
        ar-cashl.company  EQ cocode AND 
        ar-cashl.posted   EQ YES AND 
        ar-cashl.memo     EQ NO AND 
        ar-cashl.amt-paid EQ 0 AND 
        ar-cashl.amt-disc EQ 0 AND 
        (ar-cashl.on-account EQ YES OR ar-cashl.inv-no EQ 0)
        USE-INDEX c-no:
    
        /* If no more ar-cashl records for this ar-cash, delete the ar-cash as well */
        IF NOT CAN-FIND(FIRST b-ar-cashl WHERE 
                            b-ar-cashl.c-no EQ ar-cashl.c-no AND
                            ROWID(b-ar-cashl) NE ROWID(ar-cashl)) THEN DO:
            FIND ar-cash OF ar-cashl EXCLUSIVE.
            EXPORT STREAM out2 ar-cash.
            DELETE ar-cash.
            ASSIGN
                iPurgeCount2 = iPurgeCount2 + 1.
        END.    

        EXPORT STREAM out1 ar-cashl.
        DELETE ar-cashl.
        ASSIGN
            iPurgeCount = iPurgeCount + 1.
    END.

    IF tbPurgeMore:CHECKED THEN DO:
        FOR EACH ar-cash EXCLUSIVE WHERE 
            ar-cash.company     EQ cocode AND 
            ar-cash.cust-no     GE begin_cust-no AND 
            ar-cash.cust-no     LE end_cust-no AND 
            ar-cash.check-date  LE end_date AND
            ar-cash.posted      EQ YES
            USE-INDEX ar-cash:

            FOR EACH ar-cashl EXCLUSIVE WHERE
                ar-cashl.c-no = ar-cash.c-no:
                EXPORT STREAM out1 ar-cashl.
                DELETE ar-cashl.
                ASSIGN
                    iPurgeCount = iPurgeCount + 1.
            END.

            EXPORT STREAM out2 ar-cash.
            DELETE ar-cash.
            ASSIGN
                iPurgeCount2 = iPurgeCount2 + 1.
        END.
    END.

    SESSION:SET-WAIT-STATE ("").

    OUTPUT STREAM out1 CLOSE.
    OUTPUT STREAM out2 CLOSE.

    MESSAGE  
        "Purge process completed." SKIP
        STRING(iPurgeCount2) + " ar-cash records were deleted." SKIP
        STRING(iPurgeCount) + " ar-cashl records were deleted." SKIP
        "Backup files were placed in the C:\tmp directory" SKIP
        "for retrieval if necessary."
        VIEW-AS ALERT-BOX.
        
    APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

