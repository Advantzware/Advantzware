&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
DEF BUFFER b-ar-cashl FOR ar-cashl.

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
DEF VAR invBalAmt AS DEC NO-UNDO.
DEF VAR totWriteOff AS DEC NO-UNDO.

ASSIGN
    totWriteOff = 0.
    
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
&Scoped-Define ENABLED-OBJECTS EDITOR-1 tbNonZero end_date begin_cust-no ~
end_cust-no begin_inv end_inv btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 tbNonZero end_date begin_cust-no ~
end_cust-no begin_inv end_inv 

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

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "From Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Purge records through (date)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "To Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE tbNonZero AS LOGICAL INITIAL no 
     LABEL "Purge Invoices with non-zero balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     EDITOR-1 AT ROW 5.29 COL 15 NO-LABEL WIDGET-ID 10 NO-TAB-STOP 
     tbNonZero AT ROW 9.81 COL 21
     end_date AT ROW 11 COL 49 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_cust-no AT ROW 12.67 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 12.67 COL 54 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_inv AT ROW 14.1 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 14.1 COL 54 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     btn-process AT ROW 15.76 COL 21 WIDGET-ID 2
     btn-cancel AT ROW 15.76 COL 56
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.91.

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
         BGCOLOR 14 FGCOLOR 12  WIDGET-ID 100.


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
         TITLE              = "Purge AR Invoices"
         HEIGHT             = 17.91
         WIDTH              = 89.6
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
ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge AR Invoices */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge AR Invoices */
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
    AND tbNonZero:checked in frame {&frame-name} THEN DO:
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
        RETURN .
    END.
    
    FIND ar-ctrl NO-LOCK WHERE 
        ar-ctrl.company = gcompany 
        NO-ERROR.
  
  RUN enable_UI.

    ASSIGN
        editor-1:SCREEN-VALUE = "This process will purge ALL posted AR invoices " +
                                "and their associated details based on the criteria " +
                                "you select below. Purged records include: invoices, " +
                                "invoice lines, cash payments against invoices, and " +
                                "all associated cash payment details. Invoices not " +
                                "marked as POSTED will NOT be purged."
        end_date:SCREEN-VALUE = STRING(MONTH(TODAY),"99") + "/" +
                                STRING(DAY(TODAY),"99") + "/" +
                                STRING(YEAR(TODAY) - 7,"9999").
    APPLY 'entry' to tbNonZero.                            
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
  DISPLAY EDITOR-1 tbNonZero end_date begin_cust-no end_cust-no begin_inv 
          end_inv 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE EDITOR-1 tbNonZero end_date begin_cust-no end_cust-no begin_inv 
         end_inv btn-process btn-cancel 
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
    OUTPUT STREAM out1 TO VALUE("c:\tmp\ar-inv" + 
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d").
    OUTPUT STREAM out2 TO VALUE("c:\tmp\ar-invl" +
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d").
    OUTPUT STREAM out3 TO VALUE("c:\tmp\ar-cash" + 
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d").
    OUTPUT STREAM out4 TO VALUE("c:\tmp\ar-cashl" +
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d").

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    SESSION:SET-WAIT-STATE ("general").

    DISABLE TRIGGERS FOR LOAD OF ar-inv.
    DISABLE TRIGGERS FOR LOAD OF ar-invl.
    DISABLE TRIGGERS FOR LOAD OF ar-cash.
    DISABLE TRIGGERS FOR LOAD OF ar-cashl.

    MAIN-LOOP:
    FOR EACH ar-inv WHERE 
        ar-inv.company  EQ cocode AND 
        ar-inv.inv-date LE end_date AND 
        ar-inv.inv-no   GE begin_inv AND
        ar-inv.inv-no   LE end_inv AND
        ar-inv.cust-no  GE begin_cust-no AND
        ar-inv.cust-no  LE end_cust-no AND
        ar-inv.posted   EQ yes
        USE-INDEX inv-date:

        ASSIGN
            invBalAmt = IF ar-inv.net = ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
                    ar-inv.net 
                  ELSE 
                    ar-inv.gross.

        FOR EACH ar-cashl NO-LOCK WHERE
            ar-cashl.company  EQ ar-inv.company AND
            ar-cashl.posted   EQ yes AND
            ar-cashl.cust-no  EQ ar-inv.cust-no AND
            ar-cashl.inv-no   EQ ar-inv.inv-no
            USE-INDEX inv-no:

            
            FIND FIRST ar-cash NO-LOCK WHERE
                ar-cash.c-no EQ ar-cashl.c-no
                USE-INDEX c-no.
            IF ar-cash.check-date > end_date THEN
                NEXT MAIN-LOOP.

            IF ar-cashl.memo THEN DO:
                IF ar-cashl.dscr BEGINS "CREDIT MEMO CREATED FROM OE RETURN" THEN ASSIGN
                    invBalAmt = invBalAmt - ar-cashl.amt-disc.
                ELSE IF ar-cashl.amt-paid + ar-cashl.amt-disc <> 0 THEN ASSIGN
                    invBalAmt = invBalAmt + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                ELSE ASSIGN
                    invBalAmt = invBalAmt + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            END.
            ELSE ASSIGN
                invBalAmt = invBalAmt + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
        END.

        IF invBalAmt = 0 
        OR tbNonZero THEN DO:
        
            ASSIGN
                totWriteOff = totWriteOff + invBalAmt.
                
            FOR EACH ar-cashl EXCLUSIVE WHERE
                ar-cashl.company  EQ ar-inv.company AND
                ar-cashl.posted   EQ yes AND
                ar-cashl.cust-no  EQ ar-inv.cust-no AND
                ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no:

                FIND ar-cash EXCLUSIVE WHERE
                    ar-cash.c-no EQ ar-cashl.c-no
                    USE-INDEX c-no NO-ERROR.
                
                IF AVAIL ar-cash THEN DO:
                    FIND FIRST b-ar-cashl NO-LOCK WHERE
                        b-ar-cashl.c-no EQ ar-cashl.c-no AND 
                        ROWID(b-ar-cashl) NE ROWID(ar-cashl)
                        NO-ERROR.
                    IF NOT AVAIL b-ar-cashl THEN DO:
                        EXPORT STREAM out3 ar-cash.
                        DELETE ar-cash.
                        ASSIGN
                            iPurgeCount = iPurgeCount + 1.
                    END.
                END.

                EXPORT STREAM out4 ar-cashl.
                DELETE ar-cashl.
                ASSIGN
                    iPurgeCount2 = iPurgeCount2 + 1.
            END.

            FOR EACH ar-invl EXCLUSIVE WHERE
                ar-invl.company EQ cocode AND
                ar-invl.x-no EQ ar-inv.x-no:
                EXPORT STREAM out2 ar-invl.
                DELETE ar-invl.
                ASSIGN
                    iPurgeCount3 = iPurgeCount3 + 1.
            END.

            EXPORT STREAM out1 ar-inv.
            DELETE ar-inv.
            ASSIGN
                iPurgeCount4 = iPurgeCount4 + 1.
        END.
    END.

    SESSION:SET-WAIT-STATE("").
    
    OUTPUT STREAM out1 CLOSE.
    OUTPUT STREAM out2 CLOSE.
    OUTPUT STREAM out3 CLOSE.
    OUTPUT STREAM out4 CLOSE.
    
    MESSAGE  
        "Purge process completed." SKIP
        STRING(iPurgeCount4) + " invoices records were deleted." SKIP
        STRING(iPurgeCount3) + " invoice line records were deleted." SKIP
        STRING(iPurgeCount) + " ar-cash records were deleted." SKIP
        STRING(iPurgeCount2) + " ar-cashl records were deleted." SKIP
        STRING(totWriteOff,"->>>,>>>,>>9.99") + " dollars were written off." SKIP
        "Backup files were placed in then C:\tmp directory" SKIP
        "for retrieval if necessary."
        VIEW-AS ALERT-BOX.
        
    APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

