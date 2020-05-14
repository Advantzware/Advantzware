&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:            est/delRouteMulti.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:      Sewa Singh

  Created:     Wed Oct 23 2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iprwRowidEb AS ROWID NO-UNDO .
DEFINE INPUT PARAMETER iprwRecid AS RECID NO-UNDO .
DEFINE INPUT PARAMETER iprwRowidEstQty AS ROWID NO-UNDO .
DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */

{custom/globdefs.i}

DEFINE TEMP-TABLE tt-oe-route 
    FIELD tt-recid    AS RECID
    FIELD s-num       AS INTEGER
    FIELD b-num       AS INTEGER
    FIELD m-code      AS CHARACTER 
    FIELD m-dscr      AS CHARACTER
    FIELD i-count     AS INTEGER
    FIELD company     AS CHARACTER
    FIELD IS-SELECTED AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX .

DEFINE VARIABLE lv-eqty LIKE est-qty.eqty NO-UNDO.
DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id AS RECID     NO-UNDO.
DEFINE BUFFER xop FOR est-op.
DEFINE NEW SHARED BUFFER xest    FOR est.
DEFINE NEW SHARED BUFFER xef     FOR ef.
DEFINE NEW SHARED BUFFER xeb     FOR eb.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME browse-route

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-oe-route

/* Definitions for BROWSE browse-route                                 */
&Scoped-define FIELDS-IN-QUERY-browse-route tt-oe-route.IS-SELECTED tt-oe-route.s-num tt-oe-route.b-num tt-oe-route.m-code tt-oe-route.m-dscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-route tt-oe-route.IS-SELECTED   
&Scoped-define ENABLED-TABLES-IN-QUERY-browse-route tt-oe-route
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browse-route tt-oe-route
&Scoped-define SELF-NAME browse-route
&Scoped-define QUERY-STRING-browse-route FOR EACH tt-oe-route      NO-LOCK BY tt-oe-route.s-num
&Scoped-define OPEN-QUERY-browse-route OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-route      NO-LOCK BY tt-oe-route.s-num.
&Scoped-define TABLES-IN-QUERY-browse-route tt-oe-route
&Scoped-define FIRST-TABLE-IN-QUERY-browse-route tt-oe-route


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-browse-route}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS browse-route Btn_OK Btn_Select btn-cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 18 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO  NO-CONVERT-3D-COLORS
    LABEL "Delete" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_Select  NO-CONVERT-3D-COLORS
    LABEL "Select All" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-route FOR 
    tt-oe-route SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-route
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-route Dialog-Frame _FREEFORM
    QUERY browse-route NO-LOCK DISPLAY
    tt-oe-route.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX 
    tt-oe-route.s-num FORMAT ">9" COLUMN-LABEL "S" WIDTH 6 
    tt-oe-route.b-num FORMAT ">9" COLUMN-LABEL "B" WIDTH 6 
    tt-oe-route.m-code FORMAT "X(6)" COLUMN-LABEL "Machine"  
    tt-oe-route.m-dscr FORMAT "X(20)" COLUMN-LABEL "Description" 
      ENABLE tt-oe-route.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 70 BY 10.24
         BGCOLOR 8 FONT 3 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    browse-route AT ROW 1.57 COL 2.6
    Btn_Select AT ROW 12.81 COL 5.2
    Btn_OK AT ROW 12.81 COL 27.5
    btn-cancel AT ROW 12.81 COL 50.2
    SPACE(12.39) SKIP(1.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FONT 6
    TITLE "Delete Multiple Route"
    DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB browse-route 1 Dialog-Frame */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-route
/* Query rebuild information for BROWSE browse-route
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-route
     NO-LOCK BY tt-oe-route.s-num.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-route */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Delete Multipal Route */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browse-route
&Scoped-define SELF-NAME browse-route
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browse-route Dialog-Frame
ON ROW-DISPLAY OF browse-route IN FRAME Dialog-Frame
    DO:   
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
    DO:
        DEFINE VARIABLE v-process AS LOG     NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER NO-UNDO .

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        i = 0 .
        FOR EACH tt-oe-route WHERE tt-oe-route.IS-SELECTED :
            i = i + 1.
        END.

        IF i = 0  THEN 
        DO:
            MESSAGE "Please Select Record ..."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK .
            RETURN NO-APPLY .
        END.
        
        IF ipcType EQ "Delete" THEN DO:

            MESSAGE "Are you sure you want to delete records" 
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

            IF v-process THEN RUN run-process.
        END.  
        IF ipcType EQ "Import" THEN DO:
            MESSAGE "Are you sure you want to Import Standards" 
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

            IF v-process THEN RUN pRunImport.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select Dialog-Frame
ON CHOOSE OF Btn_Select IN FRAME Dialog-Frame /* OK */
    DO:
        FOR EACH tt-oe-route NO-LOCK:
             tt-oe-route.IS-SELECTED = YES .
        END.
        
        CLOSE QUERY browse-route.
        DO WITH FRAME {&FRAME-NAME}:
            
            OPEN QUERY browse-route FOR EACH tt-oe-route
                NO-LOCK BY tt-oe-route.s-num.
            END.
        END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-oe-route.IS-SELECTED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-oe-route.IS-SELECTED Browser-Table _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF tt-oe-route.IS-SELECTED IN BROWSE browse-route /* IS-SELECTED */
    DO:

        tt-oe-route.IS-SELECTED = LOGICAL(tt-oe-route.IS-SELECTED:SCREEN-VALUE IN BROWSE {&browse-name} ) .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

DEFINE VARIABLE v-return    AS LOG       NO-UNDO.
DEFINE VARIABLE lcLastValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE liNumUsrx   AS INTEGER   NO-UNDO.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
{sys/inc/f3helpw.i}

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
    RUN enable_UI.

    RUN build-table .

    FIND FIRST tt-oe-route NO-LOCK
        WHERE tt-oe-route.tt-recid EQ iprwRecid NO-ERROR .

    IF AVAILABLE tt-oe-route THEN
        tt-oe-route.IS-SELECTED    = YES .

    CLOSE QUERY browse-route.
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY browse-route FOR EACH tt-oe-route
            NO-LOCK BY tt-oe-route.s-num.
      IF ipcType EQ "Import" THEN DO:
            FRAME {&FRAME-NAME}:TITLE = "Import Routing" .
            Btn_OK:LABEL = "Import".
       END.      
    END.
 
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST est WHERE ROWID(est) EQ iprwRowidEb NO-LOCK NO-ERROR .
        FIND FIRST est-qty NO-LOCK
            WHERE est-qty.company EQ est.company
            AND rowid(est-qty) EQ iprwRowidEstQty NO-ERROR .

        EMPTY TEMP-TABLE tt-oe-route .    

       
        IF est.est-type LT 5 THEN 
        DO:

            lv-eqty = 0.
            IF AVAILABLE est THEN
                IF est.est-type NE 4 THEN lv-eqty = est-qty.eqty.
                ELSE
                    FOR EACH xop NO-LOCK
                        WHERE xop.company EQ est-qty.company
                        AND xop.est-no  EQ est-qty.est-no
                        AND xop.line    LT 500
                        BY xop.qty:
                        lv-eqty = xop.qty.
                        LEAVE.
                    END.
            
            FOR EACH est-op WHERE est-op.company = est-qty.company 
                AND est-op.est-no = est-qty.est-no 
                AND est-op.line < 500 
                AND ((ASI.est-op.qty EQ est-qty.eqty AND est.est-type EQ 1) OR 
                (ASI.est-op.qty EQ lv-eqty AND est.est-type NE 1))  
                NO-LOCK :

                CREATE tt-oe-route.
                ASSIGN 
                    tt-oe-route.tt-recid = RECID(est-op)
                    tt-oe-route.company  = est-op.company
                    tt-oe-route.s-num    = est-op.s-num  
                    tt-oe-route.b-num    = est-op.b-num
                    tt-oe-route.m-code   = est-op.m-code
                    tt-oe-route.m-dscr   = est-op.m-dscr   .
            END.

        END.
        ELSE 
        DO:
            lv-eqty = 0.
            IF AVAILABLE est THEN
                IF est.est-type NE 8 THEN lv-eqty = est-qty.eqty.
                ELSE
                    FOR EACH xop
                        WHERE xop.company EQ est-qty.company
                        AND xop.est-no  EQ est-qty.est-no
                        AND xop.line    LT 500
                        NO-LOCK
                        BY xop.qty:
                        lv-eqty = xop.qty.
                        LEAVE.
                    END.

            FOR EACH est-op WHERE est-op.company = est-qty.company 
                AND est-op.est-no = est-qty.est-no 
                AND est-op.line LT 500 AND 
                ((ASI.est-op.qty EQ est-qty.eqty AND est.est-type NE 8) OR 
                (ASI.est-op.qty EQ lv-eqty AND est.est-type GE 7)) NO-LOCK :
                
                CREATE tt-oe-route.
                ASSIGN 
                    tt-oe-route.tt-recid = RECID(est-op)
                    tt-oe-route.company  = est-op.company
                    tt-oe-route.s-num    = est-op.s-num  
                    tt-oe-route.b-num    = est-op.b-num
                    tt-oe-route.m-code   = est-op.m-code
                    tt-oe-route.m-dscr   = est-op.m-dscr   .
            END.

        END. /* else do*/

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
    HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
    ENABLE browse-route Btn_OK Btn_Select btn-cancel 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
    DEFINE BUFFER bf-est-op FOR est-op .
    FOR EACH tt-oe-route WHERE tt-oe-route.IS-SELECTED:
        FOR EACH bf-est-op EXCLUSIVE-LOCK 
            WHERE bf-est-op.company EQ tt-oe-route.company AND 
            recid(bf-est-op) EQ tt-oe-route.tt-recid :
            DELETE bf-est-op .
        END.
    END.
   
    RELEASE bf-est-op .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunImport Dialog-Frame 
PROCEDURE pRunImport :
    DEFINE BUFFER bf-est-op FOR est-op .
    
    FIND FIRST xest WHERE ROWID(xest) EQ iprwRowidEb NO-LOCK NO-ERROR .      
    
    FOR EACH tt-oe-route WHERE tt-oe-route.IS-SELECTED:
        FOR EACH bf-est-op NO-LOCK 
            WHERE bf-est-op.company EQ tt-oe-route.company AND 
            recid(bf-est-op) EQ tt-oe-route.tt-recid :
            
             FIND FIRST xef NO-LOCK
                  WHERE xef.company EQ bf-est-op.company
                  AND xef.est-no  EQ bf-est-op.est-no
                  AND xef.form-no EQ bf-est-op.s-num
                  NO-ERROR.
                  
             FIND FIRST xeb
                WHERE xeb.company   EQ xef.company
                AND xeb.est-no    EQ xef.est-no
                AND xeb.form-no   EQ xef.form-no
                AND (xeb.blank-no EQ bf-est-op.b-num OR bf-est-op.b-num EQ 0)
                NO-LOCK NO-ERROR.
                
                FOR EACH ef 
                    WHERE ef.company EQ bf-est-op.company
                    AND ef.est-no  EQ bf-est-op.est-no
                    NO-LOCK:
                    RUN set-lock (ef.form-no, NO).
                END.
                
                fil_id  = RECID(bf-est-op).
                
                IF xest.est-type LT 5 THEN
                RUN ce/mach-rek.p (ROWID(bf-est-op)).
                ELSE
                RUN cec/mach-rek.p (ROWID(bf-est-op)).
                
                FOR EACH ef 
                    WHERE ef.company EQ est-op.company
                    AND ef.est-no  EQ est-op.est-no
                  NO-LOCK:
                    RUN set-lock (ef.form-no, YES).
                END.
            
        END.
    END.
   
    RELEASE bf-est-op .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-lock Dialog-Frame 
PROCEDURE set-lock :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-form-no LIKE ef.form-no NO-UNDO.
    DEFINE INPUT PARAMETER ip-op-lock LIKE ef.op-lock NO-UNDO.
  

    FIND FIRST ef
        WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND ef.form-no EQ ip-form-no
        NO-ERROR.
    IF AVAILABLE ef THEN 
    DO:
        ef.op-lock = ip-op-lock.
        RELEASE ef.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
