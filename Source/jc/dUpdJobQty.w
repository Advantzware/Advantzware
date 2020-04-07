&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO .
DEFINE INPUT PARAMETER iplApplyJobQty AS LOGICAL NO-UNDO .

{custom/globdefs.i}

DEFINE BUFFER bf-job-hdr FOR job-hdr .
DEFINE TEMP-TABLE tt-job-hdr 
    FIELD frm AS INTEGER
    FIELD blank-no AS INTEGER
    FIELD i-no AS CHARACTER
    FIELD qty AS DECIMAL 
    FIELD on-hand AS DECIMAL 
    FIELD on-order AS DECIMAL 
    FIELD on-allo AS DECIMAL
    FIELD on-required AS DECIMAL
    FIELD org-qty AS DECIMAL
    FIELD job-no AS CHARACTER
    FIELD job-no2 AS INTEGER
    FIELD riJobHdr AS ROWID
    FIELD IS-SELECTED AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX .

DEFINE VARIABLE cbShipFrom AS CHARACTER  NO-UNDO .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-job-hdr

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-job-hdr.IS-SELECTED tt-job-hdr.frm tt-job-hdr.blank-no tt-job-hdr.i-no tt-job-hdr.qty tt-job-hdr.on-hand tt-job-hdr.on-order tt-job-hdr.on-allo tt-job-hdr.on-required   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-job-hdr.IS-SELECTED ~
   tt-job-hdr.qty   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-job-hdr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-job-hdr
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-job-hdr
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-job-hdr .
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-job-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-job-hdr


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_save Btn_apply-jobqty ~
Btn_reset-qty dOverRun Btn_select-all Btn_Deselect-all BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS cJobNo cCustNo cCustName dOverRun 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_apply-jobqty  NO-CONVERT-3D-COLORS
     LABEL "Apply" 
     SIZE 20.2 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Deselect-all  NO-CONVERT-3D-COLORS
     LABEL "Deselect All" 
     SIZE 14.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_reset-qty  NO-CONVERT-3D-COLORS
     LABEL "Reset" 
     SIZE 20.2 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_save 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U
     LABEL "&Save" 
     SIZE 8 BY 1.81
     BGCOLOR 8 .

DEFINE BUTTON Btn_select-all  NO-CONVERT-3D-COLORS
     LABEL "Select All" 
     SIZE 12.8 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cCustName AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustNo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cust" 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cJobNo AS CHARACTER FORMAT "X(25)":U 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dOverRun AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Overrun %" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 119.6 BY 15
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-job-hdr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-job-hdr.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX
      tt-job-hdr.frm FORMAT ">>>":U COLUMN-LABEL "F" WIDTH 4
      tt-job-hdr.blank-no FORMAT ">>>":U COLUMN-LABEL "B"  WIDTH 4
      tt-job-hdr.i-no FORMAT "x(15)":U COLUMN-LABEL "Item" WIDTH 20   
      tt-job-hdr.qty FORMAT "->,>>>,>>9" COLUMN-LABEL "Job Quantity"
      tt-job-hdr.on-hand FORMAT "->,>>>,>>9" COLUMN-LABEL "On Hand"
      tt-job-hdr.on-order FORMAT "->,>>>,>>9" COLUMN-LABEL "On Order"
      tt-job-hdr.on-allo FORMAT "->,>>>,>>9" COLUMN-LABEL "Allocated"
      tt-job-hdr.on-required FORMAT "->,>>>,>>9" COLUMN-LABEL "Required"
     ENABLE tt-job-hdr.IS-SELECTED
            tt-job-hdr.qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 116.6 BY 10.24
         BGCOLOR 8 FONT 0 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_save AT ROW 1.14 COL 112 WIDGET-ID 12
     cJobNo AT ROW 1.38 COL 15.8 COLON-ALIGNED WIDGET-ID 4
     cCustNo AT ROW 1.38 COL 46.8 COLON-ALIGNED WIDGET-ID 6
     cCustName AT ROW 1.38 COL 63.4 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     Btn_apply-jobqty AT ROW 2.57 COL 31.8
     Btn_reset-qty AT ROW 2.57 COL 53.2 WIDGET-ID 10
     dOverRun AT ROW 2.62 COL 15.8 COLON-ALIGNED
     Btn_select-all AT ROW 3.76 COL 3.4 WIDGET-ID 84
     Btn_Deselect-all AT ROW 3.76 COL 16.4 WIDGET-ID 86
     BROWSE-1 AT ROW 5.05 COL 3.4
     RECT-1 AT ROW 1.1 COL 1.2 WIDGET-ID 82
     SPACE(0.50) SKIP(0.50)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Job Quantity Entry".


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
/* BROWSE-TAB BROWSE-1 Btn_Deselect-all Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cCustName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cCustNo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cJobNo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-job-hdr .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Job Quantity Entry */
DO: 
    DEFINE BUFFER bf-tt-job-hdr FOR tt-job-hdr .
    FOR EACH bf-tt-job-hdr NO-LOCK ,
       FIRST bf-job-hdr EXCLUSIVE-LOCK
       WHERE ROWID(bf-job-hdr) EQ bf-tt-job-hdr.riJobHdr 
           AND bf-job-hdr.qty NE bf-tt-job-hdr.qty:
        ASSIGN bf-job-hdr.qty = bf-tt-job-hdr.qty .
    END.         
   RELEASE bf-job-hdr .
  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-job-hdr.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-job-hdr.qty BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF tt-job-hdr.qty IN BROWSE BROWSE-1 /* qty */
DO:
     tt-job-hdr.qty = integer(tt-job-hdr.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NO-ERROR . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-job-hdr.IS-SELECTED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-job-hdr.IS-SELECTED BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF tt-job-hdr.IS-SELECTED IN BROWSE BROWSE-1 /* IS-SELECTED */
DO:
     tt-job-hdr.IS-SELECTED = LOGICAL(tt-job-hdr.IS-SELECTED:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NO-ERROR . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_apply-jobqty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_apply-jobqty Dialog-Frame
ON CHOOSE OF Btn_apply-jobqty IN FRAME Dialog-Frame /* Apply */
DO:
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
    DEFINE BUFFER bf-tt-job-hdr FOR tt-job-hdr .
    DEFINE BUFFER bf-tt-job-hdr-selected FOR tt-job-hdr.
    
    DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}.
     /* The below code will make sure to increase the job quantity by overrun percentage
        of all blanks for a selected FORM */
     FOR EACH bf-tt-job-hdr
         WHERE bf-tt-job-hdr.IS-SELECTED
         BREAK BY bf-tt-job-hdr.frm:
         IF LAST-OF(bf-tt-job-hdr.frm) THEN DO:
             FOR EACH bf-tt-job-hdr-selected
                 WHERE bf-tt-job-hdr-selected.frm    EQ bf-tt-job-hdr.frm
                   AND ROWID(bf-tt-job-hdr-selected) NE ROWID(bf-tt-job-hdr):
                 bf-tt-job-hdr-selected.IS-SELECTED = TRUE.
             END.
         END.
     END.
             
     FOR EACH bf-tt-job-hdr WHERE bf-tt-job-hdr.IS-SELECTED
            EXCLUSIVE-LOCK BY bf-tt-job-hdr.frm:
         bf-tt-job-hdr.qty = bf-tt-job-hdr.org-qty + (bf-tt-job-hdr.org-qty * dOverRun * .01) .
                       {sys/inc/roundup.i bf-tt-job-hdr.qty}
     END.
     ASSIGN
         Btn_apply-jobqty:SENSITIVE = NO
         dOverRun:SENSITIVE = NO 
         Btn_apply-jobqty:LABEL = "Already Applied"
         Btn_reset-qty:SENSITIVE = YES .
    END.

    RUN open-query(rwRowid) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deselect-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect-all Dialog-Frame
ON CHOOSE OF Btn_Deselect-all IN FRAME Dialog-Frame /* Deselect All */
DO:
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
    DEFINE BUFFER bf-tt-job-hdr FOR tt-job-hdr .
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH bf-tt-job-hdr 
            EXCLUSIVE-LOCK BY bf-tt-job-hdr.frm:
            bf-tt-job-hdr.IS-SELECTED = NO .
        END.
    END.

    RUN open-query(rwRowid) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_reset-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_reset-qty Dialog-Frame
ON CHOOSE OF Btn_reset-qty IN FRAME Dialog-Frame /* Reset */
DO:
    DEFINE BUFFER bf-tt-job-hdr FOR tt-job-hdr .
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
       
     ASSIGN {&displayed-objects}.
       

     FOR EACH bf-tt-job-hdr
            EXCLUSIVE-LOCK  BY bf-tt-job-hdr.frm:
            bf-tt-job-hdr.qty = bf-tt-job-hdr.org-qty .
     END.
     ASSIGN
         Btn_apply-jobqty:SENSITIVE = YES
         dOverRun:SENSITIVE = YES 
         Btn_apply-jobqty:LABEL = "Apply"
         Btn_reset-qty:SENSITIVE = NO .
    END.

    RUN open-query (rwRowid).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_save Dialog-Frame
ON CHOOSE OF Btn_save IN FRAME Dialog-Frame /* Save */
DO:
    DEFINE BUFFER bf-tt-job-hdr FOR tt-job-hdr .
    FOR EACH bf-tt-job-hdr NO-LOCK ,
       FIRST bf-job-hdr WHERE bf-job-hdr.job-no EQ bf-tt-job-hdr.job-no
           AND bf-job-hdr.job-no2 EQ bf-tt-job-hdr.job-no2
           AND bf-job-hdr.frm EQ bf-tt-job-hdr.frm
           AND bf-job-hdr.blank-no EQ bf-tt-job-hdr.blank-no EXCLUSIVE-LOCK:
        ASSIGN bf-job-hdr.qty = bf-tt-job-hdr.qty .
    END.
   RELEASE bf-job-hdr .
  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_select-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_select-all Dialog-Frame
ON CHOOSE OF Btn_select-all IN FRAME Dialog-Frame /* Select All */
DO:
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
    DEFINE BUFFER bf-tt-job-hdr FOR tt-job-hdr .
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH bf-tt-job-hdr 
            EXCLUSIVE-LOCK BY bf-tt-job-hdr.frm:
            bf-tt-job-hdr.IS-SELECTED = YES .
        END.
    END.

    RUN open-query(rwRowid) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustName Dialog-Frame
ON HELP OF cCustName IN FRAME Dialog-Frame
DO:
    /*def var char-val as cha no-undo.
    run windows/l-shipto.w (g_company, g_loc, ip-cust-no, focus:screen-value, output char-val).
    if char-val <> "" then self:screen-value = entry(1,char-val).*/
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dOverRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dOverRun Dialog-Frame
ON LEAVE OF dOverRun IN FRAME Dialog-Frame /* Overrun % */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{sys/inc/f3helpw.i}

DEF VAR v-return AS LOG NO-UNDO.
DEF VAR lcLastValue AS CHAR NO-UNDO.
DEF VAR liNumUsrx AS INT NO-UNDO.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  

  RUN enable_UI.
  
  FIND FIRST job WHERE ROWID(job) EQ iprRowid  NO-LOCK NO-ERROR.
  FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.job EQ job.job
        AND job-hdr.job-no EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2 NO-ERROR .
  FIND FIRST cust NO-LOCK
      WHERE cust.company EQ job-hdr.company
        AND cust.cust-no EQ job-hdr.cust-no NO-ERROR . 

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
          cJobNo:SCREEN-VALUE = job.job-no + "-" + STRING(job.job-no2)
          cCustNo:SCREEN-VALUE = IF AVAIL job-hdr THEN job-hdr.cust-no ELSE ""
          cCustName:SCREEN-VALUE = IF AVAIL cust THEN cust.NAME ELSE ""
          dOverRun:SCREEN-VALUE = string(IF AVAIL cust THEN cust.over-pct ELSE 0) 
          dOverRun = (IF AVAIL cust THEN cust.over-pct ELSE 0) .
     IF iplApplyJobQty THEN
         ASSIGN
         Btn_apply-jobqty:SENSITIVE = NO
         dOverRun:SENSITIVE = NO 
         Btn_apply-jobqty:LABEL = "Already Applied"
         Btn_reset-qty:SENSITIVE = YES .
     ELSE Btn_reset-qty:SENSITIVE = NO .
  END.

  RUN build-table.
 
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
    DEFINE BUFFER bf-tt-job-hdr FOR tt-job-hdr .
    DO WITH FRAME {&FRAME-NAME}:
        

        EMPTY TEMP-TABLE tt-job-hdr.
        FOR EACH bf-job-hdr
            NO-LOCK where bf-job-hdr.company eq job.company 
            and bf-job-hdr.job eq job.job and bf-job-hdr.job-no eq job.job-no 
            and bf-job-hdr.job-no2 eq job.job-no2 BY bf-job-hdr.frm .

            CREATE tt-job-hdr .
            ASSIGN
                tt-job-hdr.frm        =   bf-job-hdr.frm     
                tt-job-hdr.blank-no   =   bf-job-hdr.blank-no
                tt-job-hdr.i-no       =   bf-job-hdr.i-no    
                tt-job-hdr.qty        =   bf-job-hdr.qty     
                tt-job-hdr.org-qty    =   bf-job-hdr.qty 
                tt-job-hdr.job-no     =   bf-job-hdr.job-no  
                tt-job-hdr.job-no2    =   bf-job-hdr.job-no2 
                tt-job-hdr.riJobHdr   =   ROWID(bf-job-hdr)
                tt-job-hdr.IS-SELECTED =  YES .

           FIND FIRST itemfg NO-LOCK
               WHERE itemfg.company EQ job.company 
               AND itemfg.i-no EQ tt-job-hdr.i-no NO-ERROR .
           IF AVAIL itemfg THEN do:
               ASSIGN
                 tt-job-hdr.on-hand  = itemfg.q-onh
                 tt-job-hdr.on-order = itemfg.q-ono
                 tt-job-hdr.on-allo  = itemfg.q-alloc .

                 tt-job-hdr.on-required = MAX(0,(itemfg.ord-level -  (itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc))) .

           END.
        END.

        IF iplApplyJobQty THEN do:
            FOR EACH bf-tt-job-hdr
                EXCLUSIVE-LOCK BY bf-tt-job-hdr.frm:
                bf-tt-job-hdr.qty = bf-tt-job-hdr.org-qty + (bf-tt-job-hdr.org-qty * dOverRun * .01) .
                {sys/inc/roundup.i bf-tt-job-hdr.qty}
            END.
        END.

        CLOSE QUERY BROWSE-1. 
        OPEN QUERY BROWSE-1 FOR EACH tt-job-hdr BY tt-job-hdr.frm.
           
            APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
            APPLY "ENTRY" TO {&BROWSE-NAME}.
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
  DISPLAY cJobNo cCustNo cCustName dOverRun 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 Btn_save Btn_apply-jobqty Btn_reset-qty dOverRun Btn_select-all 
         Btn_Deselect-all BROWSE-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Dialog-Frame 
PROCEDURE open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        CLOSE QUERY BROWSE-1. 
        OPEN QUERY BROWSE-1 FOR EACH tt-job-hdr BY tt-job-hdr.frm.

            REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.  
            
            APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
            APPLY "ENTRY" TO {&BROWSE-NAME}.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

