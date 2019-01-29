&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: po\d-poordl.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid2 AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid3 AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER ip-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE VARIABLE li AS INTEGER NO-UNDO.

{est/d-selblk.i NEW}
{sys/inc/ceprepprice.i}

DEFINE VARIABLE lv-item-recid   AS RECID   NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL NO-UNDO.

DEFINE NEW SHARED BUFFER xest    FOR est.
DEFINE NEW SHARED BUFFER xef     FOR ef.
DEFINE NEW SHARED BUFFER xeb     FOR eb.

DEFINE            BUFFER xop     FOR est-op.
DEFINE            BUFFER op-lock FOR reftable.

DEFINE NEW SHARED VARIABLE xcal                AS DECIMAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-wid              AS DECIMAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-len              AS DECIMAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id              AS RECID     NO-UNDO.
DEFINE NEW SHARED VARIABLE maxco               AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE qty                 AS INTEGER   NO-UNDO.

DEFINE            VARIABLE ll-import-stds      AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-d-seq            LIKE est-op.d-seq NO-UNDO.
DEFINE            VARIABLE lv-dept             LIKE est-op.dept NO-UNDO.
DEFINE            VARIABLE lv-op-sb            LIKE est-op.op-sb NO-UNDO.
DEFINE            VARIABLE lv-b-num            LIKE est-op.b-num NO-UNDO.
DEFINE            VARIABLE lv-n-out            LIKE est-op.n-out NO-UNDO.
DEFINE            VARIABLE v-passes            AS INTEGER   NO-UNDO.
DEFINE            VARIABLE ll-machine-modified AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-import-selected  AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-import-all       AS LOG       NO-UNDO.
DEFINE            VARIABLE v-avail             AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-qty              LIKE est-op.qty NO-UNDO.
DEFINE            VARIABLE li-cnt              AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-foam-depts       AS CHARACTER INIT "DC,RC" NO-UNDO.
DEFINE            VARIABLE lv-n-out-depts      AS CHARACTER INIT "CR,RC" NO-UNDO.
DEFINE            VARIABLE prev-m-code         LIKE est-op.m-code NO-UNDO.
DEFINE            VARIABLE ll-foam             AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-eqty             LIKE est-qty.eqty NO-UNDO.
DEFINE            VARIABLE v-override-mode     AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-add-record       AS LOG       NO-UNDO.

{est/d-machex.i NEW}


DO WITH TRANSACTION:
    {sys\inc\estopmch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est-op est est-qty

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame est-op.isLocked est-op.s-num ~
est-op.b-num est-op.m-code est-op.m-dscr est-op.op-pass est-op.n-out ~
est-op.op-mr est-op.op-waste est-op.op-speed est-op.op-spoil ~
est-op.op-crew[1] est-op.op-crew[2] est-op.op-rate[1] est-op.op-rate[2] ~
est-op.plates est-op.fountains est-op.att-type[1] est-op.att-qty[1] ~
est-op.att-type[2] est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] ~
est-op.spare-char-1 est-op.n_out_div 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame est-op.isLocked ~
est-op.s-num est-op.b-num est-op.m-code est-op.m-dscr est-op.op-pass ~
est-op.n-out est-op.op-mr est-op.op-waste est-op.op-speed est-op.op-spoil ~
est-op.op-crew[1] est-op.op-crew[2] est-op.op-rate[1] est-op.op-rate[2] ~
est-op.plates est-op.fountains est-op.att-type[1] est-op.att-qty[1] ~
est-op.att-type[2] est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] ~
est-op.spare-char-1 est-op.n_out_div 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame est-op
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame est-op
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH est-op ~
      WHERE ASI.est-op.company eq cocode  SHARE-LOCK, ~
      EACH est SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH est-op ~
      WHERE ASI.est-op.company eq cocode  SHARE-LOCK, ~
      EACH est SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame est-op est est-qty
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame est-op
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame est
&Scoped-define THIRD-TABLE-IN-QUERY-Dialog-Frame est-qty


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS est-op.isLocked est-op.s-num est-op.b-num ~
est-op.m-code est-op.m-dscr est-op.op-pass est-op.n-out est-op.op-mr ~
est-op.op-waste est-op.op-speed est-op.op-spoil est-op.op-crew[1] ~
est-op.op-crew[2] est-op.op-rate[1] est-op.op-rate[2] est-op.plates ~
est-op.fountains est-op.att-type[1] est-op.att-qty[1] est-op.att-type[2] ~
est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] est-op.spare-char-1 ~
est-op.n_out_div 
&Scoped-define ENABLED-TABLES est-op
&Scoped-define FIRST-ENABLED-TABLE est-op
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS est-op.isLocked est-op.s-num est-op.b-num ~
est-op.m-code est-op.m-dscr est-op.op-pass est-op.n-out est-op.op-mr ~
est-op.op-waste est-op.op-speed est-op.op-spoil est-op.op-crew[1] ~
est-op.op-crew[2] est-op.op-rate[1] est-op.op-rate[2] est-op.plates ~
est-op.fountains est-op.att-type[1] est-op.att-qty[1] est-op.att-type[2] ~
est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] est-op.spare-char-1 ~
est-op.n_out_div 
&Scoped-define DISPLAYED-TABLES est-op
&Scoped-define FIRST-DISPLAYED-TABLE est-op


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Done" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 30 BY 3
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 127 BY 13.81
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      est-op, 
      est, 
      est-qty SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     est-op.isLocked AT ROW 13.48 COL 109.6 WIDGET-ID 2
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
          BGCOLOR 15 FONT 1
     est-op.s-num AT ROW 1.43 COL 63.4 COLON-ALIGNED
          LABEL "Sheet #" FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.b-num AT ROW 2.67 COL 63.4 COLON-ALIGNED
          LABEL "Blank#" FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.m-code AT ROW 1.43 COL 15.8 COLON-ALIGNED
          LABEL "Machine" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.m-dscr AT ROW 2.62 COL 15.8 COLON-ALIGNED
          LABEL "Desc" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
          BGCOLOR 15 FONT 1
     est-op.op-pass AT ROW 2.76 COL 92.2 COLON-ALIGNED
          LABEL "Pass#." FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.n-out AT ROW 1.43 COL 91.8 COLON-ALIGNED
          LABEL "Out." FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.op-crew[1] AT ROW 5.05 COL 15.8 COLON-ALIGNED
          LABEL "MRCrew" FORMAT "9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.op-crew[2] AT ROW 4.57 COL 59.4 COLON-ALIGNED
          LABEL "RunCrew" FORMAT "9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.op-rate[1] AT ROW 6.29 COL 15.8 COLON-ALIGNED
          LABEL "MRate" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.op-rate[2] AT ROW 5.81 COL 59.4 COLON-ALIGNED
          LABEL "RRate" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    est-op.op-mr AT ROW 7.57 COL 15.8 COLON-ALIGNED
          LABEL "MR-Hrs" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    est-op.op-speed AT ROW 7.05 COL 59.4 COLON-ALIGNED
          LABEL "Speed" FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    est-op.op-waste AT ROW 8.81 COL 15.8 COLON-ALIGNED
          LABEL "Waste" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    est-op.op-spoil AT ROW 8.33 COL 59.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    est-op.n_out_div AT ROW 9.57 COL 59.4 COLON-ALIGNED
          LABEL "Run Qty Divisor" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    est-op.spare-char-1 AT ROW 10.81 COL 59.4 COLON-ALIGNED
          LABEL "Feed" FORMAT "x(1)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.plates AT ROW 4.57 COL 104.4 COLON-ALIGNED
          LABEL "Plate changes" FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.fountains AT ROW 5.81 COL 104.4 COLON-ALIGNED
          LABEL "Fountain Changes" FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     est-op.att-type[1] AT ROW 12.29 COL 15.8 COLON-ALIGNED
          LABEL "Adder 1" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.att-qty[1] AT ROW 13.48 COL 15.8 COLON-ALIGNED
          LABEL "Qty" FORMAT ">>,>>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     est-op.att-type[2] AT ROW 12.29 COL 45.6 COLON-ALIGNED
          LABEL "Adder 2" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.att-qty[2] AT ROW 13.48 COL 45.6 COLON-ALIGNED
          LABEL "Qty" FORMAT ">>,>>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.att-type[3] AT ROW 12.29 COL 75.6 COLON-ALIGNED
          LABEL "Adder 3" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-op.att-qty[3] AT ROW 13.48 COL 75.6 COLON-ALIGNED
          LABEL "Qty" FORMAT ">>,>>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     Btn_OK AT ROW 15.57 COL 105
     Btn_Done AT ROW 15.57 COL 110
     Btn_Cancel AT ROW 15.57 COL 115
     RECT-21 AT ROW 14.91 COL 98
     RECT-38 AT ROW 1 COL 1
     SPACE(2.19) SKIP(3.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Estimate Operation Update".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN est-op.att-qty[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.att-qty[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.att-qty[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.att-type[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.att-type[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.att-type[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.b-num IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.fountains IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.m-code IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.m-dscr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.n-out IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.n_out_div IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-crew[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-crew[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-mr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-pass IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-rate[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-rate[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-speed IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-waste IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.plates IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.s-num IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.spare-char-1 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.est-op,asi.est,asi.est-qty "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.est-op.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Estimate Operation Update */
DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lw-focus AS HANDLE    NO-UNDO.
        DEFINE VARIABLE rowidval AS ROWID     NO-UNDO.
        DEFINE BUFFER bff-eb FOR eb . 

        lw-focus = FOCUS.

        CASE lw-focus:NAME:
            WHEN "m-code" THEN 
                DO:
                    RUN windows/l-mach.w (est.company, est.loc, lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN
                        ASSIGN
                            lw-focus:SCREEN-VALUE      = ENTRY(1,char-val)
                            est-op.m-dscr:SCREEN-VALUE = ENTRY(2,char-val).
                END.
            WHEN "att-type" THEN 
                DO:
                    RUN windows/l-mchatt.w (est.company, est-op.m-code:SCREEN-VALUE , lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                    DO:
                        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                        RUN new-att-type (lw-focus).
                    END.
                END.

            WHEN "s-num" THEN 
                DO:
                    RUN windows/l-esteb.w (est.company,locode, IF AVAILABLE est THEN est.est-no ELSE ""  , INPUT-OUTPUT rowidval).
                    IF rowidval <> ? THEN 
                    DO:
                        FIND FIRST bff-eb NO-LOCK WHERE ROWID(bff-eb) EQ rowidval  NO-ERROR .
                        IF AVAILABLE bff-eb THEN
                            est-op.s-num:screen-value  = STRING(bff-eb.FORM-no) .
                    END.
                END.
            WHEN "s-num" THEN 
                DO:
                    RUN windows/l-esteb.w (est.company,locode, IF AVAILABLE est THEN est.est-no ELSE ""  , INPUT-OUTPUT rowidval).
                    IF rowidval <> ? THEN 
                    DO:
                        FIND FIRST bff-eb NO-LOCK WHERE ROWID(bff-eb) EQ rowidval  NO-ERROR .
                        IF AVAILABLE bff-eb THEN
                            est-op.b-num:screen-value  = STRING(bff-eb.blank-no) .
                    END.
                END.
        END CASE.

        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Estimate Operation Update */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Operation Update */
DO:
        DISABLE TRIGGERS FOR LOAD OF est-op .

        IF lv-item-recid NE ? THEN 
        DO:
  
            FIND FIRST est-op EXCLUSIVE-LOCK
                WHERE RECID(est-op) EQ lv-item-recid  NO-ERROR.
  
            IF AVAILABLE est-op THEN DELETE est-op .
        END.

        /* APPLY "END-ERROR":U TO SELF.*/
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-qty[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-qty[3] Dialog-Frame
ON LEAVE OF est-op.att-qty[3] IN FRAME Dialog-Frame /* Qty */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF ll-import-stds THEN RUN get-stds.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-type[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[1] Dialog-Frame
ON LEAVE OF est-op.att-type[1] IN FRAME Dialog-Frame /* Adder 1 */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-att-type (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[1] Dialog-Frame
ON VALUE-CHANGED OF est-op.att-type[1] IN FRAME Dialog-Frame /* Adder 1 */
DO:
        RUN new-att-type (FOCUS).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-type[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[2] Dialog-Frame
ON LEAVE OF est-op.att-type[2] IN FRAME Dialog-Frame /* Adder 2 */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-att-type (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[2] Dialog-Frame
ON VALUE-CHANGED OF est-op.att-type[2] IN FRAME Dialog-Frame /* Adder 2 */
DO:
        RUN new-att-type (FOCUS).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-type[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[3] Dialog-Frame
ON LEAVE OF est-op.att-type[3] IN FRAME Dialog-Frame /* Adder 3 */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-att-type (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[3] Dialog-Frame
ON VALUE-CHANGED OF est-op.att-type[3] IN FRAME Dialog-Frame /* Adder 3 */
DO:
        RUN new-att-type (FOCUS).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.b-num Dialog-Frame
ON ENTRY OF est-op.b-num IN FRAME Dialog-Frame /* Blank# */
DO:
        DEFINE VARIABLE ll-1-blank AS LOGICAL NO-UNDO.

        DEFINE BUFFER b-eb FOR eb.

        FIND mach WHERE mach.company = est.company 
            AND mach.m-code = est-op.m-code
            NO-LOCK NO-ERROR.
  
        FOR EACH b-eb
            WHERE b-eb.company EQ est.company
            AND b-eb.est-no  EQ est.est-no
            AND b-eb.form-no EQ INT(est-op.s-num:SCREEN-VALUE )
            NO-LOCK
            BREAK BY b-eb.blank-no:

            ll-1-blank = FIRST(b-eb.blank-no) AND LAST(b-eb.blank-no).

            LEAVE.
        END.

        IF ll-1-blank AND NOT (avail(mach) AND LOOKUP(mach.p-type, "A,P") GT 0) THEN 
        DO:
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.b-num Dialog-Frame
ON LEAVE OF est-op.b-num IN FRAME Dialog-Frame /* Blank# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-b-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        DISABLE TRIGGERS FOR LOAD OF est-op .

        IF lv-item-recid NE ? THEN 
        DO:

            FIND FIRST est-op EXCLUSIVE-LOCK
                WHERE RECID(est-op) EQ lv-item-recid  NO-ERROR.
  
            IF AVAILABLE est-op THEN DELETE est-op .
        END.

        IF AVAIL est-op THEN
            ip-rowid = ROWID(est-op).
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
        DEFINE VARIABLE j        AS INTEGER NO-UNDO.
        DEFINE VARIABLE v-outw   LIKE xef.n-out NO-UNDO.
        DEFINE VARIABLE v-outl   LIKE xef.n-out-l NO-UNDO.
        DEFINE VARIABLE v-rate   LIKE est-op.op-rate NO-UNDO.
        DEFINE VARIABLE v-qty    AS DECIMAL NO-UNDO.
        DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
        DEFINE VARIABLE op-error AS LOGICAL NO-UNDO.
        DEFINE BUFFER bf-est-op FOR est-op.
        DEFINE VARIABLE v-recid AS RECID NO-UNDO.
  
 
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
  
        DO WITH FRAME {&FRAME-NAME}:
            RUN valid-s-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-b-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-mach NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-op-pass NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-att-type (est-op.att-type[1]:HANDLE ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-att-type (est-op.att-type[2]:HANDLE ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-att-type (est-op.att-type[3]:HANDLE ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.

        ll-import-all = NO.
        IF ll-import-selected THEN 
            DO WITH FRAME {&FRAME-NAME}:
                MESSAGE
                    "NO = Import Standards for Only Machine Imported?" SKIP
                    "YES = Import Standards for All Machines on Routing?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO TITLE "Import Standards"
                UPDATE ll-import-all.
            END. /* with frame */

  
        DO TRANSACTION:
            FIND CURRENT est-op EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        fil_id = RECID(est-op).  /* for sub-program */
         
  
        /* Code placed here will execute AFTER standard behavior.    */
        FIND xest WHERE RECID(xest) EQ RECID(est).

        FIND FIRST mach
            {sys/look/machW.i}
            AND mach.m-code EQ est-op.m-code
        NO-LOCK NO-ERROR.

        FIND FIRST xef
            WHERE xef.company EQ est-op.company
            AND xef.est-no  EQ est-op.est-no
            AND xef.form-no EQ est-op.s-num
            NO-LOCK NO-ERROR.

        RELEASE xeb.
        IF AVAILABLE xef THEN
            FIND FIRST xeb
                WHERE xeb.company   EQ xef.company
                AND xeb.est-no    EQ xef.est-no
                AND xeb.form-no   EQ xef.form-no
                AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
                NO-LOCK NO-ERROR.

        ASSIGN
            est-op.d-seq  = mach.d-seq
            est-op.dept   = mach.dept[1]
            est-op.op-sb  = mach.p-type NE "B"
            est-op.m-code = mach.m-code
            est-op.m-dscr = mach.m-dscr.

        IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN 
        DO:
            est-op.op-crew[1] = mach.mr-crusiz.
            RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "M R",
                INPUT-OUTPUT est-op.op-crew[1]).
        END.

        IF est-op.op-crew[2] EQ 0 OR ll-import-selected THEN 
        DO:
            est-op.op-crew[2] = mach.run-crusiz.
            RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "RUN",
                INPUT-OUTPUT est-op.op-crew[2]).
        END.

        ASSIGN
            est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                       mach.mr-varoh  + mach.mr-fixoh
            est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                       mach.run-varoh + mach.run-fixoh.
         
        IF ll-import-selected THEN est-op.op-spoil = mach.run-spoil.
        IF LOOKUP(mach.p-type, "P,A") EQ 0 THEN 
        DO:
            /* Allow override for types P and A (task 09281204) */
            IF mach.p-type NE "B" THEN
                est-op.b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.
         
            ELSE
                IF est-op.b-num EQ 0 THEN est-op.b-num = 1.

        END.
        ELSE 
        DO:
            /* If adding a P or A, make it the last machine */
            IF ll-add-record THEN 
            DO:
                FOR EACH bf-est-op WHERE bf-est-op.company EQ est-op.company
                    AND bf-est-op.est-no  EQ est-op.est-no
                    NO-LOCK
                    BY bf-est-op.s-num DESCENDING.
                    LEAVE.
                END.
                IF AVAILABLE bf-est-op THEN 
                DO:
                    ASSIGN 
                        est-op.s-num = bf-est-op.s-num 
                        est-op.b-num = 1.
                END.
            END.      
        END.

        RUN is-it-foam.
     
        IF NOT CAN-DO(lv-n-out-depts,lv-dept)                 AND
            (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam) THEN est-op.n-out = 0.
    
        FOR EACH xop
            WHERE xop.company EQ est-op.company
            AND xop.est-no  EQ est-op.est-no
            AND xop.line    LT 500
            AND (NOT ll-foam OR NOT CAN-DO(lv-foam-depts,xop.dept))
            BREAK BY xop.qty
            BY xop.s-num
            BY xop.b-num
            BY xop.dept
            BY xop.line:
            
            IF FIRST-OF(xop.dept) THEN j = 0.
    
            ASSIGN
                j           = j + 1
                xop.op-pass = j.
        END.
  
        j = 0.
        FOR EACH xop
            WHERE xop.company EQ est-op.company
            AND xop.est-no  EQ est-op.est-no
            AND xop.line    LT 500
            BY xop.qty
            BY xop.s-num
            BY xop.b-num
            BY xop.d-seq
            BY xop.op-pass
            BY xop.rec_key:
      
            {sys/inc/outstrPL.i xop share}  
            ASSIGN
                j        = j + 1
                xop.line = j.
     
            IF AVAILABLE reftable THEN reftable.loc = STRING(xop.line,"9999999999"). 

        END.

        IF NOT xef.op-lock AND NOT ll-foam THEN 
        DO:
            v-outw = xef.n-out.    
            IF v-outw GT 1 THEN
                FOR EACH xop
                    WHERE xop.company EQ est-op.company
                    AND xop.est-no  EQ est-op.est-no
                    AND xop.qty     EQ est-op.qty
                    AND xop.s-num   EQ est-op.s-num
                    AND lookup(xop.dept,lv-n-out-depts) GT 0
                    AND xop.line    LT 500
                    NO-LOCK BY xop.d-seq BY xop.line:
        
                    v-outw = v-outw - xop.n-out.  
                    IF v-outw LE 0 THEN 
                    DO:
                        v-recid = RECID(xop).
                        LEAVE.
                    END.
                END.
     
            v-outl = xef.n-out-l.    
            IF v-outl GT 1 THEN
                FOR EACH xop
                    WHERE xop.company EQ est-op.company
                    AND xop.est-no  EQ est-op.est-no
                    AND xop.qty     EQ est-op.qty
                    AND xop.s-num   EQ est-op.s-num
                    AND lookup(xop.dept,lv-n-out-depts) GT 0
                    AND xop.line    LT 500
                    NO-LOCK BY xop.d-seq DESCENDING BY xop.line DESCENDING:
         
                    IF RECID(xop) EQ v-recid THEN LEAVE.       
                    v-outl = v-outl - xop.n-out.      
                    IF v-outl LE 0 THEN LEAVE.
                END.
     
            IF v-outw + v-outl LT 0 THEN 
            DO ON ENDKEY UNDO, RETRY:
                MESSAGE "Number Out for 'CR or RC' machine passes do not match layout..."
                    VIEW-AS ALERT-BOX.
            /*RETURN ERROR.*/
            END.
        END.    
  
        ASSIGN
            fil_id  = RECID(est-op)
            v-recid = fil_id.

        FOR EACH ef 
            WHERE ef.company EQ est-op.company
            AND ef.est-no  EQ est-op.est-no
            NO-LOCK:
            RUN set-lock (ef.form-no, NOT ll-import-selected).
        END.

        RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

        FOR EACH ef 
            WHERE ef.company EQ est-op.company
            AND ef.est-no  EQ est-op.est-no
            NO-LOCK:
            RUN set-lock (ef.form-no, YES).
        END.
        ASSIGN  
            fil_id        = v-recid
            ll-add-record = NO.
        RUN release-shared-buffers.
    

        ip-rowid = ROWID(est-op).

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.fountains
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.fountains Dialog-Frame
ON ENTRY OF est-op.fountains IN FRAME Dialog-Frame /* Fountain Changes */
DO:
        DEFINE VARIABLE ll AS LOG INIT YES NO-UNDO.


        IF lv-dept EQ "PR" THEN
            RUN first-of-mach (est-op.m-code:SCREEN-VALUE ,
                OUTPUT ll).

        IF ll THEN 
        DO WITH FRAME {&FRAME-NAME}:
            APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.fountains Dialog-Frame
ON LEAVE OF est-op.fountains IN FRAME Dialog-Frame /* Fountain Changes */
DO:
    /*IF LASTKEY NE -1 THEN DO:
      IF ll-import-stds THEN RUN get-stds.
    END.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-code Dialog-Frame
ON ENTRY OF est-op.m-code IN FRAME Dialog-Frame /* Machine */
DO:
        IF v-estopmch-log = NO AND v-override-mode THEN
        DO:
            APPLY "tab" TO est-op.m-code .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-code Dialog-Frame
ON LEAVE OF est-op.m-code IN FRAME Dialog-Frame /* Machine */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-mach NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            
            IF ll-import-stds AND NOT CAN-DO(lv-n-out-depts,lv-dept) THEN 
            DO:
                IF lv-dept EQ "PR" THEN
                    APPLY "entry" TO est-op.plates .
                ELSE
                    APPLY "entry" TO est-op.att-type[1] .
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-code Dialog-Frame
ON VALUE-CHANGED OF est-op.m-code IN FRAME Dialog-Frame /* Machine */
DO:
        DEFINE VARIABLE li AS INTEGER NO-UNDO.


        FIND mach
            {sys/look/machW.i}
            AND mach.m-code EQ {&self-name}:SCREEN-VALUE 
        NO-LOCK NO-ERROR.
        IF AVAILABLE MACH THEN 
        DO:
            ASSIGN
                {&self-name}:SCREEN-VALUE  = CAPS(mach.m-code)
                est-op.m-dscr:SCREEN-VALUE = mach.m-dscr
                lv-dept                    = mach.dept[1].

            DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE ):
                APPLY "cursor-right" TO {&self-name} .
            END.
 
            IF mach.p-type EQ "B"                                           AND
                INT(est-op.b-num:SCREEN-VALUE ) EQ 0 THEN
                est-op.b-num:SCREEN-VALUE  = "1".

            IF mach.p-type NE "B" AND NOT LOOKUP(mach.p-type, "P,A") GT 0 THEN
                est-op.b-num:SCREEN-VALUE  = "0".
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.m-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-dscr Dialog-Frame
ON ENTRY OF est-op.m-dscr IN FRAME Dialog-Frame /* Desc */
DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.n-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n-out Dialog-Frame
ON ENTRY OF est-op.n-out IN FRAME Dialog-Frame /* Out. */
DO:
        RUN is-it-foam.

        IF NOT CAN-DO(lv-n-out-depts,lv-dept)                  AND
            (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam)  THEN 
        DO:
            APPLY "tab" TO SELF.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n-out Dialog-Frame
ON LEAVE OF est-op.n-out IN FRAME Dialog-Frame /* Out. */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-mach NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            IF ll-import-stds AND CAN-DO(lv-n-out-depts,lv-dept) THEN 
            DO:
                IF lv-dept EQ "PR" THEN
                    APPLY "entry" TO est-op.plates .
                ELSE
                    APPLY "entry" TO est-op.att-type[1] .
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.op-pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.op-pass Dialog-Frame
ON ENTRY OF est-op.op-pass IN FRAME Dialog-Frame /* Pass#. */
DO:
        RUN is-it-foam.

        IF NOT ll-foam OR NOT CAN-DO(lv-foam-depts,lv-dept) THEN 
        DO:
            APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.op-pass Dialog-Frame
ON LEAVE OF est-op.op-pass IN FRAME Dialog-Frame /* Pass#. */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-op-pass NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.plates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.plates Dialog-Frame
ON ENTRY OF est-op.plates IN FRAME Dialog-Frame /* Plate changes */
DO:
        DEFINE VARIABLE ll AS LOG INIT YES NO-UNDO.


        IF lv-dept EQ "PR" THEN
            RUN first-of-mach (est-op.m-code:SCREEN-VALUE ,
                OUTPUT ll).

        IF ll THEN 
        DO WITH FRAME {&FRAME-NAME}:
            APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.plates Dialog-Frame
ON LEAVE OF est-op.plates IN FRAME Dialog-Frame /* Plate changes */
DO:
    /*IF LASTKEY NE -1 THEN DO:
      IF ll-import-stds AND lv-dept NE "PR" THEN RUN get-stds.
    END.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.s-num Dialog-Frame
ON ENTRY OF est-op.s-num IN FRAME Dialog-Frame /* Sheet # */
DO:
        IF est.est-type EQ 5 THEN 
        DO:
            APPLY "tab" TO SELF.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.s-num Dialog-Frame
ON LEAVE OF est-op.s-num IN FRAME Dialog-Frame /* Sheet # */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.spare-char-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.spare-char-1 Dialog-Frame
ON ENTRY OF est-op.spare-char-1 IN FRAME Dialog-Frame /* Feed */
DO:
    
    /*DO WITH FRAME {&FRAME-NAME}:
      APPLY "tab" TO {&self-name} .
      RETURN NO-APPLY.
    END.*/
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.spare-char-1 Dialog-Frame
ON LEAVE OF est-op.spare-char-1 IN FRAME Dialog-Frame /* Feed */
DO:
    
        DO WITH FRAME {&FRAME-NAME}:
            IF est-op.spare-char-1:SCREEN-VALUE  NE "" AND 
                est-op.spare-char-1:SCREEN-VALUE  NE "R" THEN 
            DO:
                APPLY "Entry" TO est-op.spare-char-1 .
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND FIRST est NO-LOCK 
        WHERE est.company EQ cocode 
        AND RECID(est) EQ ip-recid2 NO-ERROR .

    FIND FIRST est-qty NO-LOCK 
        WHERE est-qty.company EQ cocode 
        AND RECID(est-qty) EQ ip-recid3 NO-ERROR .

    RUN pGetQty.

    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.


    IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
       
    END.
    ELSE FIND est-op NO-LOCK WHERE RECID(est-op) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        IF ip-type EQ "Import"  THEN
            RUN set-import-stds ("update", YES).
        ELSE IF ip-type EQ "update"  THEN
                RUN set-import-stds ("update", NO).

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        /*IF ip-type EQ "update" THEN DISABLE est-op.s-num est-op.b-num.*/

        IF ip-type EQ "add"  OR ip-type EQ "copy" THEN 
        DO:
            APPLY "entry" TO est-op.s-num  .
        END.
    /*est-prep.s-no:HIDDEN IN FRAME {&FRAME-NAME}  = TRUE .*/
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
   
    i = 1.
    FOR EACH xop NO-LOCK
        WHERE xop.company EQ est-qty.company
        AND xop.est-no  EQ est-qty.est-no
        AND xop.line    LT 500
        BY xop.line DESCENDING:
        i = xop.line + 1.
        LEAVE.
    END.
    
    DO WITH FRAME {&FRAME-NAME}:

      
        CREATE est-op.
        ASSIGN 
            lv-item-recid = RECID(est-op).
        ll-new-record = YES.

        ASSIGN
            est-op.company = est.company
            est-op.est-no  = est.est-no
            est-op.auto    = FALSE
            est-op.line    = i
            est-op.s-num   = 1
            est-op.b-num   = IF est.est-type EQ 5 THEN 1 ELSE 0
            est-op.op-pass = 1
            est-op.qty     = IF est.est-type NE 8 THEN est-qty.eqty ELSE lv-eqty.

        FIND CURRENT est-op NO-LOCK NO-ERROR.
    END. /* avail oe-relh */

    IF ip-type = "AddStd" THEN
        RUN set-import-stds ("add", NO).
    ELSE IF ip-type = "Add" THEN
            RUN set-import-stds ("add", YES).


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    IF AVAILABLE est-op  THEN 
    DO:
        
        DISPLAY est-op.s-num est-op.b-num 
            est-op.m-code est-op.m-dscr est-op.op-pass est-op.n-out est-op.op-mr 
            est-op.op-waste est-op.op-speed est-op.op-spoil est-op.op-crew[1] 
            est-op.op-crew[2] est-op.op-rate[1] est-op.op-rate[2] est-op.plates 
            est-op.fountains est-op.att-type[1] est-op.att-qty[1] est-op.att-type[2] 
            est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] est-op.spare-char-1 
            est-op.n_out_div 
            WITH FRAME Dialog-Frame.
    END.


    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.


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
  IF AVAILABLE est-op THEN 
    DISPLAY est-op.isLocked est-op.s-num est-op.b-num est-op.m-code est-op.m-dscr 
          est-op.op-pass est-op.n-out est-op.op-mr est-op.op-waste 
          est-op.op-speed est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] 
          est-op.op-rate[1] est-op.op-rate[2] est-op.plates est-op.fountains 
          est-op.att-type[1] est-op.att-qty[1] est-op.att-type[2] 
          est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] 
          est-op.spare-char-1 est-op.n_out_div 
      WITH FRAME Dialog-Frame.
  ENABLE est-op.isLocked est-op.s-num est-op.b-num est-op.m-code est-op.m-dscr 
         est-op.op-pass est-op.n-out est-op.op-mr est-op.op-waste 
         est-op.op-speed est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] 
         est-op.op-rate[1] est-op.op-rate[2] est-op.plates est-op.fountains 
         est-op.att-type[1] est-op.att-qty[1] est-op.att-type[2] 
         est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] 
         est-op.spare-char-1 est-op.n_out_div Btn_OK Btn_Done Btn_Cancel 
         RECT-21 RECT-38 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-mach-attach Dialog-Frame 
PROCEDURE find-mach-attach :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER io-mach-attach FOR mach-attach.

    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST io-mach-attach NO-LOCK
            WHERE io-mach-attach.company  EQ cocode
            AND io-mach-attach.m-code   EQ est-op.m-code:SCREEN-VALUE 
            AND io-mach-attach.att-type EQ ip-focus:SCREEN-VALUE
            NO-ERROR.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-of-mach Dialog-Frame 
PROCEDURE first-of-mach :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-m-code LIKE est-op.m-code NO-UNDO.
    DEFINE OUTPUT PARAMETER op-first  AS   LOG           NO-UNDO.
  
    DEFINE BUFFER b-est-op FOR est-op.


    op-first = AVAILABLE est-op AND
        NOT CAN-FIND(FIRST b-est-op
        WHERE b-est-op.company EQ est-op.company
        AND b-est-op.est-no  EQ est-op.est-no
        AND b-est-op.qty     EQ est-op.qty
        AND b-est-op.m-code  EQ ip-m-code
        AND b-est-op.line    LT est-op.line).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-n-out Dialog-Frame 
PROCEDURE get-n-out :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-stds Dialog-Frame 
PROCEDURE get-stds :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE chr-handle AS CHARACTER NO-UNDO.


    ll-import-stds = NO.

    DO WITH FRAME {&FRAME-NAME}:
        APPLY "choose" TO btn_ok.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportRouting Dialog-Frame 
PROCEDURE ImportRouting :
/*------------------------------------------------------------------------------
      Purpose:  Assigns machine data to routing (est-op)   
      Parameters:  estop Row ID
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-est-op FOR est-op.
    DEFINE BUFFER bf-mach   FOR mach.

    FOR EACH bf-est-op
        WHERE bf-est-op.company EQ est-op.company
        AND bf-est-op.est-no EQ est-op.est-no
        AND bf-est-op.LINE LT 500
        AND ROWID(bf-est-op) NE ROWID(est-op)
        EXCLUSIVE-LOCK,
        FIRST bf-mach NO-LOCK
        WHERE bf-mach.company EQ bf-est-op.company
        AND bf-mach.m-code EQ bf-est-op.m-code:
    
        ASSIGN
            bf-est-op.d-seq      = bf-mach.d-seq
            bf-est-op.dept       = bf-mach.dept[1]
            bf-est-op.op-sb      = bf-mach.p-type NE "B"
            bf-est-op.m-code     = bf-mach.m-code
            bf-est-op.m-dscr     = bf-mach.m-dscr
            bf-est-op.op-rate[1] = bf-mach.mr-trate
            bf-est-op.op-rate[2] = bf-mach.run-trate
            bf-est-op.op-crew[1] = bf-mach.mr-crusiz
            bf-est-op.op-crew[2] = bf-mach.run-crusiz
            bf-est-op.op-spoil   = bf-mach.run-spoil
            .


    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-it-foam Dialog-Frame 
PROCEDURE is-it-foam :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-ef FOR ef.

  
    DO WITH FRAME {&FRAME-NAME}:
        ll-foam = NO.
        FIND FIRST b-ef
            WHERE b-ef.company EQ est.company
            AND b-ef.est-no  EQ est.est-no
            AND b-ef.form-no EQ INT(est-op.s-num:SCREEN-VALUE )
            NO-LOCK NO-ERROR.
        IF AVAILABLE b-ef THEN RUN cec/isitfoam.p (ROWID(b-ef), OUTPUT ll-foam).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-att-type Dialog-Frame 
PROCEDURE new-att-type :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        li = ip-focus:INDEX.

        IF ip-focus:SCREEN-VALUE NE "" THEN 
        DO:
            RUN find-mach-attach (BUFFER mach-attach, ip-focus).
            FIND FIRST eb WHERE eb.company = est-op.company
                AND eb.est-no = est-op.est-no
                /*AND eb.eqty = est-op.eqty*/
                AND eb.form-no = est-op.s-num
                AND eb.blank-no = est-op.b-num NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN
                FIND FIRST style WHERE style.company = eb.company
                    AND style.style = eb.style
                    AND style.flute = eb.flute
                    AND style.test = eb.test 
                    AND (style.TYPE = "p" OR style.TYPE = "R") 
                    NO-LOCK NO-ERROR.
            IF NOT AVAILABLE style THEN
                FIND FIRST style WHERE style.company = eb.company
                    AND style.style = eb.style
                    AND style.flute = ""
                    AND style.test = ""
                    AND (style.TYPE = "p" OR style.TYPE = "R") 
                    NO-LOCK NO-ERROR.
            IF AVAILABLE mach-attach THEN
                IF li EQ 1 THEN
                    est-op.att-qty[1]:SCREEN-VALUE = 
                        IF mach-attach.qty > 0 THEN STRING(mach-attach.qty)
                        ELSE IF AVAILABLE style THEN STRING(style.dim-df) ELSE "0".
                ELSE
                    IF li EQ 2 THEN
                        est-op.att-qty[2]:SCREEN-VALUE = 
                            IF mach-attach.qty > 0 THEN STRING(mach-attach.qty)
                            ELSE IF AVAILABLE style THEN STRING(style.dim-df) ELSE "0".
                    ELSE
                        est-op.att-qty[3]:SCREEN-VALUE = 
                            IF mach-attach.qty > 0 THEN STRING(mach-attach.qty)
                            ELSE IF AVAILABLE style THEN STRING(style.dim-df) ELSE "0".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetQty Dialog-Frame 
PROCEDURE pGetQty :
/*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    lv-eqty = 0.

    IF AVAILABLE est AND AVAILABLE est-qty THEN
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers Dialog-Frame 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    RELEASE xest.
    RELEASE xef.
    RELEASE xeb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-import-stds Dialog-Frame 
PROCEDURE set-import-stds :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-add-update  AS   CHARACTER           NO-UNDO.
    DEFINE INPUT PARAMETER ip-import-stds LIKE ll-import-stds NO-UNDO.

    IF ip-add-update = "Update" AND ip-import-stds = NO THEN
        v-override-mode = YES.
    ELSE
        v-override-mode = NO.

    IF ip-import-stds THEN
        FOR EACH xop NO-LOCK
            WHERE xop.company EQ est.company
            AND xop.est-no  EQ est.est-no
            AND xop.line    LT 500
            AND (NOT AVAILABLE est-op OR ROWID(xop) NE ROWID(est-op)),
            FIRST mach NO-LOCK
            {sys/look/machW.i}
        AND mach.m-code EQ xop.m-code:
   
    IF mach.obsolete THEN 
    DO: 
        MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is Inactive, please replace or standards will not be imported"
            VIEW-AS ALERT-BOX ERROR.
        ip-import-stds = NO.
        LEAVE.
    END.
END.

ASSIGN
    ll-import-stds     = ip-import-stds
    ll-import-selected = ip-import-stds.
IF AVAILABLE est-op AND est-op.isLocked THEN 
    ASSIGN
        ll-import-stds = NO 
        ll-import-selected = NO 
        .
        
IF ip-add-update EQ "update" THEN 
DO WITH FRAME {&frame-name}:
    APPLY "entry" TO est-op.s-num .
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-att-type Dialog-Frame 
PROCEDURE valid-att-type :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

    DEFINE VARIABLE lv-msg  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-type LIKE est-op.att-type NO-UNDO.
    DEFINE VARIABLE li      AS INTEGER   NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF ip-focus:SCREEN-VALUE NE "" THEN 
        DO:
            ASSIGN
                lv-type[1] = est-op.att-type[1]:SCREEN-VALUE 
                lv-type[2] = est-op.att-type[2]:SCREEN-VALUE 
                lv-type[3] = est-op.att-type[3]:SCREEN-VALUE .

            RUN find-mach-attach (BUFFER mach-attach, ip-focus).
    
            IF NOT AVAILABLE mach-attach THEN
                lv-msg = "Invalid Attachment Type for Machine, try help".

            ELSE
                ip-focus:SCREEN-VALUE = mach-attach.att-type.

            IF lv-msg EQ "" THEN
            DO li = 1 TO EXTENT(lv-type):
                IF ip-focus:INDEX NE li                 AND
                    ip-focus:SCREEN-VALUE EQ lv-type[li] THEN 
                DO:
                    lv-msg = TRIM(ip-focus:LABEL) +
                        " may not be the same as one of the other two".
                    LEAVE.
                END.
            END.
        END.

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-b-num Dialog-Frame 
PROCEDURE valid-b-num :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&frame-name}:
        APPLY "value-changed" TO est-op.m-code .

        FIND FIRST mach
            {sys/look/machW.i}
            AND mach.m-code EQ est-op.m-code:SCREEN-VALUE 
        NO-LOCK NO-ERROR.

        IF ((AVAILABLE mach AND mach.p-type EQ "B") OR
            int(est-op.b-num:screen-value ) NE 0) AND
            NOT CAN-FIND(FIRST eb
            WHERE eb.company  EQ est.company
            AND eb.est-no   EQ est.est-no
            AND eb.form-no  EQ int(est-op.s-num:screen-value )
            AND eb.blank-no EQ int(est-op.b-num:screen-value ))
            THEN 
        DO:
            MESSAGE "Must enter a valid Blank#" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO est-op.b-num .
            RETURN ERROR.
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mach Dialog-Frame 
PROCEDURE valid-mach :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE chr-handle  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-tmp      AS cha       NO-UNDO.
    DEFINE VARIABLE v-run       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-on-f      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE sh-dep      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE li-aqueous  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMachType   AS CHARACTER NO-UNDO .
    DEFINE VARIABLE v-msgreturn AS INTEGER   NO-UNDO .
    DEFINE VARIABLE dNshLen     AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dTrimL      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dMachMaxLen AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dMachMaxWid AS DECIMAL   NO-UNDO .

    DEFINE BUFFER b-est-op FOR est-op.
    DEFINE BUFFER b-mach   FOR mach.
    DEFINE BUFFER bf-ef    FOR ef.
  

    /*  {sys/inc/cepanel.i} - deprecated with 17756*/

    RUN is-it-foam.

    DO WITH FRAME {&frame-name}:
        FIND FIRST mach
            {sys/look/machW.i}
            AND mach.m-code EQ est-op.m-code:screen-value 
        NO-LOCK NO-ERROR.

        IF NOT AVAILABLE mach THEN 
        DO:
            MESSAGE "Must enter a valid Machine Code, try help"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO est-op.m-code .
            RETURN ERROR.
        END.


        IF mach.obsolete THEN 
        DO:
            MESSAGE "Machine is Inactive, please choose a different machine"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO est-op.m-code .
            RETURN ERROR.
        END.
  
        ASSIGN
            est-op.m-code:screen-value = mach.m-code
            est-op.m-dscr:screen-value = mach.m-dscr
            lv-d-seq                   = mach.d-seq
            lv-dept                    = mach.dept[1].

        FIND FIRST xef
            WHERE xef.company EQ est.company
            AND xef.est-no  EQ est.est-no
            AND xef.form-no EQ int(est-op.s-num:screen-value )
            NO-LOCK NO-ERROR.

        FIND FIRST xeb
            WHERE xeb.company EQ est.company
            AND xeb.est-no  EQ est.est-no
            AND xeb.form-no EQ xef.form-no
            AND (xeb.blank-no EQ int(est-op.b-num:screen-value ) OR
            int(est-op.b-num:screen-value ) EQ 0)
            NO-LOCK NO-ERROR.

        FIND FIRST style
            {sys/ref/styleW.i}
            AND style.style EQ xeb.style
        NO-LOCK NO-ERROR.

        FIND xest WHERE RECID(xest) EQ recid(est) NO-LOCK.

        IF ll-import-stds THEN RUN get-n-out.

        v-on-f = int(est-op.n-out:screen-value ).

        IF v-on-f NE 0 OR NOT CAN-DO(lv-n-out-depts,lv-dept) THEN 
        DO:
            IF xef.lam-dscr EQ "R"                         /*or
         (xef.lam-dscr ne "R" and xef.xgrain eq "S")*/ THEN
                ASSIGN
                    sh-wid = xef.nsh-wid
                    sh-len = xef.nsh-len.
            ELSE
                ASSIGN
                    sh-wid = xef.nsh-len
                    sh-len = xef.nsh-wid.

            DO WHILE TRUE:
                sh-dep = xef.cal.

                IF mach.p-type EQ "B" THEN
                    ASSIGN
                        sh-len = xeb.t-wid
                        sh-wid = xeb.t-len.

                IF ll-foam THEN 
                DO:
                    sh-dep = xeb.t-dep.

                    IF mach.p-type NE "B" THEN 
                    DO:
                        {sys/inc/outstrPL.i est-op NO}
         
                        IF AVAILABLE reftable THEN
                        DO i = 1 TO 3:
                            CASE reftable.val[i]:
                                WHEN 1.0 THEN 
                                    sh-len = reftable.val[i + 3].
                                WHEN 2.0 THEN 
                                    sh-wid = reftable.val[i + 3].
                                WHEN 3.0 THEN 
                                    sh-dep = reftable.val[i + 3].
                            END CASE.
                        END.
                    END.
                END.

                ASSIGN 
                    cMachType   = IF AVAILABLE mach THEN mach.p-type ELSE "" 
                    dMachMaxLen = IF AVAILABLE mach THEN mach.max-len ELSE 0 
                    dMachMaxWid = IF AVAILABLE mach THEN mach.max-wid ELSE 0
                    qty         = lv-eqty. /*20108 - qty variable needed for mach-seq.i->mach-qty.p setting equal to selected routing qty*/

                IF lv-dept EQ "RC" THEN 
                DO:
                    xcal = sh-dep.
                    RUN cec/rc-mach.p (BUFFER mach, v-on-f, NO).
                    IF AVAILABLE mach THEN LEAVE.
                END.

                ELSE 
                DO:
                    {cec/mach-seq.i sh-len sh-wid sh-dep}
                END.
                      
                IF NOT AVAILABLE mach THEN 
                DO:
                    IF (cMachType = "S" AND ( (sh-wid < sh-len AND  (dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid) ) OR (sh-wid > sh-len AND ( dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid)  ) )) THEN 
                    DO:

                        RUN custom/d-msg-mach.w ("Warning","","Sheet size outside machine limits ","",2,"Reverse Feed Direction,OK", OUTPUT v-msgreturn).         
                        IF v-msgreturn = 1  THEN 
                        DO:
                            FIND FIRST bf-ef EXCLUSIVE-LOCK WHERE 
                                bf-ef.company EQ xef.company AND
                                RECID(bf-ef)  EQ RECID(xef) NO-ERROR.
                            IF AVAILABLE bf-ef THEN
                                ASSIGN
                                    dNshLen       = xef.nsh-len  
                                    dTrimL        = xef.trim-l
                                    bf-ef.nsh-len = xef.nsh-wid
                                    bf-ef.nsh-wid = dNshLen
                                    bf-ef.trim-l  = xef.trim-w
                                    bf-ef.trim-w  = dTrimL
                                    .
                            FIND FIRST mach
                                {sys/look/machW.i}
                                AND mach.m-code EQ est-op.m-code:screen-value 
                            NO-LOCK NO-ERROR.
                            LEAVE.
                    
                        END.
                        ELSE 
                        DO:
                            APPLY "entry" TO est-op.m-code .
                            RETURN ERROR.
                        END.

                    END.  /* cMachType = "S" */   
                    ELSE IF cMachType = "B" AND est-op.spare-char-1:screen-value  NE "R" 
                            AND ( (sh-wid < sh-len AND  (dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid) ) OR (sh-wid > sh-len AND ( dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid)  ) ) THEN 
                        DO: 
                            RUN custom/d-msg-mach.w ("Warning","","Blank size outside machine limits","",2,"Reverse Blank Feed,OK", OUTPUT v-msgreturn).        
                            IF v-msgreturn = 1  THEN 
                            DO:
                                est-op.spare-char-1:screen-value  = "R" .
                                FIND FIRST mach
                                    {sys/look/machW.i}
                                    AND mach.m-code EQ est-op.m-code:screen-value 
                                NO-LOCK NO-ERROR.
                                {cec/mach-seq.i sh-wid sh-len sh-dep}
                                IF NOT AVAILABLE mach THEN  
                                DO:
                                    MESSAGE "Blank size outside machine limits, when reversed" VIEW-AS ALERT-BOX ERROR.
                                    APPLY "entry" TO est-op.m-code .
                                    RETURN ERROR.
                                END.
                                LEAVE.
                            END.
                 
                            ELSE 
                            DO:
                                APPLY "entry" TO est-op.m-code .
                                RETURN ERROR.
                            END.

                        END.
                        ELSE IF cMachType = "B" AND est-op.spare-char-1:screen-value  EQ "R" THEN 
                            DO:
                                FIND FIRST mach
                                    {sys/look/machW.i}
                                    AND mach.m-code EQ est-op.m-code:screen-value 
                                NO-LOCK NO-ERROR.
                                {cec/mach-seq.i sh-wid sh-len sh-dep}
                                IF NOT AVAILABLE mach THEN  
                                DO:
                                    MESSAGE "Blank size outside machine limits, when reversed" VIEW-AS ALERT-BOX ERROR.
                                    APPLY "entry" TO est-op.m-code .
                                    RETURN ERROR.
                                END.
                                LEAVE.
                            END. 
                            ELSE 
                            DO:
                                MESSAGE "Estimate specifications outside machine limits" VIEW-AS ALERT-BOX ERROR.
                                APPLY "entry" TO est-op.m-code .
                                RETURN ERROR.
                            END.

         
                END.
            END.
        END.
        
        IF CAN-DO(lv-n-out-depts,lv-dept) AND v-on-f LE 0 AND 
            FOCUS:NAME  NE "m-code" THEN 
        DO:
            MESSAGE TRIM(est-op.n-out:LABEL ) +
                " must not be zero..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO est-op.n-out .
            RETURN ERROR.
        END.

        IF mach.p-type NE "B" THEN
            ASSIGN
                lv-op-sb = YES
                lv-b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.
       
        ELSE
            ASSIGN
                lv-op-sb = NO
                lv-b-num = IF int(est-op.b-num:screen-value) EQ 0 THEN 1 ELSE int(est-op.b-num:screen-value).
       
        IF NOT CAN-DO(lv-n-out-depts,lv-dept) THEN lv-n-out = 0.

        IF xeb.i-pass GT 0 THEN 
        DO:
            /* press, check ink */
            IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
                mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN 
            DO:
                FIND FIRST item {sys/look/itemivW.i}
                    AND item.i-no EQ xeb.i-code[1]
                NO-LOCK NO-ERROR.
                IF NOT AVAILABLE item AND mach.dept[2] EQ "" AND
                    mach.dept[3] EQ "" AND mach.dept[4] EQ "" 
                    THEN 
                DO:
                    MESSAGE "No Inks defined !" VIEW-AS ALERT-BOX.
                    APPLY "entry" TO est-op.m-code .
                    RETURN ERROR.                   
                END.
                ELSE IF AVAILABLE item AND item.press-type NE mach.pr-type THEN 
                    DO:
                        MESSAGE "WRONG PRESS TYPE for selected Ink!" VIEW-AS ALERT-BOX ERROR.
                        APPLY "entry" TO est-op.m-code .
                        RETURN ERROR.
                    END.
                ASSIGN
                    maxco    = 0
                    v-passes = INT(adm-new-record).

                FOR EACH b-est-op
                    WHERE b-est-op.company EQ est-op.company
                    AND b-est-op.est-no  EQ est-op.est-no
                    AND (b-est-op.eqty   EQ est-op.eqty OR est.est-type GE 7)
                    AND b-est-op.s-num   EQ INT(est-op.s-num:SCREEN-VALUE )
                    AND b-est-op.line    LT 500
                    AND (ROWID(b-est-op) NE ROWID(est-op) OR NOT adm-new-record)
                    AND CAN-FIND(FIRST b-mach
                    WHERE b-mach.company EQ b-est-op.company
                    AND b-mach.m-code  EQ b-est-op.m-code
                    AND (b-mach.dept[1] EQ "PR" OR
                    b-mach.dept[2] EQ "PR" OR
                    b-mach.dept[3] EQ "PR" OR
                    b-mach.dept[4] EQ "PR" OR
                    b-mach.dept[1] EQ "CT" OR
                    b-mach.dept[2] EQ "CT" OR
                    b-mach.dept[3] EQ "CT" OR
                    b-mach.dept[4] EQ "CT"))
                    NO-LOCK BY b-est-op.d-seq BY b-est-op.line:

                    v-passes = v-passes + 1.
                    IF ROWID(b-est-op) EQ ROWID(est-op) THEN LEAVE.
                END.

                DO i = 1 TO 10:
                    IF xeb.i-ps[i] NE v-passes THEN NEXT.
                    FIND FIRST item NO-LOCK
                        {sys/look/itemW.i}
                        AND item.i-no     EQ xeb.i-code[i]
                        AND INDEX("IV",item.mat-type) GT 0
                        AND item.ink-type NE "A"
                        NO-ERROR.
                    IF AVAILABLE item THEN maxco = maxco + 1.
                END.

                IF mach.max-color LT maxco THEN 
                DO:
                    MESSAGE "NOT ENOUGH COLORS on PRESS for selected Inks!" VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO est-op.m-code .
                    RETURN ERROR.
                END.
            END.  /* dept = "PR" */
        END.  /* x-eb.i-pass */

        /*RUN cec/mach-qty.p (ROWID(mach), ROWID(xeb), v-on-f, sh-len, OUTPUT v-run).
    
        if v-run lt mach.min-run then do:
                message "RUN QTY. too small for Machine!" view-as alert-box error.
                apply "entry" to est-op.m-code .
                return error.
        end.
        if (xest.est-qty[1] / xest.prod-runs) gt mach.max-run then do:
                message "RUN QTY. too large for Machine!" view-as alert-box error.
                apply "entry" to est-op.m-code .
                return error.
        end.
        if mach.min-cal gt xef.cal then do:
                message "BOARD CALIPER too small for Machine!" view-as alert-box.
                apply "entry" to est-op.m-code .
                return error.
        end.
        if mach.max-cal lt xef.cal then do:
                message "BOARD CALIPER too large for Machine!" view-as alert-box.
                apply "entry" to est-op.m-code .
                return error.
        end.*/
    
        ll-machine-modified = est-op.m-code:MODIFIED .
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-op-pass Dialog-Frame 
PROCEDURE valid-op-pass :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-est-op FOR est-op.


    DO WITH FRAME {&FRAME-NAME}:
        RUN is-it-foam.

        IF ll-foam AND CAN-DO(lv-foam-depts,lv-dept) THEN 
        DO:
            /*IF CAN-FIND(FIRST b-est-op
                        WHERE b-est-op.company EQ est.company
                          AND b-est-op.est-no  EQ est.est-no
                          AND b-est-op.s-num   EQ INT(est-op.s-num:SCREEN-VALUE )
                          AND b-est-op.op-pass EQ INT(est-op.op-pass:SCREEN-VALUE )
                          AND ROWID(b-est-op)  NE ROWID(est-op))
            THEN DO:
              MESSAGE TRIM(est-op.op-pass:LABEL ) +
                      " already exists for this Form#..."
                  VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" to est-op.op-pass .
              RETURN ERROR.
            END.*/

            IF INT(est-op.op-pass:SCREEN-VALUE ) NE 1 AND
                NOT CAN-FIND(FIRST ef-nsh
                WHERE ef-nsh.company EQ est.company
                AND ef-nsh.est-no  EQ est.est-no
                AND ef-nsh.form-no EQ INT(est-op.s-num:SCREEN-VALUE )
                AND ef-nsh.pass-no EQ INT(est-op.op-pass:SCREEN-VALUE ))
                THEN 
            DO:
                MESSAGE "Net Sheet does not exist for this "                +
                    TRIM(est-op.op-pass:LABEL ) +
                    "..."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO est-op.op-pass .
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num Dialog-Frame 
PROCEDURE valid-s-num :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&frame-name}:
        IF NOT CAN-FIND(FIRST ef
            WHERE ef.company EQ est.company
            AND ef.est-no  EQ est.est-no
            AND ef.form-no EQ int(est-op.s-num:screen-value ))
            THEN 
        DO:
            MESSAGE "Must enter a valid Form#" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO est-op.s-num .
            RETURN ERROR.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

