&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi       PROGRESS
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

{methods/defines/globdefs.i}


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

DEFINE BUFFER xop     FOR est-op.
DEFINE BUFFER op-lock FOR reftable.
DEFINE BUFFER bf-rell FOR oe-rell.
DEFINE BUFFER bf-ordl FOR oe-ordl.

DEFINE NEW SHARED VARIABLE xcal      AS de      NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-wid    AS de      NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-len    AS de      NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id    AS RECID   NO-UNDO.
DEFINE NEW SHARED VARIABLE qty       AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-chk-qty AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE v-sht-qty AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE v-rc-seq  AS INTEGER INITIAL 9999 NO-UNDO.

{ce/mach-ink.i NEW}

DEFINE VARIABLE ll-import-stds      AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-d-seq            LIKE est-op.d-seq NO-UNDO.
DEFINE VARIABLE lv-dept             LIKE est-op.dept NO-UNDO.
DEFINE VARIABLE lv-op-sb            LIKE est-op.op-sb NO-UNDO.
DEFINE VARIABLE lv-b-num            LIKE est-op.b-num NO-UNDO.
DEFINE VARIABLE lv-n-out            LIKE est-op.n-out NO-UNDO.
DEFINE VARIABLE maxco               AS INTEGER NO-UNDO.
DEFINE VARIABLE v-passes            AS INTEGER NO-UNDO.
DEFINE VARIABLE ll-machine-modified AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-import-selected  AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-import-all       AS LOGICAL NO-UNDO.
DEFINE VARIABLE li-cnt              AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-eqty             LIKE est-qty.eqty NO-UNDO.
DEFINE VARIABLE v-override-mode     AS LOGICAL NO-UNDO.

DO WITH TRANSACTION:
    {sys\inc\ceroute#out.i}
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
&Scoped-define INTERNAL-TABLES est-op est

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame est-op.s-num est-op.b-num ~
est-op.m-code est-op.m-dscr est-op.n-out est-op.op-mr est-op.op-waste ~
est-op.op-speed est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] ~
est-op.op-rate[1] est-op.op-rate[2] est-op.num-col est-op.num-coat ~
est-op.plates est-op.fountains est-op.n_out_div 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame est-op.s-num ~
est-op.b-num est-op.m-code est-op.m-dscr est-op.n-out est-op.op-mr ~
est-op.op-waste est-op.op-speed est-op.op-spoil est-op.op-crew[1] ~
est-op.op-crew[2]  est-op.num-col ~
est-op.num-coat est-op.plates est-op.fountains est-op.n_out_div 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame est-op
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame est-op
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH est-op ~
      WHERE ASI.est-op.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH est-op ~
      WHERE ASI.est-op.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame est-op est
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame est-op
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame est


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS est-op.s-num est-op.b-num est-op.m-code ~
est-op.m-dscr est-op.n-out est-op.op-mr est-op.op-waste est-op.op-speed ~
est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2]  ~
 est-op.num-col est-op.num-coat est-op.plates ~
est-op.fountains est-op.n_out_div 
&Scoped-define ENABLED-TABLES est-op
&Scoped-define FIRST-ENABLED-TABLE est-op
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS est-op.s-num est-op.b-num est-op.m-code ~
est-op.m-dscr est-op.n-out est-op.op-mr est-op.op-waste est-op.op-speed ~
est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] est-op.op-rate[1] ~
est-op.op-rate[2] est-op.num-col est-op.num-coat est-op.plates ~
est-op.fountains est-op.n_out_div 
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
    LABEL "Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    LABEL "&Done" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    LABEL "&Save" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 127 BY 3.57.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 127 BY 10.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    est-op, 
    est SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    est-op.s-num AT ROW 1.91 COL 28.4 COLON-ALIGNED
    LABEL "Sheet #" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 14.6 BY 1
    est-op.b-num AT ROW 1.91 COL 63.4 COLON-ALIGNED
    LABEL "Blank#" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 13.4 BY 1
    est-op.m-code AT ROW 3.14 COL 28.4 COLON-ALIGNED
    LABEL "Machine" FORMAT "x(6)"
    VIEW-AS FILL-IN 
    SIZE 14.6 BY 1
    est-op.m-dscr AT ROW 3.14 COL 63.2 COLON-ALIGNED
    LABEL "Desc" FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1
    est-op.n-out AT ROW 4.38 COL 28.4 COLON-ALIGNED
    LABEL "Out." FORMAT ">>>9"
    VIEW-AS FILL-IN 
    SIZE 14.6 BY 1
    est-op.op-mr AT ROW 4.38 COL 63.2 COLON-ALIGNED
    LABEL "MR-Hrs" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.op-waste AT ROW 4.38 COL 103 COLON-ALIGNED
    LABEL "Waste" FORMAT ">>>>>9"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.op-speed AT ROW 5.62 COL 28.4 COLON-ALIGNED
    LABEL "Speed" FORMAT ">>>>9"
    VIEW-AS FILL-IN 
    SIZE 14.6 BY 1
    est-op.op-spoil AT ROW 5.62 COL 63.2 COLON-ALIGNED
    LABEL "Spoil%" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.op-crew[1] AT ROW 5.62 COL 103 COLON-ALIGNED
    LABEL "MRCrew" FORMAT "9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.op-crew[2] AT ROW 6.91 COL 28.4 COLON-ALIGNED
    LABEL "RunCrew" FORMAT "9.99"
    VIEW-AS FILL-IN 
    SIZE 14.6 BY 1
    est-op.op-rate[1] AT ROW 6.91 COL 63.2 COLON-ALIGNED
    LABEL "MRate" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.op-rate[2] AT ROW 6.91 COL 103 COLON-ALIGNED
    LABEL "RRate" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.num-col AT ROW 8.1 COL 28.4 COLON-ALIGNED
    LABEL "Inks" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 14.6 BY 1
    est-op.num-coat AT ROW 8.1 COL 63.2 COLON-ALIGNED
    LABEL "Varnish" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.plates AT ROW 8.1 COL 103 COLON-ALIGNED
    LABEL "Plate changes" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    est-op.fountains AT ROW 9.38 COL 28.4 COLON-ALIGNED
    LABEL "Fountain Changes" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 14.6 BY 1
    est-op.n_out_div AT ROW 9.38 COL 63.2 COLON-ALIGNED
    LABEL "Run Qty Divisor" FORMAT "->>,>>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    Btn_OK AT ROW 13.29 COL 37
    Btn_Done AT ROW 13.24 COL 57
    Btn_Cancel AT ROW 13.24 COL 77.2
    RECT-21 AT ROW 11.71 COL 1
    RECT-38 AT ROW 1 COL 1
    SPACE(1.39) SKIP(4.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FONT 6
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
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

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
/* SETTINGS FOR FILL-IN est-op.num-coat IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.num-col IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.n_out_div IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-crew[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-crew[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-mr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-rate[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT no-enable                                                */
/* SETTINGS FOR FILL-IN est-op.op-rate[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT  no-enable                                                */
/* SETTINGS FOR FILL-IN est-op.op-speed IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-spoil IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.op-waste IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.plates IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-op.s-num IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.est-op,asi.est "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.est-op.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Estimate Prep Item Update */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Estimate Prep Item Update */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lw-focus AS HANDLE    NO-UNDO.
        DEFINE VARIABLE rowidval AS ROWID     NO-UNDO.
        DEFINE BUFFER bff-eb FOR eb . 

        lw-focus = FOCUS.

        CASE lw-focus:NAME:
            WHEN "m-code" THEN 
                DO:
                    RUN windows/l-mach.w (est.company,est.loc, lw-focus:SCREEN-VALUE , OUTPUT char-val).
                    IF char-val <> "" AND lw-focus:SCREEN-VALUE  NE entry(1,char-val) THEN 
                    DO:
                        SELF:screen-value  = ENTRY(1,char-val).
                        APPLY "value-changed" TO FOCUS . 
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Prep Item Update */
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


&Scoped-define SELF-NAME est-op.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.s-num Dialog-Frame
ON ENTRY OF est-op.s-num IN FRAME Dialog-Frame
    DO:
        IF est.est-type EQ 1 THEN 
        DO:
            APPLY "tab" TO SELF.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.s-num Dialog-Frame
ON LEAVE OF est-op.s-num IN FRAME Dialog-Frame /* S */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME est-op.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.b-num Dialog-Frame
ON ENTRY OF est-op.b-num IN FRAME Dialog-Frame /* Blank# */
    DO:
        DEFINE VARIABLE ll-1-blank AS LOGICAL NO-UNDO.

        DEFINE BUFFER b-eb FOR eb.


        FOR EACH b-eb NO-LOCK
            WHERE b-eb.company EQ est-qty.company
            AND b-eb.est-no  EQ est-qty.est-no
            AND b-eb.form-no EQ INT(est-op.s-num:SCREEN-VALUE )
            BREAK BY b-eb.blank-no:

            ll-1-blank = FIRST(b-eb.blank-no) AND LAST(b-eb.blank-no).

            LEAVE.
        END.

        IF ll-1-blank THEN 
        DO:
            APPLY "entry" TO est-op.m-code .
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

            FIND est-op EXCLUSIVE-LOCK
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
        DEFINE VARIABLE v-qty    AS DECIMAL NO-UNDO.
        DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
        DEFINE VARIABLE op-error AS LOGICAL NO-UNDO.

        DEFINE VARIABLE v-recid  AS RECID   NO-UNDO.
  
 
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        RUN valid-s-num.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-b-num.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-mach.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        /*IF ll-import-stds THEN RUN get-stds.*/

        DO TRANSACTION:
            FIND CURRENT est-op EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        fil_id = RECID(est-op).  /* for sub-program */
         
  
        /* Code placed here will execute AFTER standard behavior.    */
        FIND xest WHERE RECID(xest) EQ RECID(est).
  
        FIND FIRST mach NO-LOCK 
            {sys/look/machW.i}
            AND mach.m-code EQ est-op.m-code
            NO-ERROR.

        FIND FIRST xef NO-LOCK 
            WHERE xef.company EQ est-op.company
            AND xef.est-no  EQ est-op.est-no
            AND xef.form-no EQ est-op.s-num
            NO-ERROR.
  
        ASSIGN
            est-op.d-seq      = mach.d-seq
            est-op.dept       = mach.dept[1]
            est-op.op-sb      = mach.p-type NE "B"
            est-op.m-code     = mach.m-code
            est-op.m-dscr     = mach.m-dscr
            est-op.op-rate[1] = mach.mr-trate
            est-op.op-rate[2] = mach.run-trate.

        IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN
            est-op.op-crew[1] = mach.mr-crusiz.

        IF est-op.op-crew[2] EQ 0 OR ll-import-selected  THEN
            est-op.op-crew[2] = mach.run-crusiz.
         
        IF ll-import-selected THEN 
            ASSIGN
                est-op.op-spoil = mach.run-spoil
                est-op.NUM-COL  = 0
                est-op.num-coat = 0.

        IF ll-import-all THEN
            RUN ImportRouting.

        FOR EACH xop
            WHERE xop.company EQ est-op.company
            AND xop.est-no  EQ est-op.est-no
            AND xop.line    LT 500
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
            AND xop.line   LT 500
            BY xop.qty BY xop.s-num BY xop.b-num BY xop.d-seq BY xop.op-pass:

            {sys/inc/outstrPL.i xop SHARE}
            ASSIGN
                j        = j + 1
                xop.line = j.
     
            IF AVAILABLE reftable THEN reftable.loc = STRING(xop.line,"9999999999"). 
        END.  
  
        ASSIGN
            fil_id  = RECID(est-op)
            v-recid = fil_id.

        FOR EACH ef NO-LOCK
            WHERE ef.company EQ est-qty.company
            AND ef.est-no  EQ est-qty.est-no:
            RUN set-lock (ef.form-no, NOT ll-import-selected).
        END.

        RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

        FOR EACH ef NO-LOCK
            WHERE ef.company EQ est-qty.company
            AND ef.est-no  EQ est-qty.est-no:
            RUN set-lock (ef.form-no, YES).
        END.

        fil_id = v-recid.

        RUN release-shared-buffers.
    

        ip-rowid = ROWID(est-op).

        APPLY "go" TO FRAME {&FRAME-NAME}.

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
ON HELP OF est-op.m-code IN FRAME Dialog-Frame /* Machine */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
    
       
        RUN windows/l-mach.w (est.company,est.loc, FOCUS:SCREEN-VALUE , OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value  NE entry(1,char-val) THEN 
        DO:
            SELF:screen-value  = ENTRY(1,char-val).
            APPLY "value-changed" TO SELF . 
        END.
        RETURN NO-APPLY.
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

            ll-import-all = NO.
            IF ll-import-selected THEN 
            DO WITH FRAME {&FRAME-NAME}:
                MESSAGE
                    "NO = Import Standards for Only Machine Imported?" SKIP
                    "YES = Import Standards for All Machines on Routing?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO TITLE "Import Standards"
                UPDATE ll-import-all.
            END. /* with frame */

            IF ll-import-stds AND NOT CAN-DO("RC,GU",lv-dept) THEN
                IF CAN-DO("PR,CT",lv-dept) THEN 
                DO:
                    APPLY "entry" TO est-op.plates .
                    RETURN NO-APPLY.
                END.
                ELSE RUN get-stds.
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
            AND mach.m-code BEGINS est-op.m-code:SCREEN-VALUE 
        NO-LOCK NO-ERROR.
        IF AVAILABLE MACH THEN 
        DO:
            ASSIGN
                est-op.m-code:SCREEN-VALUE = CAPS(mach.m-code)
                est-op.m-dscr:SCREEN-VALUE = mach.m-dscr.

            DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE ):
                APPLY "cursor-right" TO {&self-name} .
            END.

            IF mach.p-type EQ "B"                                           AND
                INT(est-op.b-num:SCREEN-VALUE ) EQ 0 THEN
                est-op.b-num:SCREEN-VALUE  = "1".

            IF mach.p-type NE "B" THEN
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
        IF NOT (v-ceroute#out-log EQ YES AND INDEX("GL,DC",lv-dept) GT 0) AND
            NOT CAN-DO("RC,GU",lv-dept) THEN 
        DO:
            APPLY "tab" TO est-op.n-out .
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

            IF ll-import-stds AND LOOKUP(lv-dept,"RC,GU") NE 0 THEN RUN get-stds.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.num-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-col Dialog-Frame
ON ENTRY OF est-op.num-col IN FRAME Dialog-Frame /* Inks */
    DO:
        IF lv-dept NE "PR" THEN 
        DO WITH FRAME {&FRAME-NAME}:
            APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-col Dialog-Frame
ON LEAVE OF est-op.num-col IN FRAME Dialog-Frame /* Inks */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF ll-import-stds AND lv-dept NE "PR" AND lv-dept NE "CT" THEN RUN get-stds.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.num-coat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-coat Dialog-Frame
ON ENTRY OF est-op.num-coat IN FRAME Dialog-Frame /* Varnish */
    DO:
        IF lv-dept NE "PR" AND lv-dept NE "CT" THEN 
        DO WITH FRAME {&FRAME-NAME}:
            APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-coat Dialog-Frame
ON LEAVE OF est-op.num-coat IN FRAME Dialog-Frame /* Varnish */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF ll-import-stds AND lv-dept NE "PR" AND lv-dept NE "CT" THEN RUN get-stds.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.plates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.plates Dialog-Frame
ON ENTRY OF est-op.plates IN FRAME Dialog-Frame /* Plate changes */
    DO:
        DEFINE VARIABLE ll AS LOGICAL INITIAL YES NO-UNDO.


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
        IF LASTKEY NE -1 THEN 
        DO:
            IF ll-import-stds AND lv-dept NE "PR" THEN RUN get-stds.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.fountains
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.fountains Dialog-Frame
ON ENTRY OF est-op.fountains IN FRAME Dialog-Frame /* Fountain Changes */
    DO:
        DEFINE VARIABLE ll AS LOGICAL INITIAL YES NO-UNDO.


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
        IF LASTKEY NE -1 THEN 
        DO:
            IF ll-import-stds THEN RUN get-stds.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.n_out_div
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n_out_div Dialog-Frame
ON LEAVE OF est-op.n_out_div IN FRAME Dialog-Frame /* Run Qty Divisor */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF ll-import-stds THEN RUN get-stds.
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
            est-op.company = est-qty.company
            est-op.est-no  = est-qty.est-no
            est-op.auto    = NO
            est-op.line    = i
            est-op.s-num   = 1
            est-op.b-num   = 0
            est-op.op-pass = 1
            est-op.qty     = IF est.est-type EQ 1 THEN est-qty.eqty ELSE lv-eqty.

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
            est-op.m-code est-op.m-dscr est-op.n-out est-op.op-mr est-op.op-waste 
            est-op.op-speed est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] 
            est-op.op-rate[1] est-op.op-rate[2] est-op.num-col est-op.num-coat 
            est-op.plates est-op.fountains est-op.n_out_div 
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
        DISPLAY est-op.s-num est-op.b-num est-op.m-code est-op.m-dscr est-op.n-out 
            est-op.op-mr est-op.op-waste est-op.op-speed est-op.op-spoil 
            est-op.op-crew[1] est-op.op-crew[2] est-op.op-rate[1] 
            est-op.op-rate[2] est-op.num-col est-op.num-coat est-op.plates 
            est-op.fountains est-op.n_out_div 
            WITH FRAME Dialog-Frame.
    ENABLE est-op.s-num est-op.b-num est-op.m-code est-op.m-dscr est-op.n-out 
        est-op.op-mr est-op.op-waste est-op.op-speed est-op.op-spoil 
        est-op.op-crew[1] est-op.op-crew[2]  
        est-op.num-col est-op.num-coat est-op.plates 
        est-op.fountains est-op.n_out_div Btn_OK Btn_Done Btn_Cancel RECT-21 
        RECT-38 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-of-mach Dialog-Frame 
PROCEDURE first-of-mach :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-m-code LIKE est-op.m-code NO-UNDO.
    DEFINE OUTPUT PARAMETER op-first  AS   LOGICAL       NO-UNDO.
  
    DEFINE BUFFER b-est-op FOR est-op.
    DEFINE BUFFER b-mach   FOR mach.
    DEFINE BUFFER b2-mach  FOR mach.

    DEFINE VARIABLE v-mach-found AS LOGICAL NO-UNDO.

    IF NOT AVAILABLE est-op THEN
        LEAVE.

    FOR EACH b-est-op FIELDS(m-code) NO-LOCK WHERE
        b-est-op.company EQ est-op.company AND
        b-est-op.est-no  EQ est-op.est-no AND
        b-est-op.qty     EQ est-op.qty AND
        b-est-op.line    LT est-op.line
        ,
        FIRST b-mach FIELDS(sch-m-code) NO-LOCK WHERE
        b-mach.company EQ est-op.company AND
        b-mach.m-code EQ est-op.m-code
        ,
        FIRST b2-mach FIELDS(sch-m-code) NO-LOCK WHERE
        b2-mach.company EQ est-op.company AND
        b2-mach.m-code EQ b-est-op.m-code
        :

        IF b-est-op.m-code EQ ip-m-code OR
            b-mach.sch-m-code EQ b2-mach.sch-m-code THEN
        DO:
            v-mach-found = YES.
            LEAVE.
        END.
    END.

    op-first = NOT v-mach-found.

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
    DEFINE INPUT PARAMETER ip-import-stds LIKE ll-import-stds      NO-UNDO.

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
            " is obsolete, please replace or standards will not be imported"
            VIEW-AS ALERT-BOX ERROR.
        ip-import-stds = NO.
        LEAVE.
    END.
END.
  
ASSIGN
    ll-import-stds     = ip-import-stds
    ll-import-selected = ip-import-stds.

IF ip-add-update EQ "update" THEN 
DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO est-op.s-num .
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetQty Dialog-Frame 
PROCEDURE pGetQty :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    lv-eqty = 0.

    IF AVAILABLE est THEN
        IF est.est-type EQ 1 THEN lv-eqty = est-qty.eqty.

        ELSE
            FOR EACH xop NO-LOCK
                WHERE xop.company EQ est-qty.company
                AND xop.est-no  EQ est-qty.est-no
                AND xop.line    LT 500
                BY xop.qty:
                lv-eqty = xop.qty.
                LEAVE.
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
        WHERE ef.company EQ est-qty.company
        AND ef.est-no  EQ est-qty.est-no
        AND ef.form-no EQ ip-form-no
        NO-ERROR.

    IF AVAILABLE ef THEN 
    DO:

        /*task 020050908*/
        IF ip-op-lock EQ ef.op-lock THEN
        DO:

            {est/op-lock.i xest}
        
            ASSIGN
                op-lock.val[1] = INTEGER(NOT ip-op-lock)
                op-lock.val[2] = op-lock.val[1].

            RELEASE op-lock.
        
        END.

        ef.op-lock = ip-op-lock.

        RELEASE ef.
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
  
    DO WITH FRAME {&FRAME-NAME}:
        APPLY "value-changed" TO est-op.m-code .

        FIND FIRST mach
            {sys/look/machW.i}
            AND mach.m-code EQ est-op.m-code:SCREEN-VALUE 
        NO-LOCK NO-ERROR.

        IF ((AVAILABLE mach AND mach.p-type EQ "B") OR
            INTEGER(est-op.b-num:SCREEN-VALUE ) NE 0) AND
            NOT CAN-FIND(FIRST eb
            WHERE eb.company  EQ est-qty.company
            AND eb.est-no   EQ est-qty.est-no
            AND eb.form-no  EQ int(est-op.s-num:SCREEN-VALUE )
            AND eb.blank-no EQ int(est-op.b-num:SCREEN-VALUE ))
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
    DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE chr-handle AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-tmp     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-msg      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld         AS DECIMAL   NO-UNDO.

    DEFINE BUFFER xop    FOR est-op.  
    DEFINE BUFFER b-ef   FOR ef.
    DEFINE BUFFER b-eb   FOR eb.
    DEFINE BUFFER b-mach FOR mach.
  

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST mach
            {sys/look/machW.i}
            AND mach.m-code EQ est-op.m-code:SCREEN-VALUE 
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE mach THEN v-msg = "Must enter a valid Machine Code, try help".

        IF v-msg EQ "" THEN
            IF mach.obsolete THEN 
            DO:  
                v-msg = "Machine is obsolete, please choose a different machine".
            END.
        IF v-msg EQ "" THEN 
        DO:
            FIND FIRST b-ef NO-LOCK
                WHERE b-ef.company EQ est-qty.company
                AND b-ef.est-no  EQ est-qty.est-no
                AND b-ef.form-no EQ INT(est-op.s-num:SCREEN-VALUE )
                NO-ERROR.

            FIND FIRST b-eb NO-LOCK
                WHERE b-eb.company  EQ b-ef.company
                AND b-eb.est-no   EQ b-ef.est-no
                AND b-eb.form-no  EQ b-ef.form-no
                AND b-eb.blank-no EQ INT(est-op.b-num:SCREEN-VALUE )
                NO-ERROR.
            IF NOT AVAILABLE b-eb THEN
                FIND FIRST b-eb NO-LOCK
                    WHERE b-eb.company  EQ b-ef.company
                    AND b-eb.est-no   EQ b-ef.est-no
                    AND b-eb.form-no  EQ b-ef.form-no
                    NO-ERROR.

            FIND xest NO-LOCK WHERE RECID(xest) EQ RECID(est) .

            ASSIGN
                lv-d-seq = mach.d-seq
                lv-dept  = mach.dept[1].

            IF LOOKUP(lv-dept,"RC,GU") NE 0 AND AVAILABLE b-ef                     AND
                (adm-adding-record OR
                INTEGER(est-op.n-out:SCREEN-VALUE ) EQ 0) THEN
                est-op.n-out:SCREEN-VALUE  =
                    IF lv-dept EQ "RC" THEN STRING(b-ef.n-out) ELSE STRING(b-ef.n-out-l).
  
            IF mach.p-type NE "B" THEN
                ASSIGN
                    lv-op-sb = YES
                    lv-b-num = 0.
       
            ELSE
                ASSIGN
                    lv-op-sb = NO
                    lv-b-num = IF INTEGER(est-op.b-num:SCREEN-VALUE) EQ 0 THEN 1 ELSE INTEGER(est-op.b-num:SCREEN-VALUE).
       
            IF LOOKUP(lv-dept,"RC,GU") EQ 0 THEN lv-n-out = 0.

            IF lv-dept EQ "PR" THEN 
            DO:
                RUN ce/mach-ink.p.

                i = INT(adm-new-record).
                FOR EACH xop NO-LOCK
                    WHERE xop.company EQ est-op.company
                    AND xop.est-no  EQ est-op.est-no
                    AND (xop.qty    EQ est-op.qty OR est.est-type GE 2)
                    AND xop.s-num   EQ b-ef.form-no
                    AND xop.line    LT 500
                    AND (ROWID(xop) NE ROWID(est-op) OR NOT adm-new-record)
                    AND CAN-FIND(FIRST b-mach
                    WHERE b-mach.company EQ xop.company
                    AND b-mach.m-code  EQ xop.m-code
                    AND (b-mach.dept[1] EQ "PR" OR
                    b-mach.dept[2] EQ "PR" OR
                    b-mach.dept[3] EQ "PR" OR
                    b-mach.dept[4] EQ "PR" OR
                    b-mach.dept[1] EQ "CT" OR
                    b-mach.dept[2] EQ "CT" OR
                    b-mach.dept[3] EQ "CT" OR
                    b-mach.dept[4] EQ "CT"))
                    BY xop.d-seq BY xop.line:

                    i = i + 1.
                    IF ROWID(xop) EQ ROWID(est-op) THEN LEAVE.
                END.
       
                FIND FIRST w-ink
                    WHERE w-ink.form-no EQ b-ef.form-no
                    AND w-ink.pass    EQ i
                    NO-ERROR.

                IF AVAILABLE w-ink THEN 
                DO:
                    IF INTEGER(est-op.NUM-COL:SCREEN-VALUE ) NE 0 THEN
                        w-ink.inks = INTEGER(est-op.NUM-COL:SCREEN-VALUE ).
                    IF INTEGER(est-op.num-coat:SCREEN-VALUE ) NE 0 THEN
                        w-ink.varn = INTEGER(est-op.num-coat:SCREEN-VALUE ).

                    IF w-ink.press NE mach.pr-type THEN
                        v-msg = "WRONG PRESS TYPE for selected Ink..".
                    ELSE
                        IF mach.max-color LT w-ink.inks + w-ink.varn THEN
                            v-msg = "NOT ENOUGH COLORS on PRESS for selected Inks...".
                END.

                ELSE v-msg = "No Inks defined...".
            END.

            IF LOOKUP(lv-dept,"RS,RC") GT 0 OR mach.p-type EQ "R" THEN
                ASSIGN
                    sh-len = IF b-ef.roll THEN b-ef.gsh-wid ELSE b-ef.nsh-wid
                    sh-wid = IF b-ef.roll THEN b-ef.gsh-len ELSE b-ef.nsh-len.
            ELSE
                IF LOOKUP(lv-dept,"PR,GU,LM") GT 0 OR b-ef.n-out-l LE 1 THEN
                    ASSIGN
                        sh-len = b-ef.nsh-wid
                        sh-wid = b-ef.nsh-len.
                ELSE
                    ASSIGN
                        sh-len = b-ef.trim-w
                        sh-wid = b-ef.trim-l.

            IF mach.p-type EQ "B" THEN
                ASSIGN
                    sh-len = b-eb.t-len
                    sh-wid = b-eb.t-wid.

            IF v-msg EQ ""            AND
                mach.min-len GT sh-len THEN
                v-msg = "BOARD too small for Machine " +
                    TRIM(IF mach.p-type EQ "R" THEN "Side-To-Side" ELSE "Front-To-Back") +
                    "!".

            IF v-msg EQ ""            AND
                mach.max-len LT sh-len THEN
                v-msg = "BOARD too large for Machine " +
                    TRIM(IF mach.p-type EQ "R" THEN "Side-To-Side" ELSE "Front-To-Back") +
                    "!".

            IF v-msg EQ ""            AND
                mach.min-wid GT sh-wid THEN
                v-msg = "BOARD too small for Machine " +
                    TRIM(IF mach.p-type EQ "R" THEN "Front-To-Back" ELSE "Side-To-Side") +
                    "!".

            IF v-msg EQ ""            AND
                mach.max-wid LT sh-wid THEN
                v-msg = "BOARD too large for Machine " +
                    TRIM(IF mach.p-type EQ "R" THEN "Front-To-Back" ELSE "Side-To-Side") +
                    "!".

            IF v-msg EQ ""             AND
                mach.min-cal GT b-ef.cal THEN
                v-msg = "BOARD CALIPER too small for Machine!".

            IF v-msg EQ ""             AND
                mach.max-cal LT b-ef.cal THEN
                v-msg = "BOARD CALIPER too large for Machine!".

            qty = est-qty.eqty.

            FIND xef NO-LOCK WHERE  ROWID(xef) EQ ROWID(b-ef)  NO-ERROR.
            FIND xeb NO-LOCK WHERE  ROWID(xeb) EQ ROWID(b-eb)  NO-ERROR.
        
            RUN ce/mach-qty.p (ROWID(mach)).

            IF v-msg EQ ""               AND
                v-chk-qty LT mach.min-run THEN
                v-msg = "RUN QTY. too small for Machine!".

            IF v-msg EQ ""               AND
                v-chk-qty GT mach.max-run THEN
                v-msg = "RUN QTY. too large for Machine!".
        END.

        IF v-msg NE "" THEN 
        DO:
            MESSAGE TRIM(v-msg) VIEW-AS ALERT-BOX.
            APPLY "entry" TO est-op.m-code.
            RETURN ERROR.
        END.

        ll-machine-modified = est-op.m-code:MODIFIED .
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
  
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST ef
            WHERE ef.company EQ est-qty.company
            AND ef.est-no  EQ est-qty.est-no
            AND ef.form-no EQ INTEGER(est-op.s-num:SCREEN-VALUE ))
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

