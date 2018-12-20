&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asitest168       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: oe\d-ordm.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-rowid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid2 AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE VARIABLE lv-item-rowid   AS ROWID   NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdTaxProcs AS HANDLE NO-UNDO.

{oe/oe-sysct1.i NEW}

{oe/d-selmis.i NEW}
{sys/inc/ceprepprice.i}

DO TRANSACTION:
    {sys/inc/OEPrepTaxCode.i}
END.
 

DEFINE VARIABLE lv-new-recid    AS RECID     NO-UNDO.
DEFINE VARIABLE lv-valid-charge AS LOGICAL   NO-UNDO.
DEFINE VARIABLE char-hdl        AS CHARACTER NO-UNDO.

DEFINE NEW SHARED VARIABLE v-misc          AS LOGICAL   INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-fr-tax        LIKE oe-ctrl.f-tax NO-UNDO.

DEFINE NEW SHARED BUFFER xoe-ord FOR oe-ord.
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-ordm oe-ord

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame oe-ordm.charge oe-ordm.amt ~
oe-ordm.actnum oe-ordm.dscr oe-ordm.po-no oe-ordm.cost oe-ordm.ord-i-no ~
oe-ordm.ord-line oe-ordm.po-no-po oe-ordm.s-man[1] oe-ordm.s-pct[1] ~
oe-ordm.s-comm[1] oe-ordm.s-man[2] oe-ordm.s-pct[2] oe-ordm.s-comm[2] ~
oe-ordm.s-man[3] oe-ordm.s-pct[3] oe-ordm.s-comm[3] oe-ordm.tax ~
oe-ordm.spare-char-1 oe-ordm.bill oe-ordm.spare-int-1 oe-ordm.spare-char-2 ~
oe-ordm.est-no oe-ordm.form-no oe-ordm.blank-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame oe-ordm.charge ~
oe-ordm.amt oe-ordm.actnum oe-ordm.dscr oe-ordm.po-no oe-ordm.cost ~
oe-ordm.ord-i-no oe-ordm.ord-line oe-ordm.po-no-po oe-ordm.s-man[1] ~
oe-ordm.s-pct[1] oe-ordm.s-comm[1] oe-ordm.s-man[2] oe-ordm.s-pct[2] ~
oe-ordm.s-comm[2] oe-ordm.s-man[3] oe-ordm.s-pct[3] oe-ordm.s-comm[3] ~
oe-ordm.tax oe-ordm.spare-char-1 oe-ordm.bill oe-ordm.spare-int-1 ~
oe-ordm.spare-char-2 oe-ordm.est-no oe-ordm.form-no oe-ordm.blank-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame oe-ordm
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame oe-ordm
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH oe-ordm ~
      WHERE ASI.oe-ordm.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH oe-ordm ~
      WHERE ASI.oe-ordm.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame oe-ordm oe-ord
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame oe-ordm
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame oe-ord


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordm.charge oe-ordm.amt oe-ordm.actnum ~
oe-ordm.dscr oe-ordm.po-no oe-ordm.cost oe-ordm.ord-i-no oe-ordm.ord-line ~
oe-ordm.po-no-po oe-ordm.s-man[1] oe-ordm.s-pct[1] oe-ordm.s-comm[1] ~
oe-ordm.s-man[2] oe-ordm.s-pct[2] oe-ordm.s-comm[2] oe-ordm.s-man[3] ~
oe-ordm.s-pct[3] oe-ordm.s-comm[3] oe-ordm.tax oe-ordm.spare-char-1 ~
oe-ordm.bill oe-ordm.spare-int-1 oe-ordm.spare-char-2 oe-ordm.est-no ~
oe-ordm.form-no oe-ordm.blank-no 
&Scoped-define ENABLED-TABLES oe-ordm
&Scoped-define FIRST-ENABLED-TABLE oe-ordm
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS oe-ordm.charge oe-ordm.amt oe-ordm.actnum ~
oe-ordm.dscr oe-ordm.po-no oe-ordm.cost oe-ordm.ord-i-no oe-ordm.ord-line ~
oe-ordm.po-no-po oe-ordm.s-man[1] oe-ordm.s-pct[1] oe-ordm.s-comm[1] ~
oe-ordm.s-man[2] oe-ordm.s-pct[2] oe-ordm.s-comm[2] oe-ordm.s-man[3] ~
oe-ordm.s-pct[3] oe-ordm.s-comm[3] oe-ordm.tax oe-ordm.spare-char-1 ~
oe-ordm.bill oe-ordm.spare-int-1 oe-ordm.spare-char-2 oe-ordm.est-no ~
oe-ordm.form-no oe-ordm.blank-no 
&Scoped-define DISPLAYED-TABLES oe-ordm
&Scoped-define FIRST-DISPLAYED-TABLE oe-ordm


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTaxableMisc Dialog-Frame
FUNCTION fGetTaxableMisc RETURNS LOGICAL 
  ( ipcCompany AS CHARACTER,
    ipcCust AS CHARACTER,
    ipcShipto AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 10 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    LABEL "&Done" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "&Save" 
    SIZE 10 BY 1.91
    BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL ROUNDED  
    SIZE 25.8 BY 2.4.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL ROUNDED  
    SIZE 133.8 BY 12.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    oe-ordm, 
    oe-ord SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    oe-ordm.charge AT ROW 1.52 COL 29.8 COLON-ALIGNED
    LABEL "Charge" FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 28.2 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.amt AT ROW 1.52 COL 79 COLON-ALIGNED
    LABEL "Sell Price" FORMAT "->>,>>>,>>9.99"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.actnum AT ROW 2.62 COL 29.8 COLON-ALIGNED
    LABEL "Account#" FORMAT "x(25)"
    VIEW-AS FILL-IN 
    SIZE 28.2 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.dscr AT ROW 2.62 COL 79 COLON-ALIGNED
    LABEL "Description" FORMAT "x(30)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.po-no AT ROW 3.71 COL 29.8 COLON-ALIGNED
    LABEL "Customer PO#" FORMAT "x(30)"
    VIEW-AS FILL-IN 
    SIZE 28.2 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.cost AT ROW 3.71 COL 79 COLON-ALIGNED
    LABEL "Cost" FORMAT "->>,>>>,>>9.99"
    VIEW-AS FILL-IN 
    SIZE 23.6 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.ord-i-no AT ROW 4.81 COL 29.8 COLON-ALIGNED
    LABEL "Job Number" FORMAT "x(6)"
    VIEW-AS FILL-IN 
    SIZE 19.2 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.ord-line AT ROW 4.81 COL 50.2 COLON-ALIGNED NO-LABELS FORMAT "99"
    VIEW-AS FILL-IN 
    SIZE 5 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.po-no-po AT ROW 4.81 COL 79 COLON-ALIGNED
    LABEL "Vendor PO#" FORMAT ">>>>>>"
    VIEW-AS FILL-IN 
    SIZE 23.6 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-man[1] AT ROW 6.19 COL 29.8 COLON-ALIGNED
    LABEL "Sls Rep" FORMAT "X(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-pct[1] AT ROW 6.19 COL 65.8 COLON-ALIGNED
    LABEL "% of Sale" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-comm[1] AT ROW 6.19 COL 102.8 COLON-ALIGNED
    LABEL "Comm%" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-man[2] AT ROW 7.29 COL 29.8 COLON-ALIGNED
    LABEL "Sls Rep" FORMAT "X(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-pct[2] AT ROW 7.29 COL 65.8 COLON-ALIGNED
    LABEL "% of Sale" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-comm[2] AT ROW 7.29 COL 102.8 COLON-ALIGNED
    LABEL "Comm%" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-man[3] AT ROW 8.38 COL 29.8 COLON-ALIGNED
    LABEL "Sls Rep" FORMAT "X(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-pct[3] AT ROW 8.38 COL 65.8 COLON-ALIGNED
    LABEL "% of Sale" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.s-comm[3] AT ROW 8.38 COL 102.8 COLON-ALIGNED
    LABEL "Comm%" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.tax AT ROW 9.48 COL 29.8 COLON-ALIGNED
    LABEL "Tax" FORMAT "Y/N"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.spare-char-1 AT ROW 9.48 COL 65.8 COLON-ALIGNED
    LABEL "Tax Prep Code" FORMAT "x(3)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.bill AT ROW 9.48 COL 102.8 COLON-ALIGNED
    LABEL "Bill" FORMAT "X"
    VIEW-AS FILL-IN 
    SIZE 6.4 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.spare-int-1 AT ROW 10.57 COL 29.8 COLON-ALIGNED
    LABEL "Line" FORMAT "->,>>>,>>9"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
    oe-ordm.spare-char-2 AT ROW 10.57 COL 65.8 COLON-ALIGNED
    LABEL "FG Item Code" FORMAT "x(15)"
    VIEW-AS FILL-IN 
    SIZE 24.2 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.est-no AT ROW 10.57 COL 102.8 COLON-ALIGNED
    LABEL "Estimate" FORMAT "x(12)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.form-no AT ROW 11.67 COL 29.8 COLON-ALIGNED
    LABEL "S" FORMAT ">9"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    oe-ordm.blank-no AT ROW 11.67 COL 65.8 COLON-ALIGNED
    LABEL "B" FORMAT ">9"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    Btn_OK AT ROW 14.4 COL 110
    Btn_Done AT ROW 14.6 COL 112
    Btn_Cancel AT ROW 14.4 COL 120
    RECT-21 AT ROW 14.1 COL 107
    RECT-38 AT ROW 1 COL 1.2
    SPACE(0.79) SKIP(3.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Misc. Charge Item Update".


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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN oe-ordm.actnum IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.amt IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.bill IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.blank-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.charge IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.dscr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.est-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.form-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.ord-i-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.ord-line IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.po-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.po-no-po IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-comm[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-comm[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-comm[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-man[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-man[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-man[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-pct[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-pct[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.s-pct[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.spare-char-1 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.spare-char-2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.spare-int-1 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordm.tax IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.oe-ordm,asi.oe-ord "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.oe-ordm.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Misc. Charge Item Update */
    DO:
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID         NO-UNDO.
        DEFINE VARIABLE v-li       AS INTEGER       NO-UNDO.
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE        NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :
            WHEN "charge" THEN 
                DO:
                    RUN windows/l-prep.w (oe-ord.company, oe-ordm.charge:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val NE "" AND ENTRY(1,char-val) NE oe-ordm.charge:SCREEN-VALUE THEN 
                    DO:
                        oe-ordm.charge:SCREEN-VALUE = ENTRY(1,char-val).
                        RUN new-charge.
                    END.
                END.
            WHEN "actnum" THEN 
                DO:
                    RUN windows/l-acct2.w (oe-ord.company, "", oe-ordm.actnum:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN oe-ordm.actnum:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            WHEN "ord-i-no" THEN RUN job-help.
            WHEN "ord-line" THEN RUN job-help.
            WHEN "s-man" THEN 
                DO:
                    v-li = FRAME-INDEX.
                    RUN windows/l-sman.w (oe-ord.company, OUTPUT char-val).
                    IF char-val NE "" THEN 
                    DO:
                        IF v-li EQ 1 AND oe-ordm.s-man[1]:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                            oe-ordm.s-man[1]:SCREEN-VALUE = ENTRY(1,char-val).
                        ELSE
                            IF v-li EQ 2 AND oe-ordm.s-man[2]:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                                oe-ordm.s-man[2]:SCREEN-VALUE = ENTRY(1,char-val).
                            ELSE
                                IF v-li EQ 3 AND oe-ordm.s-man[3]:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                                    oe-ordm.s-man[3]:SCREEN-VALUE = ENTRY(1,char-val).
                                ELSE v-li = 0.
                        IF v-li NE 0 THEN RUN new-s-man (v-li).
                    END.
                END.
            WHEN "po-no-po" THEN 
                DO:
                    RUN windows/l-ponopo.w (oe-ord.company,YES,lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val) .         
                END.
            /* when "est-no" then do:
                  run windows/l-est3.w (oe-ord.company,"",oe-ordm.est-no:SCREEN-VALUE, output char-val).
                  if char-val <> "" then assign oe-ordm.est-no:SCREEN-VALUE = entry(1,char-val) .         
             end.*/
            WHEN "spare-char-2" THEN 
                DO:
                    RUN windows/l-itmfgo.w (oe-ord.company,"",oe-ord.ord-no,oe-ordm.spare-char-2:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN ASSIGN oe-ordm.spare-char-2:SCREEN-VALUE = ENTRY(1,char-val) .         
                END. 
            WHEN "spare-char-1" THEN 
                DO:
                    RUN windows/l-stax.w (oe-ord.company,oe-ordm.spare-char-1:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN ASSIGN oe-ordm.spare-char-1:SCREEN-VALUE = ENTRY(1,char-val) .         
                END.

        END CASE.

        APPLY "entry" TO lw-focus.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Misc. Charge Item Update */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Misc. Charge Item Update */
    DO:
        IF AVAILABLE oe-ordm THEN
            op-rowid = ROWID(oe-ordm) .

        IF lv-item-rowid NE ? THEN 
        DO:
            FIND oe-ordm EXCLUSIVE-LOCK
                WHERE ROWID(oe-ordm) EQ lv-item-rowid  NO-ERROR.
            IF AVAILABLE oe-ordm THEN DELETE oe-ordm .
        END.
        DELETE OBJECT hdTaxProcs.
         APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        DISABLE TRIGGERS FOR LOAD OF oe-ordm .
        IF AVAILABLE oe-ordm THEN
            op-rowid = ROWID(oe-ordm) .

        IF lv-item-rowid NE ? THEN 
        DO:

            FIND oe-ordm EXCLUSIVE-LOCK
                WHERE ROWID(oe-ordm) EQ lv-item-rowid  NO-ERROR.
            IF AVAILABLE oe-ordm THEN DELETE oe-ordm .
        END.
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
        DEFINE VARIABLE char-hdl    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ld-prev-amt LIKE oe-ordm.amt NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            ld-prev-amt = DECIMAL(oe-ordm.amt:SCREEN-VALUE).
        END.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        /* ======== validation ===============*/
        DO WITH FRAME {&FRAME-NAME}:
            RUN valid-charge (oe-ordm.charge:HANDLE IN FRAME {&FRAME-NAME}) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            RUN valid-est (oe-ordm.est-no:HANDLE IN FRAME {&FRAME-NAME}) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY .
        END.

        RUN valid-actnum NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        RUN valid-tax NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-tax-gr NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-ord-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        RUN valid-ord-line NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-po-no-po NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        RUN valid-s-man (0) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-s-pct (0) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-bill NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        FIND CURRENT oe-ordm EXCLUSIVE-LOCK NO-ERROR.
        oe-ordm.spare-char-1 = oe-ordm.spare-char-1:SCREEN-VALUE .

        DO TRANSACTION:
            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        FIND CURRENT oe-ord EXCLUSIVE.

        IF oe-ordm.amt NE 0 AND
            (oe-ord.stat NE "N" AND oe-ord.stat NE "H" AND oe-ord.stat NE "A") THEN
            oe-ord.stat = "U".  /* order updated */
        FIND CURRENT oe-ord NO-LOCK.

        FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) EXCLUSIVE.

        RUN oe/oe-comm.p.

        RELEASE xoe-ord.
  
        RUN oe/calcordt.p (ROWID(oe-ord)).
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no
            USE-INDEX cust  NO-ERROR.
        IF (oe-ordm.bill NE "N" AND ld-prev-amt NE oe-ordm.amt)
            AND AVAILABLE cust AND cust.active NE "X" AND AVAILABLE oe-ord AND oe-ord.TYPE NE "T" THEN
            RUN oe/creditck.p (ROWID(oe-ord), YES).
  
        /* create reftable for prep */
        IF oe-ordm.est-no NE "" THEN 
        DO:
            RUN create-reft4prep NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END. 

        /* gdm - 06170905 */ 
        RUN assgn-prep-info.

  

        op-rowid = ROWID(oe-ordm).

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.charge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.charge Dialog-Frame
ON LEAVE OF oe-ordm.charge IN FRAME Dialog-Frame /* Charge */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-charge (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.charge Dialog-Frame
ON VALUE-CHANGED OF oe-ordm.charge IN FRAME Dialog-Frame /* Charge */
    DO:
        RUN new-charge.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.actnum Dialog-Frame
ON LEAVE OF oe-ordm.actnum IN FRAME Dialog-Frame /* Account# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-actnum NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.ord-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.ord-i-no Dialog-Frame
ON LEAVE OF oe-ordm.ord-i-no IN FRAME Dialog-Frame /* Job# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-ord-i-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.ord-line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.ord-line Dialog-Frame
ON LEAVE OF oe-ordm.ord-line IN FRAME Dialog-Frame
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-ord-line NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.po-no-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.po-no-po Dialog-Frame
ON LEAVE OF oe-ordm.po-no-po IN FRAME Dialog-Frame /* Vendor PO# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-po-no-po NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-man[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[1] Dialog-Frame
ON LEAVE OF oe-ordm.s-man[1] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (1) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[1] Dialog-Frame
ON VALUE-CHANGED OF oe-ordm.s-man[1] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        RUN new-s-man (1).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-pct[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-pct[1] Dialog-Frame
ON ENTRY OF oe-ordm.s-pct[1] IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF oe-ordm.s-man[1]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-comm[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-comm[1] Dialog-Frame
ON ENTRY OF oe-ordm.s-comm[1] IN FRAME Dialog-Frame /* Comm% */
    DO:
        IF oe-ordm.s-man[1]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-man[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[2] Dialog-Frame
ON ENTRY OF oe-ordm.s-man[2] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        IF oe-ordm.s-man[1]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[2] Dialog-Frame
ON LEAVE OF oe-ordm.s-man[2] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (2) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[2] Dialog-Frame
ON VALUE-CHANGED OF oe-ordm.s-man[2] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        RUN new-s-man (2).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-pct[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-pct[2] Dialog-Frame
ON ENTRY OF oe-ordm.s-pct[2] IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF oe-ordm.s-man[2]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-comm[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-comm[2] Dialog-Frame
ON ENTRY OF oe-ordm.s-comm[2] IN FRAME Dialog-Frame /* Comm% */
    DO:
        IF oe-ordm.s-man[2]:SCREEN-VALUE EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-man[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[3] Dialog-Frame
ON ENTRY OF oe-ordm.s-man[3] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        IF oe-ordm.s-man[2]:SCREEN-VALUE EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[3] Dialog-Frame
ON LEAVE OF oe-ordm.s-man[3] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (3) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-man[3] Dialog-Frame
ON VALUE-CHANGED OF oe-ordm.s-man[3] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        RUN new-s-man (3).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-pct[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-pct[3] Dialog-Frame
ON ENTRY OF oe-ordm.s-pct[3] IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF oe-ordm.s-man[3]:SCREEN-VALUE EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.s-comm[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.s-comm[3] Dialog-Frame
ON ENTRY OF oe-ordm.s-comm[3] IN FRAME Dialog-Frame /* Comm% */
    DO:
        IF oe-ordm.s-man[3]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.tax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.tax Dialog-Frame
ON LEAVE OF oe-ordm.tax IN FRAME Dialog-Frame /* Tax */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-tax NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN /* NO-APPLY */.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.bill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.bill Dialog-Frame
ON LEAVE OF oe-ordm.bill IN FRAME Dialog-Frame /* Bill */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-bill NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME oe-ordm.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.est-no Dialog-Frame
ON LEAVE OF oe-ordm.est-no IN FRAME Dialog-Frame /* Estimate */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-est (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordm.spare-char-1 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-ordm.spare-char-1 IN FRAME Dialog-Frame /* Tax */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-tax-gr NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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
        
    RUN system/TaxProcs.p PERSISTENT SET hdTaxProcs.
    
    FIND oe-ord NO-LOCK
        WHERE oe-ord.company EQ cocode
        AND ROWID(oe-ord)  EQ ip-rowid2 NO-ERROR .

    IF ip-type EQ "copy" THEN lv-item-rowid = ip-rowid.


    IF ip-rowid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND oe-ordm NO-LOCK WHERE ROWID(oe-ordm) EQ ip-rowid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

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
    /*IF ip-type EQ "update" THEN DISABLE oe-rell.ord-no oe-rell.rel-no oe-rell.b-ord-no.

    IF ip-type EQ "add"  OR ip-type EQ "copy" THEN DO:
        APPLY "entry" TO oe-rell.ord-no  .
    END.
    oe-rell.cust-no:HIDDEN IN FRAME {&FRAME-NAME}  = TRUE .*/
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assgn-prep-info Dialog-Frame 
PROCEDURE assgn-prep-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FIND FIRST prep EXCLUSIVE-LOCK
        WHERE prep.company EQ oe-ordm.company 
        AND prep.code    EQ oe-ordm.charge NO-ERROR.
    IF AVAILABLE prep THEN 
    DO:

        ASSIGN 
            prep.last-order  = oe-ordm.ord-no
            prep.last-est-no = oe-ordm.est-no.

        FIND CURRENT prep NO-LOCK.
        RELEASE prep.
    END.

    RELEASE prep.
    RELEASE reftable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ordm FOR oe-ordm.
    DEFINE VARIABLE li-line  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-fgitem AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-error AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ordl FOR oe-ordl.
    /* Code placed here will execute PRIOR to standard behavior. */
    FIND LAST bf-ordm NO-LOCK
        WHERE bf-ordm.company EQ oe-ord.company
        AND bf-ordm.ord-no  EQ oe-ord.ord-no
        USE-INDEX oe-misc NO-ERROR.
    li-line = IF AVAILABLE bf-ordm THEN bf-ordm.line ELSE 0.
  
    CREATE oe-ordm.
    ASSIGN 
        lv-item-rowid = ROWID(oe-ordm).
    ll-new-record = YES.

    /* Code placed here will execute AFTER standard behavior.    */
    lv-new-recid = RECID(oe-ordm).
    FIND CURRENT oe-ordm EXCLUSIVE-LOCK.
    ASSIGN
        oe-ordm.company   = oe-ord.company
        oe-ordm.ord-no    = oe-ord.ord-no
        oe-ordm.est-no    = oe-ord.est-no
        oe-ordm.line      = li-line + 1
        oe-ordm.bill      = "Y"
        oe-ordm.s-man[1]  = oe-ord.sman[1]
        oe-ordm.s-pct[1]  = oe-ord.s-pct[1]
        oe-ordm.s-comm[1] = oe-ord.s-comm[1]
        oe-ordm.s-man[2]  = oe-ord.sman[2]
        oe-ordm.s-pct[2]  = oe-ord.s-pct[2]
        oe-ordm.s-comm[2] = oe-ord.s-comm[2]
        oe-ordm.s-man[3]  = oe-ord.sman[3]
        oe-ordm.s-pct[3]  = oe-ord.s-pct[3]
        oe-ordm.s-comm[3] = oe-ord.s-comm[3] .

    IF AVAILABLE oe-ctrl AND oe-ctrl.prep-comm EQ NO THEN 
    DO:             /*Task# 11271302*/  
        ASSIGN
            oe-ordm.s-comm[1] = 0
            oe-ordm.s-comm[2] = 0     
            oe-ordm.s-comm[3] = 0.
    END.

    FIND FIRST ar-ctrl WHERE ar-ctrl.company = oe-ord.company NO-LOCK NO-ERROR.
    IF AVAILABLE ar-ctrl THEN oe-ordm.actnum = ar-ctrl.sales.
    FIND FIRST cust OF oe-ord NO-LOCK.

    FIND FIRST oe-ctrl NO-LOCK
        WHERE oe-ctrl.company = oe-ord.company
        NO-ERROR.
   
    IF AVAILABLE oe-ctrl AND oe-ctrl.prep-chrg THEN
        ASSIGN oe-ordm.spare-char-1 = IF AVAILABLE shipto AND shipto.tax-code NE "" THEN shipto.tax-code
                                    ELSE IF AVAILABLE cust AND cust.spare-char-1 <> "" THEN cust.spare-char-1 
                                    ELSE oe-ord.tax-gr
               oe-ordm.tax          = fGetTaxableMisc(cocode, oe-ord.cust-no, oe-ord.ship-id) .
  
    i = 0 .
    FOR EACH bf-ordl OF oe-ord NO-LOCK:
        i = i + 1.
    END.
  
    IF i = 1 THEN 
    DO:
        IF AVAILABLE oe-ord THEN
            FIND FIRST bf-ordl OF oe-ord NO-LOCK NO-ERROR.
        IF AVAILABLE bf-ordl THEN
            ASSIGN
                oe-ordm.spare-char-2 = bf-ordl.i-no 
                oe-ordm.ord-i-no     = bf-ordl.job-no
                oe-ordm.ord-line     = bf-ordl.job-no2 .
    END.
    ELSE 
    DO:
        RUN cec/mis-ordfg.w (RECID(oe-ord),OUTPUT v-fgitem,OUTPUT lv-error ) NO-ERROR.
        ASSIGN
            oe-ordm.spare-char-2 = v-fgitem .
        IF AVAILABLE oe-ord THEN
            FIND FIRST bf-ordl OF oe-ord 
                WHERE bf-ordl.i-no EQ v-fgitem NO-LOCK NO-ERROR.
        IF AVAILABLE bf-ordl THEN
            ASSIGN
                oe-ordm.ord-i-no = bf-ordl.job-no
                oe-ordm.ord-line = bf-ordl.job-no2 .
    END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reft4prep Dialog-Frame 
PROCEDURE create-reft4prep :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-prep-cnt AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-returnc  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-form#    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-line#    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-eqty     AS INTEGER   NO-UNDO.

    IF CAN-FIND(FIRST prep WHERE prep.company EQ oe-ord.company 
        AND prep.code EQ oe-ordm.charge /*AND prep.mat-type = "P"*/) AND
        AVAILABLE oe-ordm AND 
        (oe-ordm.miscType <> 1 )
        THEN 
    DO:
        lv-prep-cnt = 0.
        FOR EACH est-prep NO-LOCK WHERE est-prep.company EQ oe-ordm.company
            AND est-prep.est-no EQ oe-ordm.est-no 
            AND est-prep.CODE EQ oe-ordm.charge
            AND est-prep.simon   EQ "S" :
            lv-prep-cnt = lv-prep-cnt + 1.
        END.
        IF lv-prep-cnt > 1 THEN 
        DO:
            RUN oe/d-formno.w (INPUT oe-ordm.est-no, INPUT oe-ordm.charge, INPUT RECID(oe-ordm), OUTPUT lv-returnc).
            IF lv-returnc = "" THEN RETURN ERROR.

            ASSIGN 
                lv-form# = INTEGER(ENTRY(2,lv-returnc))
                lv-line# = INTEGER(ENTRY(4,lv-returnc))
                lv-eqty  = INTEGER(ENTRY(6,lv-returnc)).
            FIND FIRST est-prep NO-LOCK WHERE est-prep.company EQ oe-ordm.company
                AND est-prep.est-no  EQ oe-ordm.est-no
                AND est-prep.eqty    EQ lv-eqty
                AND est-prep.line    EQ lv-line#
                AND est-prep.code    EQ oe-ordm.charge
                AND est-prep.simon   EQ "S"
                AND est-prep.amtz    EQ 100  NO-ERROR.
        END.
        ELSE FIND FIRST est-prep NO-LOCK WHERE est-prep.company EQ oe-ordm.company
                AND est-prep.est-no  EQ oe-ordm.est-no
                /*  AND est-prep.eqty    EQ lv-eqty
                  AND est-prep.line    EQ lv-line#*/
                AND est-prep.code    EQ oe-ordm.charge
                AND est-prep.simon   EQ "S"
                AND est-prep.amtz    EQ 100  NO-ERROR.

      
        IF AVAILABLE est-prep THEN
            ASSIGN 
                oe-ordm.miscType    = 1
                oe-ordm.estPrepEqty = est-prep.eqty
                oe-ordm.estPrepLine = est-prep.line
                oe-ordm.est-no      = est-prep.est-no.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    IF AVAILABLE oe-ordm  THEN 
    DO:
        
        DISPLAY  oe-ordm.charge oe-ordm.amt 
            oe-ordm.actnum oe-ordm.dscr oe-ordm.po-no oe-ordm.cost oe-ordm.ord-i-no 
            oe-ordm.ord-line oe-ordm.po-no-po oe-ordm.s-man[1] oe-ordm.s-pct[1] 
            oe-ordm.s-comm[1] oe-ordm.s-man[2] oe-ordm.s-pct[2] oe-ordm.s-comm[2] 
            oe-ordm.s-man[3] oe-ordm.s-pct[3] oe-ordm.s-comm[3] oe-ordm.tax 
            oe-ordm.spare-char-1 oe-ordm.bill oe-ordm.spare-int-1 oe-ordm.spare-char-2 
            oe-ordm.est-no oe-ordm.form-no oe-ordm.blank-no  
            WITH FRAME Dialog-Frame.
    END.

    RUN oe/oe-sysct.p.

    IF NOT v-oecomm-log THEN RUN show-comm (NO).

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
    IF AVAILABLE oe-ordm THEN 
        DISPLAY oe-ordm.charge oe-ordm.amt oe-ordm.actnum oe-ordm.dscr oe-ordm.po-no 
            oe-ordm.cost oe-ordm.ord-i-no oe-ordm.ord-line oe-ordm.po-no-po 
            oe-ordm.s-man[1] oe-ordm.s-pct[1] oe-ordm.s-comm[1] oe-ordm.s-man[2] 
            oe-ordm.s-pct[2] oe-ordm.s-comm[2] oe-ordm.s-man[3] oe-ordm.s-pct[3] 
            oe-ordm.s-comm[3] oe-ordm.tax oe-ordm.spare-char-1 oe-ordm.bill 
            oe-ordm.spare-int-1 oe-ordm.spare-char-2 oe-ordm.est-no 
            oe-ordm.form-no oe-ordm.blank-no 
            WITH FRAME Dialog-Frame.
    ENABLE oe-ordm.charge oe-ordm.amt oe-ordm.actnum oe-ordm.dscr oe-ordm.po-no 
        oe-ordm.cost oe-ordm.ord-i-no oe-ordm.ord-line oe-ordm.po-no-po 
        oe-ordm.s-man[1] oe-ordm.s-pct[1] oe-ordm.s-comm[1] oe-ordm.s-man[2] 
        oe-ordm.s-pct[2] oe-ordm.s-comm[2] oe-ordm.s-man[3] oe-ordm.s-pct[3] 
        oe-ordm.s-comm[3] oe-ordm.tax oe-ordm.spare-char-1 oe-ordm.bill 
        oe-ordm.spare-int-1 oe-ordm.spare-char-2 oe-ordm.est-no 
        oe-ordm.form-no oe-ordm.blank-no Btn_OK Btn_Done Btn_Cancel RECT-21 
        RECT-38 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE job-help Dialog-Frame 
PROCEDURE job-help :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-val   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE look-recid AS RECID     NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        RUN windows/l-jobno.w (cocode, oe-ordm.ord-i-no:SCREEN-VALUE , OUTPUT char-val, OUTPUT look-recid).
        IF look-recid NE ? THEN 
        DO:
            FIND job-hdr WHERE RECID(job-hdr) EQ look-recid NO-LOCK NO-ERROR.
            IF AVAILABLE job-hdr THEN 
                ASSIGN
                    oe-ordm.ord-i-no:SCREEN-VALUE = job-hdr.job-no
                    oe-ordm.ord-line:SCREEN-VALUE = STRING(job-hdr.job-no2).                      
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-charge Dialog-Frame 
PROCEDURE new-charge :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE markUp AS DECIMAL NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:
        FIND prep  NO-LOCK
            WHERE prep.company EQ oe-ord.company 
            AND prep.code    EQ oe-ordm.charge:SCREEN-VALUE
            NO-ERROR.
        IF AVAILABLE prep THEN 
        DO:

            IF ceprepprice-chr EQ "Profit" THEN
                markUp = prep.cost / (1 - (prep.mkup / 100)).
            ELSE
                markUp = prep.cost * (1 + (prep.mkup / 100)).

            ASSIGN
                oe-ordm.dscr:SCREEN-VALUE = prep.dscr
                oe-ordm.cost:SCREEN-VALUE = STRING(prep.cost)
                oe-ordm.amt:SCREEN-VALUE  = STRING(markUp).

            FIND cust OF oe-ord NO-LOCK.

            FIND FIRST oe-ctrl NO-LOCK
                WHERE oe-ctrl.company = oe-ord.company
                NO-ERROR.
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-ord.cust-no
                NO-ERROR.
            IF AVAILABLE oe-ctrl AND oe-ctrl.prep-chrg THEN
                ASSIGN oe-ordm.spare-char-1:SCREEN-VALUE = IF AVAILABLE shipto AND shipto.tax-code NE "" THEN shipto.tax-code
                                                    ELSE IF AVAILABLE cust AND cust.spare-char-1 <> "" THEN cust.spare-char-1 
                                                    ELSE oe-ord.tax-gr
                    .

            FIND FIRST account
                WHERE account.company EQ oe-ord.company
                AND account.actnum  EQ prep.actnum
                NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN oe-ordm.actnum:SCREEN-VALUE = prep.actnum.

            RUN new-comm (0).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-comm Dialog-Frame 
PROCEDURE new-comm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE v-li AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv   AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST prep
            WHERE prep.company EQ oe-ord.company 
            AND prep.code    EQ oe-ordm.charge:SCREEN-VALUE
            NO-LOCK NO-ERROR.

        IF AVAILABLE oe-ctrl AND oe-ctrl.prep-comm EQ YES THEN 
        DO:             /*Task# 11271302*/
        
            DO v-li = 1 TO IF ip-int EQ 0 THEN 3 ELSE ip-int:
                lv = IF v-li EQ 1 THEN oe-ordm.s-man[1]:SCREEN-VALUE ELSE
                    IF v-li EQ 2 THEN oe-ordm.s-man[2]:SCREEN-VALUE ELSE
                    oe-ordm.s-man[3]:SCREEN-VALUE.

                IF lv NE "" THEN 
                DO:
                    RUN sys/inc/getsmncm.p (oe-ord.cust-no,
                        INPUT-OUTPUT lv,
                        IF AVAILABLE prep THEN prep.fgcat ELSE "",
                        0,
                        OUTPUT ld).          

                    CASE v-li:
                        WHEN 1 THEN 
                            oe-ordm.s-comm[1]:SCREEN-VALUE = STRING(ld).
                        WHEN 2 THEN 
                            oe-ordm.s-comm[2]:SCREEN-VALUE = STRING(ld).
                        WHEN 3 THEN 
                            oe-ordm.s-comm[3]:SCREEN-VALUE = STRING(ld).
                    END CASE.
                END.
            END.
        END.  /*IF AVAIL oe-ctrl AND oe-ctrl.prep-comm EQ YES THEN do:  */           /*Task# 11271302*/
    END.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-s-man Dialog-Frame 
PROCEDURE new-s-man :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE lv-sman LIKE sman.sman NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        lv-sman = IF ip-int EQ 3 THEN oe-ordm.s-man[3]:SCREEN-VALUE 
        ELSE
            IF ip-int EQ 2 THEN oe-ordm.s-man[2]:SCREEN-VALUE
            ELSE oe-ordm.s-man[1]:SCREEN-VALUE.

        IF lv-sman NE "" THEN 
        DO:
            FIND FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ lv-sman
                NO-LOCK NO-ERROR.
            IF AVAILABLE sman THEN 
            DO:
                IF ip-int EQ 3 THEN 
                DO:
                    IF DECIMAL(oe-ordm.s-pct[3]:SCREEN-VALUE) EQ 0 THEN
                        oe-ordm.s-pct[3]:SCREEN-VALUE = "100".
                    IF DECIMAL(oe-ordm.s-comm[3]:SCREEN-VALUE) EQ 0 THEN 
                    DO:
                        IF AVAILABLE oe-ctrl AND oe-ctrl.prep-comm EQ YES THEN        /*Task# 11271302*/
                            oe-ordm.s-comm[3]:SCREEN-VALUE = STRING(sman.scomm).
                    END.

                    RUN new-comm (3).
                END.
                ELSE
                    IF ip-int EQ 2 THEN 
                    DO:
                        IF DECIMAL(oe-ordm.s-pct[2]:SCREEN-VALUE) EQ 0 THEN
                            oe-ordm.s-pct[2]:SCREEN-VALUE = "100".
                        IF DECIMAL(oe-ordm.s-comm[2]:SCREEN-VALUE) EQ 0 THEN 
                        DO:
                            IF AVAILABLE oe-ctrl AND oe-ctrl.prep-comm EQ YES THEN        /*Task# 11271302*/
                                oe-ordm.s-comm[2]:SCREEN-VALUE = STRING(sman.scomm).
                        END.

                        RUN new-comm (2).
                    END.
                    ELSE 
                    DO:
                        IF DECIMAL(oe-ordm.s-pct[1]:SCREEN-VALUE) EQ 0 THEN
                            oe-ordm.s-pct[1]:SCREEN-VALUE = "100".
                        IF DECIMAL(oe-ordm.s-comm[1]:SCREEN-VALUE) EQ 0 THEN 
                        DO:
                            IF AVAILABLE oe-ctrl AND oe-ctrl.prep-comm EQ YES THEN        /*Task# 11271302*/
                                oe-ordm.s-comm[1]:SCREEN-VALUE = STRING(sman.scomm).
                        END.

                        RUN new-comm (1).
                    END.
            END.

        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-comm Dialog-Frame 
PROCEDURE show-comm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-visible AS LOGICAL NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            oe-ordm.s-pct[1]:VISIBLE IN FRAME {&FRAME-NAME}  = ip-visible
            oe-ordm.s-pct[2]:VISIBLE IN FRAME {&FRAME-NAME}  = ip-visible
            oe-ordm.s-pct[3]:VISIBLE IN FRAME {&FRAME-NAME}  = ip-visible
            oe-ordm.s-comm[1]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
            oe-ordm.s-comm[2]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
            oe-ordm.s-comm[3]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum Dialog-Frame 
PROCEDURE valid-actnum :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF oe-ordm.actnum:SCREEN-VALUE EQ "" OR
            NOT CAN-FIND(FIRST account
            WHERE account.company EQ oe-ord.company 
            AND account.actnum  EQ oe-ordm.actnum:SCREEN-VALUE
            /*AND account.type    EQ "R"*/)
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordm.actnum IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bill Dialog-Frame 
PROCEDURE valid-bill :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-DO("Y,N,I",oe-ordm.bill:SCREEN-VALUE)       
            THEN 
        DO:
            MESSAGE "Invalid entry. Enter (Y)es, (N)o, or (I)nvoiced." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordm.bill IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-charge Dialog-Frame 
PROCEDURE valid-charge :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

    DEFINE VARIABLE ll AS LOGICAL INIT YES NO-UNDO.

  
    DO WITH FRAME {&FRAME-NAME}:
        ll = ip-focus:SCREEN-VALUE NE "" AND
            CAN-FIND(FIRST prep
            WHERE prep.company EQ oe-ord.company 
            AND prep.code    EQ ip-focus:SCREEN-VALUE).

        IF NOT ll THEN 
        DO:
      

            ll = AVAILABLE oe-ordm AND
                CAN-FIND(FIRST ef
                WHERE ef.company EQ oe-ordm.company
                AND ef.est-no  EQ oe-ordm.est-no
                AND ef.eqty    EQ oe-ordm.estPrepEqty
                AND ef.form-no EQ oe-ordm.estPrepLine
                AND ef.mis-cost[INTEGER(oe-ordm.miscInd)] EQ oe-ordm.charge).
        END.

        IF NOT ll THEN 
        DO:
            MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est Dialog-Frame 
PROCEDURE valid-est :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-est-no LIKE oe-ordm.est-no NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl .
    DO WITH FRAME {&FRAME-NAME}:
        IF AVAILABLE oe-ordm THEN 
        DO:
            ASSIGN 
                lv-est-no                   = TRIM(oe-ordm.est-no:SCREEN-VALUE)
                lv-est-no                   = FILL(" ", 8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no)
                oe-ordm.est-no:SCREEN-VALUE = lv-est-no.

            IF oe-ordm.est-no:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-ordm.company
                    AND bf-oe-ordl.ord-no EQ oe-ordm.ord-no
                    AND bf-oe-ordl.est-no EQ oe-ordm.est-no:SCREEN-VALUE NO-LOCK NO-ERROR.

                IF NOT AVAILABLE bf-oe-ordl THEN 
                DO:
                    MESSAGE "Estimate is not on order..."
                        VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO ip-focus.
                    RETURN ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-i-no Dialog-Frame 
PROCEDURE valid-ord-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes: ord-i-no is used for job-no
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-job-no LIKE job.job-no NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-job-no                     = oe-ordm.ord-i-no:SCREEN-VALUE
            lv-job-no                     = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no)
            oe-ordm.ord-i-no:SCREEN-VALUE = lv-job-no.

        IF lv-job-no NE "" THEN 
        DO:
            FIND FIRST job NO-LOCK
                WHERE job.company EQ cocode
                AND job.job-no  EQ lv-job-no
                NO-ERROR.
            IF NOT AVAILABLE job THEN 
            DO:
                MESSAGE TRIM(oe-ordm.ord-i-no:LABEL IN FRAME {&FRAME-NAME}) +
                    " is invalid..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO oe-ordm.ord-i-no IN FRAME {&FRAME-NAME}.
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-line Dialog-Frame 
PROCEDURE valid-ord-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE lv-job-no LIKE job.job-no NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-job-no                     = oe-ordm.ord-i-no:SCREEN-VALUE
            lv-job-no                     = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no)
            oe-ordm.ord-i-no:SCREEN-VALUE = lv-job-no.

        IF lv-job-no NE "" THEN 
        DO:
            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ lv-job-no
                AND job.job-no2 EQ INT(oe-ordm.ord-line:SCREEN-VALUE)
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job THEN 
            DO:
                MESSAGE TRIM(oe-ordm.ord-i-no:LABEL IN FRAME {&FRAME-NAME}) +
                    " is invalid..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO oe-ordm.ord-line IN FRAME {&FRAME-NAME}.
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no-po Dialog-Frame 
PROCEDURE valid-po-no-po :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(oe-ordm.po-no-po:SCREEN-VALUE) NE 0 AND
            NOT CAN-FIND(FIRST po-ord
            WHERE po-ord.company EQ oe-ord.company 
            AND po-ord.po-no   EQ INTEGER(oe-ordm.po-no-po:SCREEN-VALUE))
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordm.po-no-po IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-man Dialog-Frame 
PROCEDURE valid-s-man :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE v-li    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-sman LIKE sman.sman NO-UNDO.

    v-li = ip-int.

    IF v-li EQ 0 THEN
        ASSIGN
            ip-int = 1
            v-li   = 3.

    DO ip-int = ip-int TO v-li WITH FRAME {&FRAME-NAME}:
        lv-sman = IF ip-int EQ 3 THEN oe-ordm.s-man[3]:SCREEN-VALUE
        ELSE
            IF ip-int EQ 2 THEN oe-ordm.s-man[2]:SCREEN-VALUE
            ELSE oe-ordm.s-man[1]:SCREEN-VALUE.
    
        IF lv-sman NE "" THEN 
        DO:
            IF NOT CAN-FIND(FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ lv-sman) THEN 
            DO:
                MESSAGE "Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
                IF ip-int EQ 3 THEN APPLY "entry" TO oe-ordm.s-man[3] IN FRAME {&FRAME-NAME}.
                ELSE
                    IF ip-int EQ 2 THEN APPLY "entry" TO oe-ordm.s-man[2] IN FRAME {&FRAME-NAME}.
                    ELSE APPLY "entry" TO oe-ordm.s-man[1] IN FRAME {&FRAME-NAME}.
                RETURN ERROR.
            END.
        END.

        ELSE 
        DO:
            IF ip-int EQ 3 THEN
                ASSIGN
                    oe-ordm.s-pct[3]:SCREEN-VALUE  = "0"
                    oe-ordm.s-comm[3]:SCREEN-VALUE = "0".
            ELSE
                IF ip-int EQ 2 THEN
                    ASSIGN
                        oe-ordm.s-pct[2]:SCREEN-VALUE  = "0"
                        oe-ordm.s-comm[2]:SCREEN-VALUE = "0".
                ELSE
                    ASSIGN
                        oe-ordm.s-pct[1]:SCREEN-VALUE  = "0"
                        oe-ordm.s-comm[1]:SCREEN-VALUE = "0".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-pct Dialog-Frame 
PROCEDURE valid-s-pct :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE ld-pct AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ll     AS LOGICAL NO-UNDO.

   
    DO WITH FRAME {&FRAME-NAME}:
        ld-pct = IF ip-int EQ 1 THEN DECIMAL(oe-ordm.s-pct[1]:SCREEN-VALUE)
        ELSE
            IF ip-int EQ 2 THEN DECIMAL(oe-ordm.s-pct[2]:SCREEN-VALUE)
            ELSE
            IF ip-int EQ 3 THEN DECIMAL(oe-ordm.s-pct[3]:SCREEN-VALUE)
            ELSE (DECIMAL(oe-ordm.s-pct[1]:SCREEN-VALUE) +
            DECIMAL(oe-ordm.s-pct[2]:SCREEN-VALUE) +
            DECIMAL(oe-ordm.s-pct[3]:SCREEN-VALUE)).

        IF (oe-ordm.s-man[1]:SCREEN-VALUE NE "" OR
            oe-ordm.s-man[2]:SCREEN-VALUE NE "" OR
            oe-ordm.s-man[3]:SCREEN-VALUE NE "")   AND
            ((ip-int EQ 0 AND ld-pct NE 100) OR
            (ip-int NE 0 AND ld-pct GT 100)) THEN 
        DO:
            IF ip-int EQ 0 THEN
                MESSAGE "Charge's Sales Rep Commission % of Sales does not equal 100%, continue?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll.
            ELSE
                MESSAGE "Sales Rep Commission % of Sales is over 100%..."
                    VIEW-AS ALERT-BOX ERROR.
            IF NOT ll THEN 
            DO:
                IF ip-int EQ 3 THEN APPLY "entry" TO oe-ordm.s-pct[3] IN FRAME {&FRAME-NAME}.
                ELSE
                    IF ip-int EQ 2 THEN APPLY "entry" TO oe-ordm.s-pct[2] IN FRAME {&FRAME-NAME}.
                    ELSE APPLY "entry" TO oe-ordm.s-pct[1] IN FRAME {&FRAME-NAME}.
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax Dialog-Frame 
PROCEDURE valid-tax :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF oe-ordm.tax:SCREEN-VALUE EQ "Y" AND
            oe-ord.tax-gr EQ ""                                      THEN 
        DO:
            MESSAGE /*"Order has no tax group! " */
                "Misc. charge can't be taxable if order's not taxable. Make sure order's taxable."
                VIEW-AS ALERT-BOX ERROR.
            oe-ordm.tax:SCREEN-VALUE = "N".
            APPLY "entry" TO oe-ordm.tax.
            RETURN ERROR.     
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-gr Dialog-Frame 
PROCEDURE valid-tax-gr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        IF oe-ordm.spare-char-1:SCREEN-VALUE NE "" AND
            NOT CAN-FIND(FIRST stax
            WHERE stax.company   EQ cocode
            AND stax.tax-group EQ oe-ordm.spare-char-1:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE TRIM(oe-ordm.spare-char-1:LABEL) + " is invalid, try help ..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordm.spare-char-1.
            RETURN ERROR.
        END.
    END.

    {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTaxableMisc Dialog-Frame
FUNCTION fGetTaxableMisc RETURNS LOGICAL 
  ( ipcCompany AS CHARACTER,
    ipcCust AS CHARACTER,
    ipcShipto AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.

    RUN GetTaxableMisc IN hdTaxProcs (ipcCompany, ipcCust, ipcShipto, OUTPUT lTaxable).  
    RETURN lTaxable.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


