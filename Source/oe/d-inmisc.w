&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: oe\d-inmisc.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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

{oe/oe-sysct1.i NEW}

{oe/d-selmis.i NEW}
{sys/inc/ceprepprice.i}


DEFINE            VARIABLE lv-new-recid    AS RECID     NO-UNDO.
DEFINE            VARIABLE lv-valid-charge AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE char-hdl        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE ilogic          AS LOGICAL   NO-UNDO .
DEFINE            VARIABLE lErrorPopClose  AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES inv-misc inv-head

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame inv-misc.charge inv-misc.amt ~
inv-misc.actnum inv-misc.dscr inv-misc.po-no inv-misc.cost ~
inv-misc.inv-i-no inv-misc.inv-line inv-misc.po-no-po inv-misc.s-man[1] ~
inv-misc.s-pct[1] inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] ~
inv-misc.s-comm[2] inv-misc.s-man[3] inv-misc.s-pct[3] inv-misc.s-comm[3] ~
inv-misc.tax inv-misc.spare-char-1 inv-misc.bill inv-misc.spare-char-2 ~
inv-misc.est-no inv-misc.spare-int-1 inv-misc.spare-int-2 inv-misc.ord-no ~
inv-misc.spare-int-3 inv-misc.spare-int-4
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame inv-misc.charge ~
inv-misc.amt inv-misc.actnum inv-misc.dscr inv-misc.po-no inv-misc.cost ~
inv-misc.inv-i-no inv-misc.inv-line inv-misc.po-no-po inv-misc.s-man[1] ~
inv-misc.s-pct[1] inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] ~
inv-misc.s-comm[2] inv-misc.s-man[3] inv-misc.s-pct[3] inv-misc.s-comm[3] ~
inv-misc.tax inv-misc.spare-char-1 inv-misc.bill inv-misc.spare-char-2 ~
inv-misc.est-no inv-misc.spare-int-1 inv-misc.spare-int-2 inv-misc.ord-no ~
inv-misc.spare-int-3 inv-misc.spare-int-4
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame inv-misc
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame inv-misc
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH inv-misc ~
      WHERE inv-misc.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH inv-misc ~
      WHERE inv-misc.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame inv-misc inv-head
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame inv-misc
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame inv-head


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS inv-misc.charge inv-misc.amt inv-misc.actnum ~
inv-misc.dscr inv-misc.po-no inv-misc.cost inv-misc.inv-i-no ~
inv-misc.inv-line inv-misc.po-no-po inv-misc.s-man[1] inv-misc.s-pct[1] ~
inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] inv-misc.s-comm[2] ~
inv-misc.s-man[3] inv-misc.s-pct[3] inv-misc.s-comm[3] inv-misc.tax ~
inv-misc.spare-char-1 inv-misc.bill inv-misc.spare-char-2 inv-misc.est-no ~
inv-misc.spare-int-1 inv-misc.spare-int-2 inv-misc.ord-no ~
inv-misc.spare-int-3 inv-misc.spare-int-4
&Scoped-define ENABLED-TABLES inv-misc
&Scoped-define FIRST-ENABLED-TABLE inv-misc
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS inv-misc.charge inv-misc.amt ~
inv-misc.actnum inv-misc.dscr inv-misc.po-no inv-misc.cost ~
inv-misc.inv-i-no inv-misc.inv-line inv-misc.po-no-po inv-misc.s-man[1] ~
inv-misc.s-pct[1] inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] ~
inv-misc.s-comm[2] inv-misc.s-man[3] inv-misc.s-pct[3] inv-misc.s-comm[3] ~
inv-misc.tax inv-misc.spare-char-1 inv-misc.bill inv-misc.spare-char-2 ~
inv-misc.est-no inv-misc.spare-int-1 inv-misc.spare-int-2 inv-misc.ord-no ~
inv-misc.spare-int-3 inv-misc.spare-int-4
&Scoped-define DISPLAYED-TABLES inv-misc
&Scoped-define FIRST-DISPLAYED-TABLE inv-misc


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTaxableMisc Dialog-Frame 
FUNCTION fGetTaxableMisc RETURNS LOGICAL
    ( ipcCompany AS CHARACTER,
    ipcCust AS CHARACTER,
    ipcShipto AS CHARACTER, 
    ipcPrepCode AS CHARACTER) FORWARD.

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
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 25.8 BY 2.38.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 133.8 BY 12.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    inv-misc, 
    inv-head SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    inv-misc.charge AT ROW 1.52 COL 29.8 COLON-ALIGNED
    LABEL "Charge" FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 28.2 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.amt AT ROW 1.52 COL 79 COLON-ALIGNED
    LABEL "Sell Price" FORMAT "->>,>>>,>>9.99"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.actnum AT ROW 2.62 COL 29.8 COLON-ALIGNED
    LABEL "Account#" FORMAT "x(25)"
    VIEW-AS FILL-IN 
    SIZE 28.2 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.dscr AT ROW 2.62 COL 79 COLON-ALIGNED
    LABEL "Description" FORMAT "x(30)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.po-no AT ROW 3.71 COL 29.8 COLON-ALIGNED
    LABEL "Customer PO#" FORMAT "x(30)"
    VIEW-AS FILL-IN 
    SIZE 28.2 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.cost AT ROW 3.71 COL 79 COLON-ALIGNED
    LABEL "Cost" FORMAT "->>,>>>,>>9.99"
    VIEW-AS FILL-IN 
    SIZE 23.6 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.inv-i-no AT ROW 4.81 COL 29.8 COLON-ALIGNED
    LABEL "Job Number" FORMAT "x(6)"
    VIEW-AS FILL-IN 
    SIZE 19.2 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.inv-line AT ROW 4.81 COL 50.2 COLON-ALIGNED NO-LABELS FORMAT "99"
    VIEW-AS FILL-IN 
    SIZE 5 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.po-no-po AT ROW 4.81 COL 79 COLON-ALIGNED
    LABEL "Vendor PO#" FORMAT ">>>>>>"
    VIEW-AS FILL-IN 
    SIZE 23.6 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-man[1] AT ROW 6.19 COL 29.8 COLON-ALIGNED
    LABEL "Sls Rep" FORMAT "X(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-pct[1] AT ROW 6.19 COL 65.8 COLON-ALIGNED
    LABEL "% of Sale" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-comm[1] AT ROW 6.19 COL 102.8 COLON-ALIGNED
    LABEL "Comm%" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-man[2] AT ROW 7.29 COL 29.8 COLON-ALIGNED
    LABEL "Sls Rep" FORMAT "X(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-pct[2] AT ROW 7.29 COL 65.8 COLON-ALIGNED
    LABEL "% of Sale" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-comm[2] AT ROW 7.29 COL 102.8 COLON-ALIGNED
    LABEL "Comm%" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-man[3] AT ROW 8.38 COL 29.8 COLON-ALIGNED
    LABEL "Sls Rep" FORMAT "X(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-pct[3] AT ROW 8.38 COL 65.8 COLON-ALIGNED
    LABEL "% of Sale" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.s-comm[3] AT ROW 8.38 COL 102.8 COLON-ALIGNED
    LABEL "Comm%" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
    inv-misc.tax AT ROW 9.48 COL 29.8 COLON-ALIGNED
    LABEL "Tax" FORMAT "Y/N"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.spare-char-1 AT ROW 9.48 COL 65.8 COLON-ALIGNED
    LABEL "Tax Prep Code" FORMAT "x(3)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.bill AT ROW 9.48 COL 102.8 COLON-ALIGNED
    LABEL "Bill" FORMAT "X"
    VIEW-AS FILL-IN 
    SIZE 6.4 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.spare-char-2 AT ROW 10.57 COL 29.8 COLON-ALIGNED
    LABEL "FG Item Code" FORMAT "x(15)"
    VIEW-AS FILL-IN 
    SIZE 24.2 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.est-no AT ROW 10.57 COL 65.8 COLON-ALIGNED
    LABEL "Estimate" FORMAT "x(12)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.spare-int-1 AT ROW 10.57 COL 102.8 COLON-ALIGNED 
    LABEL "S" FORMAT ">9"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.spare-int-2 AT ROW 11.67 COL 29.8 COLON-ALIGNED
    LABEL "B" FORMAT ">9"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.ord-no AT ROW 11.67 COL 65.8 COLON-ALIGNED
    LABEL "Order/Line# " FORMAT ">>>>>>>>"
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.spare-int-3 AT ROW 11.67 COL 77.8 COLON-ALIGNED NO-LABELS FORMAT "->,>>>,>>9"
    VIEW-AS FILL-IN 
    SIZE 5 BY 1
    BGCOLOR 15 FONT 1
    inv-misc.spare-int-4 AT ROW 11.67 COL 102.8 COLON-ALIGNED FORMAT ">>>>>"
    LABEL "Line Ref#"
    VIEW-AS FILL-IN 
    SIZE 10 BY 1
    BGCOLOR 15 FONT 1
    Btn_OK AT ROW 14.38 COL 110
    Btn_Done AT ROW 14.62 COL 112
    Btn_Cancel AT ROW 14.38 COL 120
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

/* SETTINGS FOR FILL-IN inv-misc.actnum IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.amt IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.bill IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.charge IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.dscr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.est-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.inv-i-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.inv-line IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.ord-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.po-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.po-no-po IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-comm[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-comm[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-comm[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-man[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-man[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-man[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-pct[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-pct[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.s-pct[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.spare-char-1 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.spare-char-2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.spare-int-1 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.spare-int-2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.spare-int-3 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.tax IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-misc.spare-int-4 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.inv-misc,asi.inv-head "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.inv-misc.company eq cocode "
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
                    RUN windows/l-prep.w (inv-head.company, inv-misc.charge:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val NE "" AND ENTRY(1,char-val) NE inv-misc.charge:SCREEN-VALUE THEN 
                    DO:
                        inv-misc.charge:SCREEN-VALUE = ENTRY(1,char-val).
                        RUN new-charge.
                    END.
                END.
            WHEN "actnum" THEN 
                DO:
                    RUN windows/l-acct2.w (inv-head.company, "", inv-misc.actnum:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN inv-misc.actnum:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            WHEN "inv-i-no" THEN RUN job-help.
            WHEN "inv-line" THEN RUN job-help.
            WHEN "s-man" THEN 
                DO:
                    v-li = FRAME-INDEX.
                    RUN windows/l-sman.w (inv-head.company, OUTPUT char-val).
                    IF char-val NE "" THEN 
                    DO:
                        IF v-li EQ 1 AND inv-misc.s-man[1]:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                            inv-misc.s-man[1]:SCREEN-VALUE = ENTRY(1,char-val).
                        ELSE
                            IF v-li EQ 2 AND inv-misc.s-man[2]:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                                inv-misc.s-man[2]:SCREEN-VALUE = ENTRY(1,char-val).
                            ELSE
                                IF v-li EQ 3 AND inv-misc.s-man[3]:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                                    inv-misc.s-man[3]:SCREEN-VALUE = ENTRY(1,char-val).
                                ELSE v-li = 0.
                        IF v-li NE 0 THEN RUN new-s-man (v-li).
                    END.
                END.
            WHEN "po-no-po" THEN 
                DO:
                    RUN windows/l-ponopo.w (inv-head.company,YES,lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val) .         
                END.
            WHEN "est-no" then do:
                  run windows/l-est2.w (inv-head.company,"",inv-misc.est-no:SCREEN-VALUE, output char-val).
                  if char-val <> "" THEN do:
                      FIND FIRST eb WHERE recid(eb) EQ INTEGER(char-val) NO-LOCK NO-ERROR .
                      IF AVAIL eb THEN
                          assign inv-misc.est-no:SCREEN-VALUE = string(eb.est-no) . 
                  END.
            END.
            when "ord-no" then do:
                  run windows/l-ordl.w (inv-head.company,inv-misc.ord-no:SCREEN-VALUE, output char-val,OUTPUT look-recid).
                  if char-val <> "" then assign inv-misc.est-no:SCREEN-VALUE = entry(1,char-val) .         
             end.
            WHEN "spare-char-2" THEN 
                DO:
                    RUN windows/l-itemfg.w (inv-head.company,"",inv-misc.spare-char-2:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN ASSIGN inv-misc.spare-char-2:SCREEN-VALUE = ENTRY(1,char-val) .         
                END. 
            WHEN "spare-char-1" THEN 
                DO:
                    RUN windows/l-stax.w (inv-head.company,inv-misc.spare-char-1:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN ASSIGN inv-misc.spare-char-1:SCREEN-VALUE = ENTRY(1,char-val) .         
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
        IF AVAILABLE inv-misc THEN
            op-rowid = ROWID(inv-misc) .

        IF lv-item-rowid NE ? THEN 
        DO:
            FIND inv-misc EXCLUSIVE-LOCK
                WHERE ROWID(inv-misc) EQ lv-item-rowid  NO-ERROR.
            IF AVAILABLE inv-misc THEN DELETE inv-misc .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.actnum Dialog-Frame
ON LEAVE OF inv-misc.actnum IN FRAME Dialog-Frame /* Account# */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-actnum(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.bill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.bill Dialog-Frame
ON LEAVE OF inv-misc.bill IN FRAME Dialog-Frame /* Bill */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-bill(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        DISABLE TRIGGERS FOR LOAD OF inv-misc .
        IF AVAILABLE inv-misc THEN
            op-rowid = ROWID(inv-misc) .

        IF lv-item-rowid NE ? THEN 
        DO:

            FIND inv-misc EXCLUSIVE-LOCK
                WHERE ROWID(inv-misc) EQ lv-item-rowid  NO-ERROR.
            IF AVAILABLE inv-misc THEN DELETE inv-misc .
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
        DEFINE VARIABLE ld-prev-amt LIKE inv-misc.amt NO-UNDO.
        DEFINE VARIABLE lCheckError AS LOGICAL   NO-UNDO .

        DO WITH FRAME {&FRAME-NAME}:
            ld-prev-amt = inv-misc.amt.
        END.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
  
        /* ======== validation ===============*/
       
        RUN valid-charge(OUTPUT lCheckError)  NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
        
        RUN valid-actnum(OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
  
        RUN valid-tax( OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-tax-gr(OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-inv-i-no(OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
  
        RUN valid-inv-line(OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-po-no-po(OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
  
        RUN valid-s-man (0,OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-s-pct (0,OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-est(OUTPUT lCheckError) NO-ERROR.
         IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-ord-no(OUTPUT lCheckError) NO-ERROR.
         IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-spare-char-2(OUTPUT lCheckError) NO-ERROR.
         IF lCheckError THEN RETURN NO-APPLY.

        RUN valid-bill(OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
 
        FIND CURRENT inv-misc EXCLUSIVE-LOCK NO-ERROR.
        inv-misc.spare-char-1 = inv-misc.spare-char-1:SCREEN-VALUE .

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
        END.
       
        IF ll-new-record AND inv-misc.cost EQ 0 THEN inv-misc.cost = inv-misc.amt.

        RUN oe/oe-invup.p (ROWID(inv-head), INPUT NO).

        op-rowid = ROWID(inv-misc).
       
        RELEASE inv-misc .

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.charge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.charge Dialog-Frame
ON LEAVE OF inv-misc.charge IN FRAME Dialog-Frame /* Charge */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 
            AND SELF:SCREEN-VALUE NE inv-misc.charge THEN 
        DO:
            RUN valid-charge(OUTPUT lCheckError)  NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.charge Dialog-Frame
ON VALUE-CHANGED OF inv-misc.charge IN FRAME Dialog-Frame /* Charge */
    DO:
        RUN new-charge.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.est-no Dialog-Frame
ON LEAVE OF inv-misc.est-no IN FRAME Dialog-Frame /* Estimate */
    DO:
       DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-est(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME inv-misc.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.ord-no Dialog-Frame
ON LEAVE OF inv-misc.ord-no IN FRAME Dialog-Frame /* Estimate */
    DO:
       DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-ord-no(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME inv-misc.spare-char-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.spare-char-2 Dialog-Frame
ON LEAVE OF inv-misc.spare-char-2 IN FRAME Dialog-Frame /* Estimate */
    DO:
       DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-spare-char-2(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.inv-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.inv-i-no Dialog-Frame
ON LEAVE OF inv-misc.inv-i-no IN FRAME Dialog-Frame /* Job Number */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-inv-i-no(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.inv-line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.inv-line Dialog-Frame
ON LEAVE OF inv-misc.inv-line IN FRAME Dialog-Frame /* Invoice Line */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-inv-line(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.po-no-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.po-no-po Dialog-Frame
ON LEAVE OF inv-misc.po-no-po IN FRAME Dialog-Frame /* Vendor PO# */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-po-no-po(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-comm[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-comm[1] Dialog-Frame
ON ENTRY OF inv-misc.s-comm[1] IN FRAME Dialog-Frame /* Comm% */
    DO:
        IF inv-misc.s-man[1]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-comm[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-comm[2] Dialog-Frame
ON ENTRY OF inv-misc.s-comm[2] IN FRAME Dialog-Frame /* Comm% */
    DO:
        IF inv-misc.s-man[2]:SCREEN-VALUE EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-comm[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-comm[3] Dialog-Frame
ON ENTRY OF inv-misc.s-comm[3] IN FRAME Dialog-Frame /* Comm% */
    DO:
        IF inv-misc.s-man[3]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-man[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[1] Dialog-Frame
ON LEAVE OF inv-misc.s-man[1] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (1,OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[1] Dialog-Frame
ON VALUE-CHANGED OF inv-misc.s-man[1] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        RUN new-s-man (1).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-man[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[2] Dialog-Frame
ON ENTRY OF inv-misc.s-man[2] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        IF inv-misc.s-man[1]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[2] Dialog-Frame
ON LEAVE OF inv-misc.s-man[2] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (2,OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[2] Dialog-Frame
ON VALUE-CHANGED OF inv-misc.s-man[2] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        RUN new-s-man (2).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-man[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[3] Dialog-Frame
ON ENTRY OF inv-misc.s-man[3] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        IF inv-misc.s-man[2]:SCREEN-VALUE EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[3] Dialog-Frame
ON LEAVE OF inv-misc.s-man[3] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (3,OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-man[3] Dialog-Frame
ON VALUE-CHANGED OF inv-misc.s-man[3] IN FRAME Dialog-Frame /* Sls Rep */
    DO:
        RUN new-s-man (3).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-pct[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-pct[1] Dialog-Frame
ON ENTRY OF inv-misc.s-pct[1] IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF inv-misc.s-man[1]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-pct[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-pct[2] Dialog-Frame
ON ENTRY OF inv-misc.s-pct[2] IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF inv-misc.s-man[2]:SCREEN-VALUE  EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.s-pct[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.s-pct[3] Dialog-Frame
ON ENTRY OF inv-misc.s-pct[3] IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF inv-misc.s-man[3]:SCREEN-VALUE EQ "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = "".
            APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-misc.tax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-misc.tax Dialog-Frame
ON LEAVE OF inv-misc.tax IN FRAME Dialog-Frame /* Tax */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-tax(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN  NO-APPLY .
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
        
    FIND inv-head NO-LOCK
        WHERE inv-head.company EQ cocode
        AND ROWID(inv-head)  EQ ip-rowid2 NO-ERROR .

    IF ip-type EQ "copy" THEN lv-item-rowid = ip-rowid.


    IF ip-rowid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND inv-misc NO-LOCK WHERE ROWID(inv-misc) EQ ip-rowid NO-ERROR.

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

    IF lErrorPopClose THEN DO:
        APPLY "choose" TO Btn_Cancel IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY .
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
    DEFINE BUFFER bf-misc FOR inv-misc.
    DEFINE VARIABLE z        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-line  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-fgitem AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inv-line FOR inv-line .
    
    i = 0 .
    FOR EACH bf-inv-line OF inv-head NO-LOCK:
        i = i + 1.
    END.

    IF i GT 1 THEN 
    DO:
        RUN oe/mis-invfg.w (RECID(inv-head),OUTPUT v-fgitem,OUTPUT lErrorPopClose ) NO-ERROR.
    END.

    /* Code placed here will execute PRIOR to standard behavior. */
    FIND LAST bf-misc WHERE bf-misc.r-no = inv-head.r-no NO-LOCK NO-ERROR.
    z = IF AVAILABLE bf-misc THEN bf-misc.LINE + 1 ELSE 1.
  
    CREATE inv-misc.
    ASSIGN 
        inv-misc.r-no    = inv-head.r-no
        inv-misc.company = inv-head.company
        inv-misc.LINE    = z
        inv-misc.bill    = "Y"
        lv-item-rowid    = ROWID(inv-misc).
    ll-new-record = YES.

    /* Code placed here will execute AFTER standard behavior.    */
    lv-new-recid = RECID(inv-misc).
    FIND CURRENT inv-misc EXCLUSIVE-LOCK.
    ASSIGN
        inv-misc.s-man[1]  = inv-head.sman[1]
        inv-misc.s-pct[1]  = inv-head.s-pct[1]
        inv-misc.s-comm[1] = inv-head.s-comm[1]
        inv-misc.s-man[2]  = inv-head.sman[2]
        inv-misc.s-pct[2]  = inv-head.s-pct[2]
        inv-misc.s-comm[2] = inv-head.s-comm[2]
        inv-misc.s-man[3]  = inv-head.sman[3]
        inv-misc.s-pct[3]  = inv-head.s-pct[3]
        inv-misc.s-comm[3] = inv-head.s-comm[3] .

    FIND FIRST ar-ctrl WHERE ar-ctrl.company = inv-head.company NO-LOCK NO-ERROR.
    IF AVAILABLE ar-ctrl THEN inv-misc.actnum = ar-ctrl.sales.
    FIND FIRST cust OF inv-head NO-LOCK.
    inv-misc.tax = cust.SORT = "Y" AND inv-head.tax-gr <> "".

    FIND FIRST prep NO-LOCK 
        WHERE prep.company EQ inv-head.company 
        AND prep.code    EQ inv-misc.charge:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-ERROR.
    IF AVAILABLE prep AND NOT prep.commissionable THEN
        ASSIGN 
            inv-misc.s-comm[1] = 0 
            inv-misc.s-comm[2] = 0
            inv-misc.s-comm[3] = 0
            .
        
    ASSIGN 
        inv-misc.spare-char-1 = inv-head.tax-gr
        inv-misc.tax          = fGetTaxableMisc(cocode, inv-head.cust-no, inv-head.sold-no, inv-misc.charge:SCREEN-VALUE IN FRAME {&FRAME-NAME}) .
  
    IF i = 1 THEN 
    DO:
        IF AVAILABLE inv-head THEN
            FIND FIRST bf-inv-line OF inv-head NO-LOCK NO-ERROR.
        IF AVAILABLE bf-inv-line THEN
            ASSIGN
                inv-misc.spare-char-2 = bf-inv-line.i-no 
                inv-misc.inv-i-no     = bf-inv-line.job-no
                inv-misc.inv-line     = bf-inv-line.job-no2 
                inv-misc.ord-no       = bf-inv-line.ord-no
                inv-misc.est-no       = bf-inv-line.est-no 
                inv-misc.spare-int-1  = bf-inv-line.form-no
                inv-misc.spare-int-2  = bf-inv-line.blank-no.
    END.
    ELSE 
    DO:
        ASSIGN
            inv-misc.spare-char-2 = v-fgitem .
        IF AVAILABLE inv-head THEN
            FIND FIRST bf-inv-line OF inv-head 
                WHERE bf-inv-line.i-no EQ v-fgitem NO-LOCK NO-ERROR.
        IF AVAILABLE bf-inv-line THEN
            ASSIGN
                inv-misc.inv-i-no    = bf-inv-line.job-no
                inv-misc.inv-line    = bf-inv-line.job-no2 
                inv-misc.ord-no      = bf-inv-line.ord-no
                inv-misc.est-no      = bf-inv-line.est-no
                inv-misc.spare-int-1 = bf-inv-line.form-no
                inv-misc.spare-int-2 = bf-inv-line.blank-no   .
        
    END.
    
    FIND CURRENT inv-misc NO-LOCK NO-ERROR.   

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
   
    IF AVAILABLE inv-misc  THEN 
    DO:
        DISPLAY  inv-misc.charge inv-misc.amt 
            inv-misc.actnum inv-misc.dscr inv-misc.po-no inv-misc.cost inv-misc.inv-i-no 
            inv-misc.inv-line inv-misc.po-no-po inv-misc.s-man[1] inv-misc.s-pct[1] 
            inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] inv-misc.s-comm[2] 
            inv-misc.s-man[3] inv-misc.s-pct[3] inv-misc.s-comm[3] inv-misc.tax 
            inv-misc.spare-char-1 inv-misc.bill inv-misc.spare-int-1 inv-misc.spare-char-2 
            inv-misc.est-no inv-misc.spare-int-1 inv-misc.spare-int-2 inv-misc.ord-no inv-misc.spare-int-3
            inv-misc.spare-int-4
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
    IF AVAILABLE inv-misc THEN 
        DISPLAY inv-misc.charge inv-misc.amt inv-misc.actnum inv-misc.dscr 
            inv-misc.po-no inv-misc.cost inv-misc.inv-i-no inv-misc.inv-line 
            inv-misc.po-no-po inv-misc.s-man[1] inv-misc.s-pct[1] 
            inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] 
            inv-misc.s-comm[2] inv-misc.s-man[3] inv-misc.s-pct[3] 
            inv-misc.s-comm[3] inv-misc.tax inv-misc.spare-char-1 inv-misc.bill 
            inv-misc.spare-char-2 inv-misc.est-no inv-misc.spare-int-1 
            inv-misc.spare-int-2 inv-misc.ord-no inv-misc.spare-int-3 
            inv-misc.spare-int-4
            WITH FRAME Dialog-Frame.
    ENABLE inv-misc.charge inv-misc.amt inv-misc.actnum inv-misc.dscr 
        inv-misc.po-no inv-misc.cost inv-misc.inv-i-no inv-misc.inv-line 
        inv-misc.po-no-po inv-misc.s-man[1] inv-misc.s-pct[1] 
        inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] 
        inv-misc.s-comm[2] inv-misc.s-man[3] inv-misc.s-pct[3] 
        inv-misc.s-comm[3] inv-misc.tax inv-misc.spare-char-1 inv-misc.bill 
        inv-misc.spare-char-2 inv-misc.est-no inv-misc.spare-int-1 
        inv-misc.spare-int-2 inv-misc.ord-no inv-misc.spare-int-3
        inv-misc.spare-int-4 Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
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
        RUN windows/l-jobno.w (cocode, inv-misc.inv-i-no:SCREEN-VALUE , OUTPUT char-val, OUTPUT look-recid).
        IF look-recid NE ? THEN 
        DO:
            FIND job-hdr WHERE RECID(job-hdr) EQ look-recid NO-LOCK NO-ERROR.
            IF AVAILABLE job-hdr THEN 
                ASSIGN
                    inv-misc.inv-i-no:SCREEN-VALUE = job-hdr.job-no
                    inv-misc.inv-line:SCREEN-VALUE = STRING(job-hdr.job-no2).                      
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
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST prep
            WHERE prep.company EQ cocode
            AND prep.code    EQ inv-misc.charge:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF AVAILABLE prep THEN 
        DO:
            inv-misc.tax:SCREEN-VALUE = STRING(prep.taxable).
            inv-misc.dscr:SCREEN-VALUE  = prep.dscr.

            FIND FIRST account
                WHERE account.company EQ prep.company
                AND account.actnum  EQ prep.actnum
                AND account.type    EQ "R"
                NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN inv-misc.actnum:SCREEN-VALUE  = prep.actnum.
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
            WHERE prep.company EQ inv-head.company 
            AND prep.code    EQ inv-misc.charge:SCREEN-VALUE
            NO-LOCK NO-ERROR.

        IF AVAILABLE prep AND prep.commissionable THEN 
        DO:             
        
            DO v-li = 1 TO IF ip-int EQ 0 THEN 3 ELSE ip-int:
                lv = IF v-li EQ 1 THEN inv-misc.s-man[1]:SCREEN-VALUE ELSE
                    IF v-li EQ 2 THEN inv-misc.s-man[2]:SCREEN-VALUE ELSE
                    inv-misc.s-man[3]:SCREEN-VALUE.

                IF lv NE "" THEN 
                DO:
                    RUN sys/inc/getsmncm.p (inv-head.cust-no,
                        INPUT-OUTPUT lv,
                        IF AVAILABLE prep THEN prep.fgcat ELSE "",
                        0,
                        OUTPUT ld).          

                    CASE v-li:
                        WHEN 1 THEN 
                            inv-misc.s-comm[1]:SCREEN-VALUE = STRING(ld).
                        WHEN 2 THEN 
                            inv-misc.s-comm[2]:SCREEN-VALUE = STRING(ld).
                        WHEN 3 THEN 
                            inv-misc.s-comm[3]:SCREEN-VALUE = STRING(ld).
                    END CASE.
                END.
            END.
        END.  /*IF AVAIL oe-ctrl AND oe-ctrl.prep-comm EQ YES THEN do:  */           
        ELSE
            ASSIGN 
                inv-misc.s-comm[1]:SCREEN-VALUE = '0'
                inv-misc.s-comm[2]:SCREEN-VALUE = '0'
                inv-misc.s-comm[3]:SCREEN-VALUE = '0'
                . 
            
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
        lv-sman = IF ip-int EQ 3 THEN inv-misc.s-man[3]:SCREEN-VALUE 
        ELSE
            IF ip-int EQ 2 THEN inv-misc.s-man[2]:SCREEN-VALUE
            ELSE inv-misc.s-man[1]:SCREEN-VALUE.

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
                    IF DECIMAL(inv-misc.s-pct[3]:SCREEN-VALUE) EQ 0 THEN
                        inv-misc.s-pct[3]:SCREEN-VALUE = "100".
                    IF DECIMAL(inv-misc.s-comm[3]:SCREEN-VALUE) EQ 0 THEN 
                    DO:
                        FIND FIRST prep
                            WHERE prep.company EQ inv-head.company 
                            AND prep.code    EQ inv-misc.charge:SCREEN-VALUE
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE prep AND prep.commissionable THEN        /*Task# 11271302*/
                            inv-misc.s-comm[3]:SCREEN-VALUE = STRING(sman.scomm).
                    END.

                    RUN new-comm (3).
                END.
                ELSE
                    IF ip-int EQ 2 THEN 
                    DO:
                        IF DECIMAL(inv-misc.s-pct[2]:SCREEN-VALUE) EQ 0 THEN
                            inv-misc.s-pct[2]:SCREEN-VALUE = "100".
                        IF DECIMAL(inv-misc.s-comm[2]:SCREEN-VALUE) EQ 0 THEN 
                        DO:
                            FIND FIRST prep
                                WHERE prep.company EQ inv-head.company 
                                AND prep.code    EQ inv-misc.charge:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE prep AND prep.commissionable THEN        /*Task# 11271302*/
                                inv-misc.s-comm[2]:SCREEN-VALUE = STRING(sman.scomm).
                        END.

                        RUN new-comm (2).
                    END.
                    ELSE 
                    DO:
                        IF DECIMAL(inv-misc.s-pct[1]:SCREEN-VALUE) EQ 0 THEN
                            inv-misc.s-pct[1]:SCREEN-VALUE = "100".
                        IF DECIMAL(inv-misc.s-comm[1]:SCREEN-VALUE) EQ 0 THEN 
                        DO:
                            FIND FIRST prep
                                WHERE prep.company EQ inv-head.company 
                                AND prep.code    EQ inv-misc.charge:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
                            
                            IF AVAILABLE prep AND prep.commissionable THEN        /*Task# 11271302*/
                                inv-misc.s-comm[1]:SCREEN-VALUE = STRING(sman.scomm).
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
            inv-misc.s-pct[1]:VISIBLE IN FRAME {&FRAME-NAME}  = ip-visible
            inv-misc.s-pct[2]:VISIBLE IN FRAME {&FRAME-NAME}  = ip-visible
            inv-misc.s-pct[3]:VISIBLE IN FRAME {&FRAME-NAME}  = ip-visible
            inv-misc.s-comm[1]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
            inv-misc.s-comm[2]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
            inv-misc.s-comm[3]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible.
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST account WHERE account.company EQ cocode
            AND account.actnum  EQ inv-misc.actnum:SCREEN-VALUE 
            /*AND account.type    EQ "R"*/) THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO inv-misc.actnum .
            opReturnError = YES .
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-DO("Y,N,I",inv-misc.bill:SCREEN-VALUE)       
            THEN 
        DO:
            MESSAGE "Invalid entry. Enter (Y)es, (N)o, or (I)nvoiced." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO inv-misc.bill IN FRAME {&FRAME-NAME}.
            opReturnError = YES .
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF inv-misc.charge:SCREEN-VALUE  EQ "" /*OR 
       NOT CAN-FIND(FIRST prep WHERE prep.company EQ cocode
                                 AND prep.code    EQ inv-misc.charge:SCREEN-VALUE IN BROWSE {&browse-name})*/
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO inv-misc.charge .
            opReturnError = YES .
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
     DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
     DEFINE VARIABLE lv-est-no AS CHARACTER NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
       ASSIGN 
           lv-est-no                    = TRIM(inv-misc.est-no:SCREEN-VALUE )
           lv-est-no                    = FILL(" ", 8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no)
           inv-misc.est-no:SCREEN-VALUE = lv-est-no.

       IF inv-misc.est-no:SCREEN-VALUE NE "" THEN DO:
           FIND FIRST eb NO-LOCK
               WHERE eb.company EQ cocode
               AND eb.est-no EQ inv-misc.est-no:SCREEN-VALUE NO-ERROR.

           IF NOT AVAILABLE eb THEN DO:
               MESSAGE "Estimate is not valid..."
                   VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO inv-misc.est-no.
               opReturnError = YES .
           END.
       END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no Dialog-Frame 
PROCEDURE valid-ord-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
     DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    
    DO WITH FRAME {&FRAME-NAME}:
       
       IF inv-misc.ord-no:SCREEN-VALUE NE "" AND inv-misc.ord-no:SCREEN-VALUE NE "0" THEN DO:
           FIND FIRST oe-ord NO-LOCK
               WHERE oe-ord.company EQ cocode
               AND oe-ord.ord-no EQ INTEGER(inv-misc.ord-no:SCREEN-VALUE) NO-ERROR.

           IF NOT AVAILABLE oe-ord THEN DO:
               MESSAGE "Order is not valid..."
                   VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO inv-misc.ord-no .
               opReturnError = YES .
           END.
       END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-spare-char-2 Dialog-Frame 
PROCEDURE valid-spare-char-2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
     DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    
    DO WITH FRAME {&FRAME-NAME}:
       
       IF inv-misc.spare-char-2:SCREEN-VALUE NE "" THEN DO:
           FIND FIRST itemfg NO-LOCK
               WHERE itemfg.company EQ cocode
               AND itemfg.i-no EQ inv-misc.spare-char-2:SCREEN-VALUE NO-ERROR.

           IF NOT AVAILABLE itemfg THEN DO:
               MESSAGE "FG Item is not valid..."
                   VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO inv-misc.spare-char-2 .
               opReturnError = YES .
           END.
       END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-i-no Dialog-Frame 
PROCEDURE valid-inv-i-no :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes: inv-i-no is used for job-no
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-job-no LIKE job.job-no NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-job-no                      = inv-misc.inv-i-no:SCREEN-VALUE
            lv-job-no                      = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no)
            inv-misc.inv-i-no:SCREEN-VALUE = lv-job-no.

        IF lv-job-no NE "" THEN 
        DO:
            FIND FIRST job NO-LOCK
                WHERE job.company EQ cocode
                AND job.job-no  EQ lv-job-no
                NO-ERROR.
            IF NOT AVAILABLE job THEN 
            DO:
                MESSAGE TRIM(inv-misc.inv-i-no:LABEL IN FRAME {&FRAME-NAME}) +
                    " is invalid..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO inv-misc.inv-i-no IN FRAME {&FRAME-NAME}.
                opReturnError = YES .
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-line Dialog-Frame 
PROCEDURE valid-inv-line :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-job-no LIKE job.job-no NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-job-no                      = inv-misc.inv-i-no:SCREEN-VALUE
            lv-job-no                      = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no)
            inv-misc.inv-i-no:SCREEN-VALUE = lv-job-no.

        IF lv-job-no NE "" THEN 
        DO:
            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ lv-job-no
                AND job.job-no2 EQ INT(inv-misc.inv-line:SCREEN-VALUE)
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job THEN 
            DO:
                MESSAGE TRIM(inv-misc.inv-i-no:LABEL IN FRAME {&FRAME-NAME}) +
                    " is invalid..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO inv-misc.inv-line IN FRAME {&FRAME-NAME}.
                opReturnError = YES .
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(inv-misc.po-no-po:SCREEN-VALUE) NE 0 AND
            NOT CAN-FIND(FIRST po-ord
            WHERE po-ord.company EQ inv-head.company 
            AND po-ord.po-no   EQ INTEGER(inv-misc.po-no-po:SCREEN-VALUE))
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO inv-misc.po-no-po IN FRAME {&FRAME-NAME}.
            opReturnError = YES .
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE v-li    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-sman LIKE sman.sman NO-UNDO.

    v-li = ip-int.

    IF v-li EQ 0 THEN
        ASSIGN
            ip-int = 1
            v-li   = 3.

    DO ip-int = ip-int TO v-li WITH FRAME {&FRAME-NAME}:
        lv-sman = IF ip-int EQ 3 THEN inv-misc.s-man[3]:SCREEN-VALUE
        ELSE
            IF ip-int EQ 2 THEN inv-misc.s-man[2]:SCREEN-VALUE
            ELSE inv-misc.s-man[1]:SCREEN-VALUE.
    
        IF lv-sman NE "" THEN 
        DO:
            IF NOT CAN-FIND(FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ lv-sman) THEN 
            DO:
                MESSAGE "Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
                IF ip-int EQ 3 THEN APPLY "entry" TO inv-misc.s-man[3] IN FRAME {&FRAME-NAME}.
                ELSE
                    IF ip-int EQ 2 THEN APPLY "entry" TO inv-misc.s-man[2] IN FRAME {&FRAME-NAME}.
                    ELSE APPLY "entry" TO inv-misc.s-man[1] IN FRAME {&FRAME-NAME}.
                opReturnError = YES .
            END.
        END.

        ELSE 
        DO:
            IF ip-int EQ 3 THEN
                ASSIGN
                    inv-misc.s-pct[3]:SCREEN-VALUE  = "0"
                    inv-misc.s-comm[3]:SCREEN-VALUE = "0".
            ELSE
                IF ip-int EQ 2 THEN
                    ASSIGN
                        inv-misc.s-pct[2]:SCREEN-VALUE  = "0"
                        inv-misc.s-comm[2]:SCREEN-VALUE = "0".
                ELSE
                    ASSIGN
                        inv-misc.s-pct[1]:SCREEN-VALUE  = "0"
                        inv-misc.s-comm[1]:SCREEN-VALUE = "0".
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE ld-pct AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ll     AS LOGICAL NO-UNDO.

   
    DO WITH FRAME {&FRAME-NAME}:
        ld-pct = IF ip-int EQ 1 THEN DEC(inv-misc.s-pct[1]:SCREEN-VALUE )
        ELSE
            IF ip-int EQ 2 THEN DEC(inv-misc.s-pct[2]:SCREEN-VALUE )
            ELSE
            IF ip-int EQ 3 THEN DEC(inv-misc.s-pct[3]:SCREEN-VALUE )
            ELSE (DEC(inv-misc.s-pct[1]:SCREEN-VALUE ) +
            DEC(inv-misc.s-pct[2]:SCREEN-VALUE ) +
            DEC(inv-misc.s-pct[3]:SCREEN-VALUE )).

        IF (inv-misc.s-man[1]:SCREEN-VALUE  NE "" OR
            inv-misc.s-man[2]:SCREEN-VALUE  NE "" OR
            inv-misc.s-man[3]:SCREEN-VALUE  NE "")   AND
            ((ip-int EQ 0 AND ld-pct LT 100) OR
            (ip-int NE 0 AND ld-pct GT 100)) THEN 
        DO:
            MESSAGE "% of Sales for all sales reps must total 100..." VIEW-AS ALERT-BOX INFORMATION.
            IF ip-int EQ 3 THEN APPLY "entry" TO inv-misc.s-pct[3] .
            ELSE
                IF ip-int EQ 2 THEN APPLY "entry" TO inv-misc.s-pct[2] .
                ELSE APPLY "entry" TO inv-misc.s-pct[1] .
            opReturnError = YES .
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF inv-misc.tax:SCREEN-VALUE EQ "Y" 
            AND inv-head.tax-gr EQ "" THEN 
        DO:
            MESSAGE /*"Order has no tax group! " */
                "Misc. charge cannot be taxable if the Invoice is not taxable." SKIP 
                "Ensure that the Invoice is taxable (has a valid tax code)." SKIP 
                "Normally the tax code will be pulled from the Ship To ID or" SKIP 
                "from the Customer record when the order is created."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN 
                inv-misc.tax:SCREEN-VALUE = "N".
            APPLY "entry" TO inv-misc.tax.
            opReturnError = YES . 
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
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        IF inv-misc.spare-char-1:SCREEN-VALUE NE "" AND
            NOT CAN-FIND(FIRST stax
            WHERE stax.company   EQ cocode
            AND stax.tax-group EQ inv-misc.spare-char-1:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE TRIM(inv-misc.spare-char-1:LABEL) + " is invalid, try help ..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO inv-misc.spare-char-1.
            opReturnError = YES .
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
    ipcShipto AS CHARACTER, 
    ipcPrepCode AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.

    RUN Tax_GetTaxableMisc  (ipcCompany, ipcCust, ipcShipto, ipcPrepCode, OUTPUT lTaxable).  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

