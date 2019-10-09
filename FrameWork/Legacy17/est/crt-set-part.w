&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\crt-set-part.w

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
def input param ip-rowid as rowid no-undo.
DEF INPUT PARAM ip-last-cat AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

def new shared buffer xest for est.
def new shared buffer xeb for eb.
def new shared buffer xef for ef.

{cec/tt-eb-set-part.i}

DEF VAR ll-crt-itemfg AS LOG INIT NO NO-UNDO.
DEF VAR k_frac AS DEC INIT "6.25" NO-UNDO. 
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

{cec/msfcalc.i}

DO TRANSACTION:
   {sys/inc/cepartition.i}
END.

{sys/inc/setprint.i}
{sys/inc/f16to32.i}


IF v-cecscrn-dec THEN
DO:
   DEF TEMP-TABLE tt-64-dec NO-UNDO
       FIELD DEC AS DEC DECIMALS 6.

   DO v-count = 0 TO 63:
       CREATE tt-64-dec.
       tt-64-dec.DEC = v-count / 64.0.
       RELEASE tt-64-dec.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH eb SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH eb SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog eb
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog eb


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 fi_stock-no fi_part-no fi_part-dscr1 ~
fi_part-dscr2 fi_procat fi_len scr-style-1 scr-end-cell-l1 ~
scr-in-cell-length scr-end-cell-l2 fi_wid scr-style-2 scr-end-cell-w1 ~
scr-in-cell-width scr-end-cell-w2 fi_height scr-est-qty scr-board ~
cb_rev-corr rd_alloc scr-no-forms tb_unitize Btn_SAVE 
&Scoped-Define DISPLAYED-OBJECTS fi_stock-no fi_part-no fi_part-dscr1 ~
fi_part-dscr2 fi_procat fi_len scr-style-1 scr-end-cell-l1 ~
scr-in-cell-length scr-end-cell-l2 fi_wid scr-style-2 scr-end-cell-w1 ~
scr-in-cell-width scr-end-cell-w2 fi_height scr-est-qty scr-board ~
cb_rev-corr rd_alloc scr-no-forms tb_unitize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 rd_alloc 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_SAVE AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cb_rev-corr AS CHARACTER FORMAT "X":U 
     LABEL "Rev. Corr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "N","B","S" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fi_height AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Height" 
     VIEW-AS FILL-IN 
     SIZE 13.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_len AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "FG Length" 
     VIEW-AS FILL-IN 
     SIZE 13.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_part-dscr1 AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_part-dscr2 AS CHARACTER FORMAT "x(30)" 
     LABEL "Part Description" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "x(15)" 
     LABEL "Set Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_procat AS CHARACTER FORMAT "x(5)" 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_stock-no AS CHARACTER FORMAT "x(15)" 
     LABEL "Set FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_wid AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 13.8 BY 1 NO-UNDO.

DEFINE VARIABLE scr-board AS CHARACTER FORMAT "X(10)":U 
     LABEL "Board" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE scr-end-cell-l1 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-end-cell-l2 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-end-cell-w1 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-end-cell-w2 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-est-qty AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Est. Qty" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE scr-in-cell-length AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-in-cell-width AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-no-forms AS INTEGER FORMAT ">9":U INITIAL 2 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE scr-style-1 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE scr-style-2 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE rd_alloc AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Assembled", NO,
"Unassembled", YES,
"Assembled w/Part Receipts", ?
     SIZE 41 BY 2.62 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97.6 BY 14.91.

DEFINE VARIABLE tb_unitize AS LOGICAL INITIAL yes 
     LABEL "Unitize?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi_stock-no AT ROW 1.48 COL 20.2 COLON-ALIGNED
     fi_part-no AT ROW 2.48 COL 20.2 COLON-ALIGNED
     fi_part-dscr1 AT ROW 3.48 COL 20.2 COLON-ALIGNED
     fi_part-dscr2 AT ROW 4.48 COL 20.2 COLON-ALIGNED
     fi_procat AT ROW 5.48 COL 20.2 COLON-ALIGNED
     fi_len AT ROW 6.48 COL 20.2 COLON-ALIGNED
     scr-style-1 AT ROW 6.48 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     scr-end-cell-l1 AT ROW 6.48 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     scr-in-cell-length AT ROW 6.48 COL 61.2 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     scr-end-cell-l2 AT ROW 6.48 COL 78.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     fi_wid AT ROW 7.48 COL 20.2 COLON-ALIGNED
     scr-style-2 AT ROW 7.48 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     scr-end-cell-w1 AT ROW 7.48 COL 44.2 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     scr-in-cell-width AT ROW 7.48 COL 61.2 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     scr-end-cell-w2 AT ROW 7.48 COL 78.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fi_height AT ROW 8.48 COL 20.2 COLON-ALIGNED
     scr-est-qty AT ROW 9.48 COL 20.2 COLON-ALIGNED WIDGET-ID 28
     scr-board AT ROW 10.48 COL 20.2 COLON-ALIGNED WIDGET-ID 18
     cb_rev-corr AT ROW 11.48 COL 20.2 COLON-ALIGNED WIDGET-ID 44
     rd_alloc AT ROW 11.71 COL 43 NO-LABEL WIDGET-ID 36
     scr-no-forms AT ROW 12.81 COL 20.2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     tb_unitize AT ROW 14.33 COL 43 WIDGET-ID 40
     Btn_SAVE AT ROW 16.14 COL 42.2
     "Form(s)" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.95 COL 28.8 WIDGET-ID 4
     "End Cell" VIEW-AS TEXT
          SIZE 10.2 BY .62 AT ROW 5.67 COL 46.6 WIDGET-ID 8
     "Slot" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 5.67 COL 36.6 WIDGET-ID 6
     "End Cell" VIEW-AS TEXT
          SIZE 11.2 BY .62 AT ROW 5.67 COL 80.8 WIDGET-ID 34
     "Interior Cell" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5.67 COL 63.6 WIDGET-ID 10
     RECT-5 AT ROW 1 COL 1
     SPACE(0.39) SKIP(2.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Assembled Partition"
         DEFAULT-BUTTON Btn_SAVE.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET rd_alloc IN FRAME D-Dialog
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "ASI.eb"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Assembled Partition */
DO:
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_SAVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SAVE D-Dialog
ON CHOOSE OF Btn_SAVE IN FRAME D-Dialog /* Save */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   do with frame {&frame-name}:
      ASSIGN fi_stock-no
             fi_part-no
             fi_part-dscr1
             fi_part-dscr2
             fi_procat
             fi_len
             fi_wid
             fi_height
             scr-no-forms
             scr-style-1
             scr-style-2
             scr-board
             scr-est-qty
             scr-end-cell-l1
             scr-end-cell-l2
             scr-in-cell-length
             scr-in-cell-width
             scr-end-cell-w1
             scr-end-cell-w2
             rd_alloc
             tb_unitize
             cb_rev-corr.

      RUN valid-stock-no(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      RUN valid-part-no(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      RUN valid-procat(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      RUN valid-style(input scr-style-1,input 1,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      RUN valid-style(input scr-style-2, input 2,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      RUN valid-board(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      RUN valid-sizes(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      IF scr-est-qty EQ 0 THEN
      DO:
         MESSAGE "Invalid Est. Qty."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN NO-APPLY.
      END.

      RUN valid-no-forms(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.

      create tt-eb-set-part.
      assign tt-eb-set-part.est-type = 6
             tt-eb-set-part.company = est.company
             tt-eb-set-part.loc = est.loc
             tt-eb-set-part.est-no = est.est-no
             tt-eb-set-part.form-no = 0
             tt-eb-set-part.blank-no = 0
             tt-eb-set-part.est-int = int(est.est-no)
             tt-eb-set-part.stock-no = fi_stock-no
             tt-eb-set-part.part-no = fi_part-no
             tt-eb-set-part.header-part-dscr1 = fi_part-dscr1
             tt-eb-set-part.header-part-dscr2 = fi_part-dscr2
             tt-eb-set-part.header-stock-no   = fi_stock-no
             tt-eb-set-part.part-dscr1 = fi_part-dscr1 + " Long"
             tt-eb-set-part.part-dscr2 = fi_part-dscr2 + " Long"
             tt-eb-set-part.part-dscr3 = fi_part-dscr1 + " Short"
             tt-eb-set-part.part-dscr4 = fi_part-dscr2 + " Short"
             tt-eb-set-part.len = fi_len
             tt-eb-set-part.wid = fi_wid
             tt-eb-set-part.dep = fi_height
             tt-eb-set-part.procat = fi_procat
             tt-eb-set-part.set-is-assembled = YES
             tt-eb-set-part.pur-man = YES
             tt-eb-set-part.board = scr-board
             tt-eb-set-part.style-1 = scr-style-1
             tt-eb-set-part.style-2 = scr-style-2
             tt-eb-set-part.end-cell-length-1 = scr-end-cell-l1
             tt-eb-set-part.end-cell-length-2 = scr-end-cell-l2
             tt-eb-set-part.end-cell-width-1 = scr-end-cell-w1
             tt-eb-set-part.end-cell-width-2 = scr-end-cell-w2
             tt-eb-set-part.in-cell-length = scr-in-cell-length
             tt-eb-set-part.in-cell-width = scr-in-cell-width
             tt-eb-set-part.num-forms = scr-no-forms
             tt-eb-set-part.est-qty = scr-est-qty
             tt-eb-set-part.set-is-assembled = rd_alloc
             tt-eb-set-part.pur-man = tb_unitize
             tt-eb-set-part.grain = cb_rev-corr.

      IF tt-eb-set-part.set-is-assembled NE ? THEN
         tt-eb-set-part.set-is-assembled = NOT tt-eb-set-part.set-is-assembled.

      FIND FIRST style WHERE
           style.company EQ est.company AND
           style.style EQ scr-style-1
           NO-LOCK NO-ERROR.

      IF AVAIL style THEN
         tt-eb-set-part.qty-set-2 = style.dim-df.

      FIND FIRST style WHERE
           style.company EQ est.company AND
           style.style EQ scr-style-2
           NO-LOCK NO-ERROR.

      IF AVAIL style THEN
         tt-eb-set-part.qty-set-1 = style.dim-df.

      RELEASE tt-eb-set-part.
   end.

   apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_height
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_height D-Dialog
ON LEAVE OF fi_height IN FRAME D-Dialog /* Height */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   v-dec = decimal(fi_height:screen-value) - trunc(decimal(fi_height:screen-value),0).

   IF v-dec >= v-16-or-32 THEN
   do:
      message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
         view-as alert-box error.
      RETURN NO-APPLY.
   END.

   IF v-cecscrn-dec THEN
   DO:
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_len D-Dialog
ON LEAVE OF fi_len IN FRAME D-Dialog /* FG Length */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   v-dec = decimal(fi_len:screen-value) - trunc(decimal(fi_len:screen-value),0).
   IF v-dec >= v-16-or-32 THEN
   do:
      message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
         view-as alert-box error.
      RETURN NO-APPLY.
   END.

   IF v-cecscrn-dec THEN
   DO:
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).

      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-dscr1 D-Dialog
ON HELP OF fi_part-dscr1 IN FRAME D-Dialog /* Item Name */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 4, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no D-Dialog
ON HELP OF fi_part-no IN FRAME D-Dialog /* Set Cust Part# */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 3, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no D-Dialog
ON LEAVE OF fi_part-no IN FRAME D-Dialog /* Set Cust Part# */
DO:
  DEF VAR op-error AS LOG NO-UNDO.
  IF LASTKEY NE -1 THEN DO:
     RUN valid-part-no(OUTPUT op-error).
     IF op-error THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_procat D-Dialog
ON HELP OF fi_procat IN FRAME D-Dialog /* Category */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   run windows/l-fgcat.w (est.company,focus:screen-value, output char-val).
   if char-val <> "" then 
      focus:screen-value in frame {&frame-name} = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_procat D-Dialog
ON LEAVE OF fi_procat IN FRAME D-Dialog /* Category */
DO:
  DEF VAR op-error AS LOG NO-UNDO.
  IF LASTKEY NE -1 THEN DO:
     RUN valid-procat(OUTPUT op-error).
     IF op-error THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_stock-no D-Dialog
ON HELP OF fi_stock-no IN FRAME D-Dialog /* Set FG Item# */
DO:
  RUN est/l-ebstk.w (cocode, locode, est.est-type, ?, 5, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_stock-no D-Dialog
ON LEAVE OF fi_stock-no IN FRAME D-Dialog /* Set FG Item# */
DO:
  DEF VAR op-error AS LOG NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
     RUN valid-stock-no(OUTPUT op-error).
     IF op-error THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_wid D-Dialog
ON LEAVE OF fi_wid IN FRAME D-Dialog /* Width */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   v-dec = decimal(fi_wid:screen-value) - trunc(decimal(fi_wid:screen-value),0).
   IF v-dec >= v-16-or-32 THEN
   do:
      message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
       view-as alert-box error.
      RETURN NO-APPLY.
   END.

   IF v-cecscrn-dec THEN
   DO:
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-board D-Dialog
ON HELP OF scr-board IN FRAME D-Dialog /* Board */
DO:
   DEF VAR lv-rowid AS ROWID NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      run windows/l-board1.w (INPUT est.company,
                              INPUT '2',
                              INPUT scr-board:screen-value,
                              output lv-rowid).

      FIND FIRST ITEM WHERE
           ROWID(item) EQ lv-rowid
           NO-LOCK NO-ERROR.
      IF AVAIL ITEM AND ITEM.i-no NE scr-board:SCREEN-VALUE THEN
         scr-board:SCREEN-VALUE = item.i-no.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-board D-Dialog
ON LEAVE OF scr-board IN FRAME D-Dialog /* Board */
DO:
   DEF VAR op-error AS LOG NO-UNDO.
   IF LASTKEY NE -1 THEN DO:
      RUN valid-board(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-end-cell-l1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-end-cell-l1 D-Dialog
ON LEAVE OF scr-end-cell-l1 IN FRAME D-Dialog
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   if lastkey <> -1 THEN
   DO:
      v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

      IF v-dec >= v-16-or-32 then do:
         message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
         return no-apply.
      end.

      IF v-cecscrn-dec THEN
      DO:
         RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
         IF op-error THEN DO:
            MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.

/*       scr-end-cell-w1:SCREEN-VALUE = scr-end-cell-l1:SCREEN-VALUE. */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-end-cell-l2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-end-cell-l2 D-Dialog
ON LEAVE OF scr-end-cell-l2 IN FRAME D-Dialog
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   if lastkey <> -1 THEN
   DO:
      v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

      IF v-dec >= v-16-or-32 then do:
         message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
         return no-apply.
      end.

      IF v-cecscrn-dec THEN
      DO:
         RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
         IF op-error THEN DO:
            MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.

/*       scr-end-cell-w2:SCREEN-VALUE = scr-end-cell-l2:SCREEN-VALUE. */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-end-cell-w1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-end-cell-w1 D-Dialog
ON LEAVE OF scr-end-cell-w1 IN FRAME D-Dialog
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   if lastkey <> -1 THEN
   DO:
      v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

      IF v-dec >= v-16-or-32 then do:
         message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
         return no-apply.
      end.

      IF v-cecscrn-dec THEN
      DO:
         RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
         IF op-error THEN DO:
            MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-end-cell-w2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-end-cell-w2 D-Dialog
ON LEAVE OF scr-end-cell-w2 IN FRAME D-Dialog
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   if lastkey <> -1 THEN
   DO:
      v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

      IF v-dec >= v-16-or-32 then do:
         message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
         return no-apply.
      end.

      IF v-cecscrn-dec THEN
      DO:
         RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
         IF op-error THEN DO:
            MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-in-cell-length
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-in-cell-length D-Dialog
ON LEAVE OF scr-in-cell-length IN FRAME D-Dialog
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   if lastkey <> -1 THEN
   DO:
      v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

      IF v-dec >= v-16-or-32 then do:
         message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
         return no-apply.
      end.

      IF v-cecscrn-dec THEN
      DO:
         RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
         IF op-error THEN DO:
            MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.

/*       scr-in-cell-width:SCREEN-VALUE = scr-in-cell-length:SCREEN-VALUE. */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-in-cell-width
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-in-cell-width D-Dialog
ON LEAVE OF scr-in-cell-width IN FRAME D-Dialog
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   if lastkey <> -1 THEN
   DO:
      v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

      IF v-dec >= v-16-or-32 then do:
         message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
         return no-apply.
      end.

      IF v-cecscrn-dec THEN
      DO:
         RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
         IF op-error THEN DO:
            MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-style-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-style-1 D-Dialog
ON HELP OF scr-style-1 IN FRAME D-Dialog
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      run windows/l-stylec.w (est.company,scr-style-1:SCREEN-VALUE, output char-val).
      if char-val <> "" and scr-style-1:SCREEN-VALUE <> entry(1,char-val) THEN
      DO:
         ASSIGN
            scr-style-1:SCREEN-VALUE = entry(1,char-val).
            scr-style-1.

         FIND FIRST style WHERE
              style.company = est.company and
              style.style = scr-style-1:SCREEN-VALUE
              NO-LOCK NO-ERROR.

         IF AVAIL style AND scr-board:SCREEN-VALUE EQ "" THEN
            scr-board:SCREEN-VALUE = style.material[1].
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-style-1 D-Dialog
ON LEAVE OF scr-style-1 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.
   DEF VAR v-out-cell-1 AS DEC DECIMALS 4 NO-UNDO.
   DEF VAR v-len AS DEC DECIMALS 4 NO-UNDO.

   IF LASTKEY NE -1 THEN DO:
      RUN valid-style(input scr-style-1:SCREEN-VALUE,input 1,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.

   IF scr-style-1:SCREEN-VALUE NE scr-style-1 THEN
   DO:
      ASSIGN scr-style-1.

      FIND FIRST style WHERE
           style.company = est.company and
           style.style = scr-style-1:SCREEN-VALUE
           NO-LOCK NO-ERROR.

      IF AVAIL style AND scr-board:SCREEN-VALUE EQ "" THEN
         scr-board:SCREEN-VALUE = style.material[1].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-style-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-style-2 D-Dialog
ON HELP OF scr-style-2 IN FRAME D-Dialog
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      run windows/l-stylec.w (est.company,scr-style-2:SCREEN-VALUE, output char-val).
      if char-val <> "" and scr-style-2:SCREEN-VALUE <> entry(1,char-val) THEN
         scr-style-2:SCREEN-VALUE = entry(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-style-2 D-Dialog
ON LEAVE OF scr-style-2 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.
   IF LASTKEY NE -1 THEN DO:
      RUN valid-style(input scr-style-2:SCREEN-VALUE,input 2,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rd_alloc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_alloc D-Dialog
ON VALUE-CHANGED OF rd_alloc IN FRAME D-Dialog                      /*Task# 01291403*/
DO:

    ASSIGN rd_alloc.
    IF rd_alloc THEN
      ASSIGN
        tb_unitize:SCREEN-VALUE = "NO"
        tb_unitize = NO.
    ELSE
       ASSIGN
        tb_unitize:SCREEN-VALUE = "YES"
        tb_unitize = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.
find est where rowid(est) = ip-rowid no-lock NO-ERROR.


IF AVAIL est THEN DO:
  {src/adm/template/dialogmn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fi_stock-no fi_part-no fi_part-dscr1 fi_part-dscr2 fi_procat fi_len 
          scr-style-1 scr-end-cell-l1 scr-in-cell-length scr-end-cell-l2 fi_wid 
          scr-style-2 scr-end-cell-w1 scr-in-cell-width scr-end-cell-w2 
          fi_height scr-est-qty scr-board cb_rev-corr rd_alloc scr-no-forms 
          tb_unitize 
      WITH FRAME D-Dialog.
  ENABLE RECT-5 fi_stock-no fi_part-no fi_part-dscr1 fi_part-dscr2 fi_procat 
         fi_len scr-style-1 scr-end-cell-l1 scr-in-cell-length scr-end-cell-l2 
         fi_wid scr-style-2 scr-end-cell-w1 scr-in-cell-width scr-end-cell-w2 
         fi_height scr-est-qty scr-board cb_rev-corr rd_alloc scr-no-forms 
         tb_unitize Btn_SAVE 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
     IF est.ord-no EQ 0 AND est.ord-date EQ ? THEN.
     ELSE DISABLE fi_stock-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  EMPTY TEMP-TABLE tt-eb-set-part.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:

     CASE cepartition-int:
         WHEN 0 THEN
            cb_rev-corr:SCREEN-VALUE = "N".
         WHEN 1 THEN
            cb_rev-corr:SCREEN-VALUE = "S".
         WHEN 2 THEN
            cb_rev-corr:SCREEN-VALUE = "B".
     END CASE.
  END.

  IF v-cecscrn-dec THEN
     ASSIGN
        fi_len:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999" 
        fi_wid:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999"
        fi_height:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999"
        scr-end-cell-l1:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-end-cell-l2:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-in-cell-length:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-in-cell-width:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-end-cell-w1:FORMAT IN FRAME {&FRAME-NAME}  = "->>,>>9.999999"
        scr-end-cell-w2:FORMAT IN FRAME {&FRAME-NAME}  = "->>,>>9.999999".

  /* SAB:  The radio set will only accept ? assignment by setting the screen-value first, then assign it. */
  ASSIGN /*rd_alloc   = IF est.est-type GE 5 THEN v-alloc ELSE v-allocf.*/
     rd_alloc:SCREEN-VALUE   = /*IF est.est-type GE 5 THEN*/ string(v-alloc) /*ELSE v-allocf*/ .

  ASSIGN /* DISPLAY*/ rd_alloc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

/*   RUN get-default-cat. */

      ASSIGN fi_procat:SCREEN-VALUE = ip-last-cat.

      ASSIGN fi_procat.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-eb D-Dialog 
PROCEDURE new-eb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR ll-alloc LIKE eb.set-is-assembled NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAIL eb                                         AND
       (eb.stock-no   NE fi_stock-no:SCREEN-VALUE   OR
        eb.part-no    NE fi_part-no:SCREEN-VALUE    OR
        eb.part-dscr1 NE fi_part-dscr1:SCREEN-VALUE)    THEN DO:

      ASSIGN
       fi_stock-no:SCREEN-VALUE   = eb.stock-no
       fi_part-no:SCREEN-VALUE    = eb.part-no
       fi_part-dscr1:SCREEN-VALUE = eb.part-dscr1
       fi_part-dscr2:SCREEN-VALUE = eb.part-dscr2
       fi_procat:SCREEN-VALUE     = eb.procat
       fi_len:SCREEN-VALUE        = STRING(eb.len)
       fi_wid:SCREEN-VALUE        = STRING(eb.wid)
       fi_height:SCREEN-VALUE     = STRING(eb.dep)
       rd_alloc                   = eb.set-is-assembled
       tb_unitize                 = eb.pur-man.

      IF rd_alloc NE ? THEN rd_alloc = NOT rd_alloc.

      DISPLAY rd_alloc.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "eb"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-64-dec D-Dialog 
PROCEDURE valid-64-dec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-dec AS DEC DECIMALS 6 NO-UNDO.
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

   IF NOT CAN-FIND(FIRST tt-64-dec WHERE
      tt-64-dec.DEC EQ ip-dec) THEN
      op-error = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-board D-Dialog 
PROCEDURE valid-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      IF scr-board:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Board must be entered. Try help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO scr-board.
         op-error = YES.
         LEAVE.
      END.

      IF NOT CAN-FIND(FIRST item WHERE
         item.company = est.company and
         ITEM.i-no = scr-board:SCREEN-VALUE AND
         ITEM.industry EQ '2' AND
         LOOKUP(item.mat-type,'B,P,R') > 0) THEN
         DO:
            MESSAGE "Invalid Board. Try help." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO scr-board.
            op-error = YES.
         END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-no-forms D-Dialog 
PROCEDURE valid-no-forms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

 DO WITH FRAME {&FRAME-NAME}:

    IF scr-no-forms LT 1 OR scr-no-forms GT 2 THEN DO:
       MESSAGE "Number of Forms can only be 1 or 2." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO scr-no-forms.
       op-error = YES.
    END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no D-Dialog 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF fi_part-no:SCREEN-VALUE EQ "" OR
       CAN-FIND(FIRST eb WHERE eb.company   EQ est.company
                           AND eb.est-no    EQ est.est-no
                           AND (eb.part-no  EQ fi_part-no:SCREEN-VALUE OR
                                eb.stock-no EQ fi_part-no:SCREEN-VALUE)
                           AND eb.form-no   NE 0) THEN DO:
      IF fi_part-no:SCREEN-VALUE EQ "" THEN
        MESSAGE TRIM(fi_part-no:LABEL) + " must be entered..." VIEW-AS ALERT-BOX.
      ELSE
        MESSAGE TRIM(fi_part-no:LABEL) + " already exists on estimate..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fi_part-no.
      op-error = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat D-Dialog 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

 DO WITH FRAME {&FRAME-NAME}:
    IF fi_procat:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Category must be entered. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fi_procat.
       op-error = YES.
       LEAVE.
    END.
    IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = est.company and
                                      fgcat.procat = fi_procat:screen-value)
    THEN DO:
        MESSAGE "Invalid Category. Try help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fi_procat.
        op-error = YES.
    END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sizes D-Dialog 
PROCEDURE valid-sizes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

   DEF VAR v-total AS DEC NO-UNDO.
   DEF VAR v-dim AS DEC NO-UNDO.
   DEF VAR v-cont AS LOG NO-UNDO.

   FIND FIRST style WHERE
        style.company EQ cocode AND 
        style.style EQ scr-style-1
        NO-LOCK NO-ERROR.

   IF AVAIL style THEN
   DO:
      ASSIGN
         v-total = scr-end-cell-l1 + scr-end-cell-l2
                 + (scr-in-cell-length * (style.dim-df - 1))
         v-dim = fi_len.

      {sys/inc/k16bb.i v-total}.
      {sys/inc/k16bb.i v-dim}.

      IF v-total NE v-dim THEN
      DO:
         MESSAGE "Length Cell Dimensions Do Not Add Up to Length.  Continue?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE v-cont.

         IF v-cont = NO THEN
         DO:
            op-error = YES.
            LEAVE.
         END.
      END.
   END.

   v-total = 0.

   FIND FIRST style WHERE
        style.company EQ cocode AND 
        style.style EQ scr-style-2
        NO-LOCK NO-ERROR.

   IF AVAIL style THEN
   DO:
      ASSIGN
         v-total = scr-end-cell-w1 + scr-end-cell-w2
                 + (scr-in-cell-width * (style.dim-df - 1))
         v-dim = fi_wid.

      {sys/inc/k16bb.i v-total}.
      {sys/inc/k16bb.i v-dim}.

      IF v-total NE v-dim THEN
      DO:
         MESSAGE "Width Cell Dimensions Do Not Add Up to Width.  Continue?"
             VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE v-cont.

         IF v-cont = NO THEN
            op-error = YES.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-stock-no D-Dialog 
PROCEDURE valid-stock-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

  DEF VAR ll-ans AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF fi_stock-no:SCREEN-VALUE NE "" THEN DO:
      IF CAN-FIND(FIRST eb WHERE eb.company  EQ est.company
                             AND eb.est-no   EQ est.est-no
                             AND eb.stock-no EQ fi_stock-no:SCREEN-VALUE 
                             AND eb.form-no  NE 0) THEN DO:
        MESSAGE TRIM(fi_stock-no:LABEL) + " already exists on estimate..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO fi_stock-no.
        op-error = YES.
      END.

      ELSE
      IF NOT ll-crt-itemfg THEN DO:
        FIND FIRST itemfg
            WHERE itemfg.company EQ est.company
              AND itemfg.i-no    EQ fi_stock-no:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF NOT AVAIL itemfg THEN DO:
          MESSAGE "This item does not exist, would you like to add it?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
          ll-crt-itemfg = ll-ans.
          IF NOT ll-crt-itemfg THEN op-error = YES.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style D-Dialog 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-style AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-num AS INT NO-UNDO.
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      IF ip-style EQ "" OR
         NOT CAN-FIND(FIRST style WHERE
             style.company = style.company and
             style.style = ip-style AND
             LOOKUP(style.TYPE,'P,R') > 0) THEN DO:
         IF ip-style EQ "" THEN
            MESSAGE "Style " + STRING(ip-num) + " must be entered."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         ELSE
            MESSAGE "Invalid Style."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.

         IF ip-num EQ 1 THEN
            APPLY "entry" TO scr-style-1 IN FRAME {&FRAME-NAME}.
         ELSE
            APPLY "entry" TO scr-style-2 IN FRAME {&FRAME-NAME}.

         op-error = YES.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

