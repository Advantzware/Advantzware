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
def input param ip-est-rowid as rowid no-undo.
DEF INPUT PARAM ip-eb-rowid AS ROWID NO-UNDO.

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
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO. 
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-eb2 FOR eb.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-set FOR eb.
DEF BUFFER bf-eb FOR eb.

DEF VAR v-int AS INT NO-UNDO.

{cec/msfcalc.i}

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
&Scoped-define FIELDS-IN-QUERY-D-Dialog eb.stock-no eb.part-no ~
eb.part-dscr1 eb.part-dscr2 eb.procat eb.len eb.wid eb.dep 
&Scoped-define QUERY-STRING-D-Dialog FOR EACH eb SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH eb SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog eb
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog eb


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 btn_update Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS eb.stock-no eb.part-no eb.part-dscr1 ~
eb.part-dscr2 eb.procat eb.len eb.wid eb.dep 
&Scoped-define DISPLAYED-TABLES eb
&Scoped-define FIRST-DISPLAYED-TABLE eb
&Scoped-Define DISPLAYED-OBJECTS scr-style-1 scr-end-cell-l1 ~
scr-in-cell-length scr-end-cell-l2 scr-style-2 scr-end-cell-w1 ~
scr-in-cell-width scr-end-cell-w2 rd_alloc tb_unitize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 eb.stock-no eb.part-no eb.part-dscr1 eb.part-dscr2 ~
eb.procat eb.len eb.wid eb.dep rd_alloc tb_unitize 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-GO 
     LABEL "&Close" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn_update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

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

DEFINE VARIABLE scr-in-cell-length AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-in-cell-width AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE scr-style-1 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE scr-style-2 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE rd_alloc AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Assembled", no,
"Unassembled", yes,
"Assembled w/Part Receipts", ?
     SIZE 41 BY 2.62 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97.6 BY 12.62.

DEFINE VARIABLE tb_unitize AS LOGICAL INITIAL no 
     LABEL "Unitize (Ship all parts on same pallet)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     eb.stock-no AT ROW 1.29 COL 20.2 COLON-ALIGNED WIDGET-ID 52
          LABEL "Set FG Item#"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     eb.part-no AT ROW 2.29 COL 20.2 COLON-ALIGNED WIDGET-ID 48
          LABEL "Set Cust Part#"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     eb.part-dscr1 AT ROW 3.29 COL 20.2 COLON-ALIGNED WIDGET-ID 44
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.part-dscr2 AT ROW 4.29 COL 20.2 COLON-ALIGNED WIDGET-ID 46
          LABEL "Part Description"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.procat AT ROW 5.33 COL 20.2 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.len AT ROW 6.48 COL 20.2 COLON-ALIGNED WIDGET-ID 42
          LABEL "F.G. Length" FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.8 BY 1
     scr-style-1 AT ROW 6.48 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     scr-end-cell-l1 AT ROW 6.48 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     scr-in-cell-length AT ROW 6.48 COL 61.2 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     scr-end-cell-l2 AT ROW 6.48 COL 78.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     eb.wid AT ROW 7.48 COL 20.2 COLON-ALIGNED WIDGET-ID 54 FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.8 BY 1
     scr-style-2 AT ROW 7.48 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     scr-end-cell-w1 AT ROW 7.48 COL 44.2 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     scr-in-cell-width AT ROW 7.48 COL 61.2 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     scr-end-cell-w2 AT ROW 7.48 COL 78.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     eb.dep AT ROW 8.48 COL 20.2 COLON-ALIGNED WIDGET-ID 40
          LABEL "Height" FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.8 BY 1
     rd_alloc AT ROW 9.76 COL 31 NO-LABEL WIDGET-ID 56
     tb_unitize AT ROW 12.38 COL 31 WIDGET-ID 60
     btn_update AT ROW 13.86 COL 26.6 WIDGET-ID 38
     Btn_Cancel AT ROW 13.86 COL 60.6 WIDGET-ID 36
     "Slot" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 5.67 COL 36.8 WIDGET-ID 6
     "End Cell" VIEW-AS TEXT
          SIZE 10.2 BY .62 AT ROW 5.67 COL 46.6 WIDGET-ID 8
     "End Cell" VIEW-AS TEXT
          SIZE 11.2 BY .62 AT ROW 5.67 COL 80.8 WIDGET-ID 34
     "Interior Cell" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5.67 COL 63.6 WIDGET-ID 10
     RECT-5 AT ROW 1 COL 1
     SPACE(0.39) SKIP(2.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Assembled Partition".


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

/* SETTINGS FOR FILL-IN eb.dep IN FRAME D-Dialog
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.len IN FRAME D-Dialog
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME D-Dialog
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.part-dscr2 IN FRAME D-Dialog
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.part-no IN FRAME D-Dialog
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.procat IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET rd_alloc IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN scr-end-cell-l1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-end-cell-l2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-end-cell-w1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-end-cell-w2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-in-cell-length IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-in-cell-width IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-style-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-style-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME D-Dialog
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX tb_unitize IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN eb.wid IN FRAME D-Dialog
   NO-ENABLE 1 EXP-FORMAT                                               */
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


&Scoped-define SELF-NAME btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_update D-Dialog
ON CHOOSE OF btn_update IN FRAME D-Dialog /* Update */
DO:
    DEF VAR op-error AS LOG NO-UNDO.

    IF SELF:LABEL = "&Update" THEN DO:
       RUN check-use(OUTPUT op-error).
       IF op-error THEN RETURN.

       SELF:LABEL = "&Save".
       btn_cancel:LABEL = "&Cancel".

       FIND FIRST bf-est WHERE bf-est.company = eb.company
                            AND bf-est.est-no = eb.est-no NO-LOCK NO-ERROR.

       DO WITH FRAME {&FRAME-NAME}:
         ENABLE {&list-1}.
         IF bf-est.ord-no EQ 0 AND bf-est.ord-date EQ ? THEN
           APPLY "entry" TO eb.stock-no.
         ELSE DO:
           DISABLE eb.stock-no.
           APPLY "entry" TO eb.part-no.
         END.
       END.
    END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
         RUN valid-part-no(OUTPUT op-error).
         IF op-error THEN RETURN NO-APPLY. 
         RUN valid-procat(OUTPUT op-error).
         IF op-error THEN RETURN NO-APPLY. 
         FIND CURRENT eb EXCLUSIVE-LOCK.
         ASSIGN {&DISPLAYED-FIELDS}.

         /*IF eb.est-type GE 5 THEN DO:*/
            {sys/inc/k16bb.i eb.len  } 
            {sys/inc/k16bb.i eb.wid  } 
            {sys/inc/k16bb.i eb.dep  } 
         /*END.*/

         ASSIGN
            rd_alloc
            tb_unitize
            eb.set-is-assembled = rd_alloc
            eb.pur-man          = tb_unitize.

         RUN UpdateSetUnitize.

         IF eb.set-is-assembled NE ? THEN
            eb.set-is-assembled = NOT eb.set-is-assembled.

         IF eb.stock-no NE "" THEN
            FIND FIRST itemfg WHERE
                 itemfg.company EQ eb.company AND
                 itemfg.i-no    EQ eb.stock-no
                 EXCLUSIVE-LOCK  NO-ERROR.

         IF AVAIL itemfg AND eb.stock-no NE "" THEN
         DO: 
            ASSIGN
               itemfg.alloc = rd_alloc
               itemfg.procat = eb.procat.

           FIND CURRENT itemfg NO-LOCK NO-ERROR.
         END.
         FIND CURRENT eb NO-LOCK.

         APPLY "go" TO FRAME {&frame-name}.
    END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dep D-Dialog
ON LEAVE OF eb.dep IN FRAME D-Dialog /* Height */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   v-dec = decimal(eb.dep:screen-value) - trunc(decimal(eb.dep:screen-value),0).

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


&Scoped-define SELF-NAME eb.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len D-Dialog
ON LEAVE OF eb.len IN FRAME D-Dialog /* F.G. Length */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   v-dec = decimal(eb.len:screen-value) - trunc(decimal(eb.len:screen-value),0).
   IF v-dec >= v-16-or-32 THEN
   do:
      message "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
         view-as alert-box error.
      RETURN NO-APPLY.
      /*
      IF v-cecscrn-dec THEN
      DO:
         RUN valid-64-dec(INPUT v-dec, OUTPUT op-error).
         IF op-error THEN DO:
            MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.
      */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-dscr1 D-Dialog
ON HELP OF eb.part-dscr1 IN FRAME D-Dialog /* Item Name */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 4, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no D-Dialog
ON HELP OF eb.part-no IN FRAME D-Dialog /* Set Cust Part# */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 3, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no D-Dialog
ON LEAVE OF eb.part-no IN FRAME D-Dialog /* Set Cust Part# */
DO:
  DEF VAR op-error AS LOG NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-part-no(OUTPUT op-error).
    IF op-error THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat D-Dialog
ON HELP OF eb.procat IN FRAME D-Dialog /* Category */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   run windows/l-fgcat.w (eb.company,focus:screen-value, output char-val).
   if char-val <> "" then 
      assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
               /*        itemfg.procat-desc:screen-value = entry(2,char-val) */
                       .
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat D-Dialog
ON LEAVE OF eb.procat IN FRAME D-Dialog /* Category */
DO:
  DEF VAR op-error AS LOG NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat(OUTPUT op-error).
    IF op-error THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_alloc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_alloc D-Dialog
ON VALUE-CHANGED OF rd_alloc IN FRAME D-Dialog
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


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no D-Dialog
ON HELP OF eb.stock-no IN FRAME D-Dialog /* Set FG Item# */
DO:
  RUN est/l-ebstk.w (cocode, locode, est.est-type, ?, 5, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid D-Dialog
ON LEAVE OF eb.wid IN FRAME D-Dialog /* Width */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.

   v-dec = decimal(eb.wid:screen-value) - trunc(decimal(eb.wid:screen-value),0).
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.
find est where rowid(est) = ip-est-rowid no-lock NO-ERROR.
FIND eb WHERE ROWID(eb) = ip-eb-rowid NO-LOCK NO-ERROR.

FIND FIRST bf-set WHERE
     bf-set.company = eb.company AND
     bf-set.est-no = eb.est-no AND
     bf-set.form-no = 0
     NO-LOCK NO-ERROR.

FIND FIRST b-eb1 WHERE 
     b-eb1.company EQ eb.company AND
     b-eb1.est-no  EQ eb.est-no AND
     b-eb1.form-no NE 0 AND
     b-eb1.blank-no NE 0
     USE-INDEX est-qty
     NO-LOCK NO-ERROR.

IF AVAIL b-eb1 THEN
   FIND FIRST b-eb2 WHERE 
        b-eb2.company EQ eb.company AND
        b-eb2.est-no  EQ eb.est-no AND
        b-eb2.form-no NE 0 AND
        b-eb2.blank-no NE 0 AND
        ROWID(b-eb2) NE ROWID(b-eb1)
        USE-INDEX est-qty
        NO-LOCK NO-ERROR.

IF AVAIL b-eb1 THEN
DO:
   FIND FIRST style WHERE
        style.company EQ eb.company AND
        style.style = b-eb1.style
        NO-LOCK.

   ASSIGN
      v-int = style.dim-df + 1
      scr-style-1 = b-eb1.style
      scr-end-cell-l1 = {sys/inc/k16.i b-eb1.k-len-array2[1]}
      scr-end-cell-l2 = {sys/inc/k16.i b-eb1.k-len-array2[v-int]}
      scr-in-cell-length = {sys/inc/k16.i b-eb1.k-len-array2[2]}.
END.

IF AVAIL b-eb2 THEN
DO:
   FIND FIRST style WHERE
        style.company EQ eb.company AND
        style.style = b-eb2.style
        NO-LOCK.

   ASSIGN
      v-int = style.dim-df + 1
      scr-style-2 = b-eb2.style
      scr-end-cell-w1 = {sys/inc/k16.i b-eb2.k-len-array2[1]}
      scr-end-cell-w2 = {sys/inc/k16.i b-eb2.k-len-array2[v-int]}
      scr-in-cell-width = {sys/inc/k16.i b-eb2.k-len-array2[2]}.
END.

ASSIGN
   eb.len:SCREEN-VALUE = STRING({sys/inc/k16.i eb.len})
   eb.wid:SCREEN-VALUE = STRING({sys/inc/k16.i eb.wid})
   eb.dep:SCREEN-VALUE = STRING({sys/inc/k16.i eb.dep})
   rd_alloc   = bf-set.set-is-assembled
   tb_unitize = bf-set.pur-man.

   IF rd_alloc NE ? THEN
      rd_alloc = NOT rd_alloc.

RELEASE itemfg.

IF bf-set.stock-no NE "" THEN
   FIND FIRST itemfg WHERE
        itemfg.company EQ bf-set.company AND
        itemfg.i-no    EQ bf-set.stock-no
        NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN
   rd_alloc = itemfg.alloc.

{src/adm/template/dialogmn.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-use D-Dialog 
PROCEDURE check-use :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

   ERROR-STATUS:ERROR = NO.
  {est/checkuse.i "no-cancel"}
  IF ERROR-STATUS:ERROR THEN
     op-error = YES.
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
  DISPLAY scr-style-1 scr-end-cell-l1 scr-in-cell-length scr-end-cell-l2 
          scr-style-2 scr-end-cell-w1 scr-in-cell-width scr-end-cell-w2 rd_alloc 
          tb_unitize 
      WITH FRAME D-Dialog.
  IF AVAILABLE eb THEN 
    DISPLAY eb.stock-no eb.part-no eb.part-dscr1 eb.part-dscr2 eb.procat eb.len 
          eb.wid eb.dep 
      WITH FRAME D-Dialog.
  ENABLE RECT-5 btn_update Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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
  
  IF v-cecscrn-dec THEN
     ASSIGN
        eb.len:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999" 
        eb.wid:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999"
        eb.dep:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999"
        scr-end-cell-l1:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-end-cell-l2:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-in-cell-length:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-in-cell-width:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>9.999999"
        scr-end-cell-w1:FORMAT IN FRAME {&FRAME-NAME}  = "->>,>>9.999999"
        scr-end-cell-w2:FORMAT IN FRAME {&FRAME-NAME}  = "->>,>>9.999999".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateSetUnitize D-Dialog 
PROCEDURE UpdateSetUnitize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF tb_unitize THEN DO:
     FIND FIRST bf-eb WHERE bf-eb.company EQ eb.company
                  AND bf-eb.est-no  EQ eb.est-no
                  AND bf-eb.form-no NE 0
                  AND bf-eb.blank-no NE 0 NO-LOCK NO-ERROR.
     IF AVAIL bf-eb AND eb.cas-no EQ "" OR eb.cas-cnt = 0 THEN
     /* task 07211403 - only override if information is not already entered*/
     ASSIGN eb.cas-no = bf-eb.cas-no
            eb.cas-cost = bf-eb.cas-cost
            eb.cas-cnt = bf-eb.cas-cnt
            eb.cas-pal = bf-eb.cas-pal
            eb.cas-len = bf-eb.cas-len
            eb.cas-wid = bf-eb.cas-wid
            eb.cas-dep = bf-eb.cas-dep
            eb.cas-wt = bf-eb.cas-wt
            eb.tr-no = bf-eb.tr-no
            eb.tr-cost = bf-eb.tr-cost
            eb.tr-cnt = bf-eb.tr-cnt
            eb.tr-cas = bf-eb.tr-cas
            eb.tr-len = bf-eb.tr-len
            eb.tr-wid = bf-eb.tr-wid
            eb.tr-dep = bf-eb.tr-dep
            eb.stacks = bf-eb.stacks
            eb.stack-code = bf-eb.stack-code
            eb.weight-m = bf-eb.weight-m
            eb.carrier = bf-eb.carrier
            eb.carr-dscr = bf-eb.carr-dscr
            eb.dest-code = bf-eb.dest-code
            eb.fr-out-c = bf-eb.fr-out-c
            eb.fr-out-m = bf-eb.fr-out-m.


  END.
  ELSE DO:
     ASSIGN eb.cas-no = ""
            eb.cas-cost = 0
            eb.cas-cnt  = 0
            eb.cas-pal  = 0
            eb.cas-len = 0
            eb.cas-wid = 0
            eb.cas-dep = 0
            eb.cas-wt  = 0
            eb.tr-no   = ""
            eb.tr-cost = 0
            eb.tr-cnt  = 0
            eb.tr-cas  = 0
            eb.tr-len  = 0
            eb.tr-wid  = 0
            eb.tr-dep  = 0
            eb.stacks  = 0
            eb.stack-code = ""
            eb.weight-m   = 0
            eb.carrier    = ""
            eb.carr-dscr  = ""
            eb.dest-code  = ""
            eb.fr-out-c  = 0
            eb.fr-out-m  = 0.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no D-Dialog 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
  
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-eb FOR eb.
  
  FOR EACH b-eb
      WHERE b-eb.company EQ eb.company
        AND b-eb.est-no  EQ eb.est-no
        AND b-eb.form-no NE 0
      NO-LOCK:
    li = li + 1.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    IF li GT 1 THEN
      IF eb.part-no:SCREEN-VALUE EQ "" OR
         CAN-FIND(FIRST b-eb WHERE b-eb.company   EQ est.company
                               AND b-eb.est-no    EQ est.est-no
                               AND (b-eb.part-no  EQ eb.part-no:SCREEN-VALUE OR
                                    b-eb.stock-no EQ eb.part-no:SCREEN-VALUE)
                               AND b-eb.form-no   NE 0) THEN DO:
        IF eb.part-no:SCREEN-VALUE EQ "" THEN
          MESSAGE TRIM(eb.part-no:LABEL) + " must be entered..." VIEW-AS ALERT-BOX.
        ELSE
          MESSAGE TRIM(eb.part-no:LABEL) + " already exists on estimate..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO eb.part-no.
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
 DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.
     
 DO WITH FRAME {&FRAME-NAME}:
    IF eb.procat:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Category must be entered. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO eb.procat.
       op-error = YES.
       LEAVE.
    END.
    IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = eb.company and
                                      fgcat.procat = eb.procat:screen-value)
    THEN DO:
        MESSAGE "Invalid Category. Try help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.procat.
        op-error = YES.
    END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

