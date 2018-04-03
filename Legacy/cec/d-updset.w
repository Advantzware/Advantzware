&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME d-updset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS d-updset 
/*------------------------------------------------------------------------

  File: cec\d-updset.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-est-type AS INT NO-UNDO.

DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-set FOR eb.
DEF VAR lv-set-recid AS RECID NO-UNDO.
DEF VAR lv-new-set AS LOG NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR ld-sqin AS DEC NO-UNDO.
DEF VAR ld-msf AS DEC NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR ll-alloc AS LOG NO-UNDO.
def var ll-crt-itemfg as log no-undo.

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.
def shared buffer xqty for est-qty.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

{cec/msfcalc.i}

{sys/inc/setprint.i}
ll-alloc = IF ip-est-type LE 4 THEN v-allocf ELSE v-alloc.

def var k_frac as dec init "6.25" no-undo.
{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME d-updset

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb est-qty

/* Definitions for DIALOG-BOX d-updset                                  */
&Scoped-define FIELDS-IN-QUERY-d-updset eb.stock-no eb.part-no ~
eb.part-dscr1 eb.part-dscr2 eb.procat eb.len eb.wid eb.dep 
&Scoped-define QUERY-STRING-d-updset FOR EACH eb SHARE-LOCK, ~
      EACH est-qty WHERE TRUE /* Join to eb incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-d-updset OPEN QUERY d-updset FOR EACH eb SHARE-LOCK, ~
      EACH est-qty WHERE TRUE /* Join to eb incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-d-updset eb est-qty
&Scoped-define FIRST-TABLE-IN-QUERY-d-updset eb
&Scoped-define SECOND-TABLE-IN-QUERY-d-updset est-qty


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 btn_qty-msf btn_update Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS eb.stock-no eb.part-no eb.part-dscr1 ~
eb.part-dscr2 eb.procat eb.len eb.wid eb.dep 
&Scoped-define DISPLAYED-TABLES eb
&Scoped-define FIRST-DISPLAYED-TABLE eb
&Scoped-Define DISPLAYED-OBJECTS rd_alloc tb_unitize 

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

DEFINE BUTTON btn_qty-msf 
     LABEL "" 
     SIZE 58 BY 1.

DEFINE BUTTON btn_update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_msf AS DECIMAL FORMAT "->>,>>9.999":U INITIAL 0 
     LABEL "MSF" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE rd_alloc AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Assembled", no,
"Unassembled", yes,
"Assembled w/Part Receipts", ?
     SIZE 41 BY 2.62 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 14.29.

DEFINE VARIABLE tb_unitize AS LOGICAL INITIAL no 
     LABEL "Unitize (Ship all parts on same pallet)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY d-updset FOR 
      eb, 
      est-qty SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME d-updset
     eb.stock-no AT ROW 1.95 COL 24 COLON-ALIGNED
          LABEL "Set FG Item#"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     eb.part-no AT ROW 2.95 COL 24 COLON-ALIGNED
          LABEL "Set Cust Part#"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     eb.part-dscr1 AT ROW 3.95 COL 24 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.part-dscr2 AT ROW 4.95 COL 24 COLON-ALIGNED
          LABEL "Part Description"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.procat AT ROW 6 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.len AT ROW 7.19 COL 24 COLON-ALIGNED
          LABEL "F.G. Length" FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     eb.wid AT ROW 8.19 COL 24 COLON-ALIGNED FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     eb.dep AT ROW 9.19 COL 24 COLON-ALIGNED
          LABEL "Depth" FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     btn_qty-msf AT ROW 10.29 COL 10
     est-qty.eqty AT ROW 10.29 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     fi_msf AT ROW 10.29 COL 46 COLON-ALIGNED
     rd_alloc AT ROW 11.48 COL 28 NO-LABEL
     tb_unitize AT ROW 14.1 COL 28
     btn_update AT ROW 16 COL 15
     Btn_Cancel AT ROW 16 COL 49
     "Set Allocation :" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 12.19 COL 9
          FGCOLOR 9 
     RECT-5 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "SET Information"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX d-updset
   FRAME-NAME                                                           */
ASSIGN 
       FRAME d-updset:SCROLLABLE       = FALSE
       FRAME d-updset:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN eb.dep IN FRAME d-updset
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN est-qty.eqty IN FRAME d-updset
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       est-qty.eqty:HIDDEN IN FRAME d-updset           = TRUE.

/* SETTINGS FOR FILL-IN fi_msf IN FRAME d-updset
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_msf:HIDDEN IN FRAME d-updset           = TRUE.

/* SETTINGS FOR FILL-IN eb.len IN FRAME d-updset
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME d-updset
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.part-dscr2 IN FRAME d-updset
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.part-no IN FRAME d-updset
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.procat IN FRAME d-updset
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET rd_alloc IN FRAME d-updset
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME d-updset
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX tb_unitize IN FRAME d-updset
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN eb.wid IN FRAME d-updset
   NO-ENABLE 1 EXP-FORMAT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX d-updset
/* Query rebuild information for DIALOG-BOX d-updset
     _TblList          = "ASI.eb,asi.est-qty WHERE ASI.eb ..."
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX d-updset */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME d-updset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-updset d-updset
ON WINDOW-CLOSE OF FRAME d-updset /* SET Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel d-updset
ON CHOOSE OF Btn_Cancel IN FRAME d-updset /* Close */
DO:
  IF SELF:LABEL EQ "&Cancel" THEN RUN validate-set.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_qty-msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_qty-msf d-updset
ON CHOOSE OF btn_qty-msf IN FRAME d-updset
DO:
  IF AVAIL eb THEN RUN est/d-estmsf.w (ROWID(eb)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no d-updset
ON LEAVE OF eb.stock-no IN FRAME d-updset /* FG Item# */
DO:
  DEF VAR ll-copy-fg AS LOG NO-UNDO.
  DEF VAR lActive AS LOG NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
    IF eb.stock-no:SCREEN-VALUE  NE "" THEN DO:
        RUN fg/GetItemfgActInact.p (INPUT g_company,
                                    INPUT SELF:SCREEN-VALUE,
                                    OUTPUT lActive).
        IF NOT lActive THEN DO:
/*                                                                   */
/*         FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" */
/*                        AND reftable.company  EQ g_company         */
/*                        AND reftable.loc      EQ ""                */
/*                        AND reftable.code     EQ SELF:SCREEN-VALUE */
/*                        NO-LOCK NO-ERROR.                          */
/*         IF AVAIL reftable AND reftable.code2 = "I" THEN DO:       */
           MESSAGE eb.stock-no:SCREEN-VALUE + " has InActive Status. Order cannot be placed for the Inactive Item."
                   VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.        
    END.
    
    RUN valid-stock-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_update d-updset
ON CHOOSE OF btn_update IN FRAME d-updset /* Update */
DO:
    IF SELF:LABEL = "&Update" THEN DO:
       RUN check-use NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN.

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
         IF bf-est.est-type LE 4 THEN DISABLE tb_unitize.       
       END.
    END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
         RUN valid-part-no NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
         RUN valid-procat NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
         FIND CURRENT eb EXCLUSIVE-LOCK.
         ASSIGN {&DISPLAYED-FIELDS}.

         IF eb.est-type GE 5 THEN DO:
            {sys/inc/k16bb.i eb.len  } 
            {sys/inc/k16bb.i eb.wid  } 
            {sys/inc/k16bb.i eb.dep  } 
         END.

         RUN validate-set.

         ASSIGN rd_alloc tb_unitize.

         ASSIGN
          eb.set-is-assembled = rd_alloc
          eb.pur-man          = tb_unitize.

         RUN UpdateSetUnitize.

         IF eb.set-is-assembled NE ? THEN
           eb.set-is-assembled = NOT eb.set-is-assembled.

         if ll-crt-itemfg then do:
             /*find xest where recid(xest) = recid(est) no-lock no-error.
             find xeb where recid(xeb) = recid(eb) no-lock no-error.
             find xef where recid(xef) = recid(ef) no-lock no-error.*/
             run fg/ce-addfg.p (xeb.stock-no).
             FIND FIRST xeb NO-LOCK
                 WHERE xeb.company  EQ eb.company
                 AND xeb.est-no   EQ eb.est-no
                 AND xeb.form-no  EQ 0
                 AND xeb.stock-no NE ""
                 AND NOT CAN-FIND(FIRST itemfg
                                  WHERE itemfg.company EQ xeb.company
                                  AND itemfg.i-no    EQ xeb.stock-no)
                 NO-ERROR.
             IF AVAIL xeb THEN RUN fg/ce-addfg.p (xeb.stock-no).
             ll-crt-itemfg = no.
         end.


         FIND FIRST itemfg
             WHERE itemfg.company EQ eb.company
               AND itemfg.i-no    EQ eb.stock-no
             NO-ERROR.
         IF AVAIL itemfg THEN
           ASSIGN
            itemfg.alloc  = rd_alloc
            itemfg.procat = eb.procat.
         
         APPLY "go" TO FRAME {&frame-name}.
    END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-dscr1 d-updset
ON HELP OF eb.part-dscr1 IN FRAME d-updset /* Item Name */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 4, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no d-updset
ON HELP OF eb.part-no IN FRAME d-updset /* Set Cust Part# */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 3, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no d-updset
ON LEAVE OF eb.part-no IN FRAME d-updset /* Set Cust Part# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-part-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF length(eb.part-no:SCREEN-VALUE in frame {&frame-name}) GT 12 THEN
        MESSAGE "Set Cust Part# should be Limited To:" SKIP
        "12 Characters if there are more than 9 components." SKIP
        "13 Characters if there are less than 9 components." VIEW-AS ALERT-BOX WARNING BUTTONS OK.


  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat d-updset
ON HELP OF eb.procat IN FRAME d-updset /* Category */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat d-updset
ON LEAVE OF eb.procat IN FRAME d-updset /* Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_alloc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_alloc d-updset
ON VALUE-CHANGED OF rd_alloc IN FRAME d-updset
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no d-updset
ON HELP OF eb.stock-no IN FRAME d-updset /* Set FG Item# */
DO:
  RUN est/l-ebstk.w (cocode, locode, est.est-type, ?, 5, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK d-updset 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpd.i}
DEF VAR ll AS LOG NO-UNDO.

SESSION:DATA-ENTRY-RETURN = YES.
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

DEF BUFFER b-eb FOR eb.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   FIND bf-eb WHERE RECID(bf-eb) EQ ip-recid NO-LOCK NO-ERROR.
   
   FIND FIRST style WHERE
        style.company EQ bf-eb.company AND
        style.style   EQ bf-eb.style
        NO-LOCK NO-ERROR.

   IF AVAIL style AND style.TYPE = "P" THEN
      eb.dep:LABEL = "Height".

   FIND FIRST bf-set WHERE bf-set.company = bf-eb.company
                       AND bf-set.est-no = bf-eb.est-no
                       AND bf-set.form-no = 0
                       NO-LOCK NO-ERROR.

   IF AVAIL bf-set THEN lv-set-recid = RECID(bf-set).
   ELSE DO:
     ll = bf-eb.est-type EQ ip-est-type OR
          CAN-FIND(FIRST eb
                   WHERE eb.company EQ bf-eb.company
                     AND eb.est-no  EQ bf-eb.est-no
                     AND eb.eqty    EQ bf-eb.eqty
                     AND ROWID(eb)  NE ROWID(bf-eb)).
     IF NOT ll AND ((bf-eb.est-type GE 5 AND bf-eb.quantityPerSet GT 1) OR
                    (bf-eb.est-type LE 4 AND bf-eb.cust-% GT 1)) THEN
       MESSAGE "Change this estimate to a set?"
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE ll.
     IF ll THEN DO:
       RUN check-use NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN.
       RUN create-set.
       FIND bf-set WHERE RECID(bf-set) EQ lv-set-recid NO-ERROR.
     END.
     ELSE RETURN.
   END.

   /* update set info from eb */
   i = 0.
   FOR EACH bf-eb WHERE bf-eb.company EQ bf-set.company
                    AND bf-eb.est-no  EQ bf-set.est-no
                    AND bf-eb.form-no NE 0
                  NO-LOCK BREAK BY bf-eb.est-no:
       i = i + 1.
       IF LAST(bf-eb.est-no) THEN LEAVE.
   END.
   IF i LE 1 AND ((bf-eb.est-type GE 5 AND bf-eb.quantityPerSet EQ 2) OR
                  (bf-eb.est-type LE 4 AND bf-eb.cust-% EQ 2)) THEN DO:
     RUN check-use NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN.
     RUN upd-2box.     
   END.

   FIND eb WHERE RECID(eb) = lv-set-recid NO-LOCK.
  
   FIND FIRST est
       WHERE est.company EQ eb.company
         AND est.est-no  EQ eb.est-no
       NO-LOCK.

   FIND FIRST est-qty
       WHERE est-qty.company EQ eb.company
         AND est-qty.est-no  EQ eb.est-no
       NO-LOCK NO-ERROR.

   ASSIGN
    ld-msf     = 0
    rd_alloc   = bf-set.set-is-assembled
    tb_unitize = bf-set.pur-man.

   IF rd_alloc NE ? THEN rd_alloc = NOT rd_alloc.

   FOR EACH b-eb
       WHERE b-eb.company EQ eb.company
         AND b-eb.est-no  EQ eb.est-no
         AND b-eb.form-no NE 0
         AND ROWID(b-eb)  NE ROWID(eb)
       NO-LOCK
       BREAK BY b-eb.est-no:
     ASSIGN
      ld-yld   = IF b-eb.est-type GE 5 THEN
                   (IF b-eb.quantityPerSet LT 0 THEN -1 / b-eb.quantityPerSet ELSE b-eb.quantityPerSet)
                 ELSE
                   (IF b-eb.cust-%  LT 0 THEN -1 / b-eb.cust-%  ELSE b-eb.cust-%)
      ld-sqin  = est.est-qty[1] * ld-yld * b-eb.t-sqin
      ld-msf   = ld-msf + ((IF v-corr THEN (ld-sqin * .007) ELSE (ld-sqin / 144)) / 1000).

     /*IF FIRST(b-eb.est-no) AND LAST(b-eb.est-no) THEN rd_alloc = YES.*/
   END.

   ASSIGN
      fi_msf = ld-msf
      btn_qty-msf:LABEL = TRIM(est-qty.eqty:LABEL) + ": " +
                          TRIM(STRING(est-qty.eqty,est-qty.eqty:FORMAT)) +
                          FILL(" ",10) +
                          TRIM(fi_msf:LABEL) + ": " +
                          TRIM(STRING(fi_msf,fi_msf:FORMAT)).

   FIND FIRST itemfg
       WHERE itemfg.company EQ eb.company
         AND itemfg.i-no    EQ eb.stock-no
         AND itemfg.i-no    NE ""
       NO-LOCK NO-ERROR.
   IF AVAIL itemfg THEN rd_alloc = itemfg.alloc.

   RUN enable_UI.
   IF lv-new-set THEN RUN enable-all.

   IF eb.est-type GE 5 THEN
   DO:
      IF v-cecscrn-char NE "Decimal" THEN
         ASSIGN eb.len:FORMAT = ">>>9.99"
                eb.wid:FORMAT = ">>>9.99"
                eb.dep:FORMAT = ">>>9.99".
      ELSE
         ASSIGN eb.len:FORMAT = ">>>9.999999"
                eb.wid:FORMAT = ">>>9.999999"
                eb.dep:FORMAT = ">>>9.999999".

      ASSIGN
         eb.len:SCREEN-VALUE = STRING({sys/inc/k16.i eb.len})
         eb.wid:SCREEN-VALUE = STRING({sys/inc/k16.i eb.wid})
         eb.dep:SCREEN-VALUE = STRING({sys/inc/k16.i eb.dep}).
   END.

   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-use d-updset 
PROCEDURE check-use :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ERROR-STATUS:ERROR = NO.
  {est/checkuse.i "no-cancel"}
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-set d-updset 
PROCEDURE create-set :
/*------------------------------------------------------------------------------
  Purpose:     /* ce/set-info.a */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST bf-est
      WHERE bf-est.company EQ bf-eb.company
        AND bf-est.est-no  EQ bf-eb.est-no
      NO-LOCK NO-ERROR.

  {ce/set-info.a ip-est-type "bf-" "bf-"}

  bf-eb.set-is-assembled = ll-alloc.
  IF bf-eb.set-is-assembled NE ? THEN
    bf-eb.set-is-assembled = NOT bf-eb.set-is-assembled.

  ASSIGN
   bf-eb.pur-man          = bf-eb.set-is-assembled NE NO
   lv-new-set             = YES
   lv-set-recid           = RECID(bf-eb).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI d-updset  _DEFAULT-DISABLE
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
  HIDE FRAME d-updset.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-all d-updset 
PROCEDURE enable-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    btn_update:LABEL = "&Save".
    ENABLE {&DISPLAYED-FIELDS}.
    IF bf-eb.est-type LE 4 THEN DISABLE tb_unitize.
    DISABLE fi_msf.
    APPLY "entry" TO eb.stock-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI d-updset  _DEFAULT-ENABLE
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
  DISPLAY rd_alloc tb_unitize 
      WITH FRAME d-updset.
  IF AVAILABLE eb THEN 
    DISPLAY eb.stock-no eb.part-no eb.part-dscr1 eb.part-dscr2 eb.procat eb.len 
          eb.wid eb.dep 
      WITH FRAME d-updset.
  ENABLE RECT-5 btn_qty-msf btn_update Btn_Cancel 
      WITH FRAME d-updset.
  VIEW FRAME d-updset.
  {&OPEN-BROWSERS-IN-QUERY-d-updset}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-eb d-updset 
PROCEDURE new-eb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER new-eb FOR eb.

  DEF VAR ll-alloc LIKE eb.set-is-assembled NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND new-eb WHERE ROWID(new-eb) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAIL new-eb                                         AND
       (new-eb.stock-no   NE eb.stock-no:SCREEN-VALUE   OR
        new-eb.part-no    NE eb.part-no:SCREEN-VALUE    OR
        new-eb.part-dscr1 NE eb.part-dscr1:SCREEN-VALUE)    THEN DO:
      ASSIGN
       eb.stock-no:SCREEN-VALUE   = new-eb.stock-no
       eb.part-no:SCREEN-VALUE    = new-eb.part-no
       eb.part-dscr1:SCREEN-VALUE = new-eb.part-dscr1
       eb.part-dscr2:SCREEN-VALUE = new-eb.part-dscr2
       eb.procat:SCREEN-VALUE     = new-eb.procat
       eb.len:SCREEN-VALUE        = STRING(new-eb.len)
       eb.wid:SCREEN-VALUE        = STRING(new-eb.wid)
       eb.dep:SCREEN-VALUE        = STRING(new-eb.dep)
       rd_alloc                   = new-eb.set-is-assembled
       tb_unitize                 = new-eb.pur-man.

      IF rd_alloc NE ? THEN rd_alloc = NOT rd_alloc.

      DISPLAY rd_alloc.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-2box d-updset 
PROCEDURE upd-2box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


  MESSAGE "Is this a 2 Piece Box?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.

  IF ll THEN DO:
    FIND eb WHERE RECID(eb) = lv-set-recid .
    FIND bf-eb WHERE RECID(bf-eb) EQ ip-recid NO-LOCK NO-ERROR.
    ASSIGN eb.stock-no = bf-eb.stock-no
           eb.part-no = bf-eb.part-no
           eb.part-dscr1 = bf-eb.part-dscr1
           eb.part-dscr2 = bf-eb.part-dscr2
           eb.procat = bf-eb.procat
           eb.len = bf-eb.len
           eb.wid = bf-eb.wid
           eb.dep = bf-eb.dep
           /* eb.pur-man = NO */
           .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateSetUnitize d-updset 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no d-updset 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-eb FOR eb.

      
  li = 0.
  FOR EACH b-eb
      WHERE b-eb.company EQ bf-eb.company
        AND b-eb.est-no  EQ bf-eb.est-no
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
        RETURN ERROR.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat d-updset 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    IF eb.procat:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Category must be entered. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO eb.procat.
       RETURN ERROR.
    END.
    IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = eb.company and
                                      fgcat.procat = eb.procat:screen-value)
    THEN DO:
        MESSAGE "Invalid Category. Try help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.procat.
        RETURN ERROR.
    END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-set d-updset 
PROCEDURE validate-set :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-eb2 FOR eb.

  FIND FIRST bf-est WHERE bf-est.company = eb.company
                            AND bf-est.est-no = eb.est-no NO-ERROR.
  FIND FIRST bf-eb2 WHERE bf-eb2.company = eb.company
                      AND bf-eb2.est-no = eb.est-no
                      AND bf-eb2.form-no = 0
                      NO-ERROR.
  IF AVAIL bf-eb2 THEN DO:
     IF bf-eb2.part-no = "" THEN DELETE bf-eb2.
     ELSE IF AVAIL bf-est THEN bf-est.est-type = ip-est-type.
  END.
  IF NOT AVAIL bf-eb2 THEN bf-est.est-type = ip-est-type - 1.
  
  FOR EACH bf-eb2 WHERE bf-eb2.company = bf-est.company
                    AND bf-eb2.est-no = bf-est.est-no:
      bf-eb2.est-type = bf-est.est-type.
  END.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-stock-no d-updset 
PROCEDURE valid-stock-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    IF eb.stock-no:SCREEN-VALUE  NE "" AND
       NOT ll-crt-itemfg                                       AND
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ cocode
                      AND itemfg.i-no    EQ eb.stock-no:SCREEN-VALUE )
    THEN DO:
      MESSAGE "This item does not exist, would you like to add it?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-ans as log.  
      IF ll-ans then ll-crt-itemfg = YES.
      ELSE DO:
        APPLY "entry" TO eb.stock-no .
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
