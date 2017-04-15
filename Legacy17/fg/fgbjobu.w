&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
def input parameter ip-recid as recid no-undo.

/* Local Variable Definitions ---                                       */
{sys/inc/var.i shared}

def shared temp-table w-job no-undo
  field job-no-disp as char
  field job-no like job-hdr.job-no
  field job-no2 like job-hdr.job-no2
  FIELD po-no LIKE fg-bin.po-no
  field i-no like job-hdr.i-no
  field j-no like job-hdr.j-no
  field loc like fg-bin.loc
  field loc-bin like fg-bin.loc-bin
  field tag like fg-bin.tag
  field cust-no like fg-bin.cust-no
  FIELD cases AS INT
  field case-count like fg-bin.case-count
  field cases-unit like fg-bin.cases-unit
  field qty as int format "->>>,>>9"
  field std-tot-cost like  job-hdr.std-tot-cost
  field std-mat-cost like  job-hdr.std-mat-cost
  field std-lab-cost like  job-hdr.std-lab-cost
  field std-var-cost like  job-hdr.std-var-cost
  field std-fix-cost like  job-hdr.std-fix-cost
  field last-cost like fg-bin.last-cost
  field sell-uom like itemfg.sell-uom
  field partial-count like fg-bin.partial-count
  field rel-qty as int format "->>>,>>9"
  field bol-qty as int format "->>>,>>9"
  field avl-qty as int format "->>>,>>9"
  FIELD tot-wt like fg-bin.tot-wt
  INDEX w-job job-no job-no2 loc loc-bin tag.

def buffer tmp-w-job for w-job.


{sys/ref/fgoecost.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 ld-v1 ld-v4 ld-v3 ld-v11 ld-v5 ld-v6 ~
ld-v7 ld-v8 ld-v9 ld-v10 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS ld-job ld-po ld-whse ld-bin ld-tag ld-v1 ~
ld-v4 ld-v3 ld-v11 ld-v2 ld-v5 ld-v6 ld-v7 ld-v8 ld-v9 ld-v10 ld-v12 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ld-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-job AS CHARACTER FORMAT "X(9)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-po AS CHARACTER FORMAT "X(9)":U 
     LABEL "PO#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-tag AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v1 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Units" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v10 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Fixed Overhead" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v11 AS DECIMAL FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Partial Count" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v12 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Total Cost" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v2 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "On-Hand Qty in Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v3 AS DECIMAL FORMAT ">>>,>>>":U INITIAL 0 
     LABEL "Units per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v4 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Unit Count" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v5 AS DECIMAL FORMAT ">>>,>>>":U INITIAL 0 
     LABEL "Stacks per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v6 AS CHARACTER FORMAT "X":U INITIAL "0" 
     LABEL "Stacking Code" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v7 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Material" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v8 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Labor" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE ld-v9 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Variable Overhead" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE ld-whse AS CHARACTER FORMAT "X(5)":U 
     LABEL "Whse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 18.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     ld-job AT ROW 1.52 COL 38 COLON-ALIGNED
     ld-po AT ROW 2.52 COL 38 COLON-ALIGNED
     ld-whse AT ROW 3.52 COL 38 COLON-ALIGNED
     ld-bin AT ROW 4.52 COL 38 COLON-ALIGNED
     ld-tag AT ROW 5.52 COL 38 COLON-ALIGNED
     ld-v1 AT ROW 6.52 COL 38 COLON-ALIGNED
     ld-v4 AT ROW 7.48 COL 38 COLON-ALIGNED
     ld-v3 AT ROW 8.48 COL 38 COLON-ALIGNED
     ld-v11 AT ROW 9.48 COL 38 COLON-ALIGNED
     ld-v2 AT ROW 10.48 COL 38 COLON-ALIGNED
     ld-v5 AT ROW 11.43 COL 38 COLON-ALIGNED
     ld-v6 AT ROW 12.43 COL 38 COLON-ALIGNED
     ld-v7 AT ROW 14 COL 38 COLON-ALIGNED
     ld-v8 AT ROW 14.95 COL 38 COLON-ALIGNED
     ld-v9 AT ROW 15.91 COL 38 COLON-ALIGNED
     ld-v10 AT ROW 16.86 COL 38 COLON-ALIGNED
     ld-v12 AT ROW 17.86 COL 38 COLON-ALIGNED
     Btn_OK AT ROW 19.52 COL 16
     Btn_Cancel AT ROW 19.52 COL 49
     RECT-27 AT ROW 1 COL 1
     SPACE(0.59) SKIP(1.89)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Finished Good Cost Update"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR FILL-IN ld-bin IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-job IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-po IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-tag IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-v12 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-v2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-whse IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Finished Good Cost Update */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DEF VAR w-job-rec AS RECID NO-UNDO.
  DEF VAR lv-qty LIKE fg-bin.qty NO-UNDO.
  DEF VAR lv-part LIKE fg-bin.partial-count NO-UNDO.
  DEF VAR ll-changed AS LOG NO-UNDO.

  DEF BUFFER b-fg-bin FOR fg-bin.


  DISABLE TRIGGERS FOR LOAD OF loadtag.

  do with frame {&frame-name}:
    assign ld-v1 ld-v3 ld-v4 ld-v5 ld-v6 ld-v7 ld-v8 ld-v9 ld-v10 ld-v11.
    ld-v2 = (ld-v1 * ld-v4) + ld-v11.
  end.

  if not avail w-job then return.

  assign
   w-job.cases         = ld-v1
   w-job.case-count    = ld-v4
   w-job.cases-unit    = ld-v3
   w-job.qty           = ld-v2
   w-job.avl-qty       = ld-v2 - w-job.rel-qty - w-job.bol-qty
   w-job.std-mat-cost  = ld-v7
   w-job.std-lab-cost  = ld-v8
   w-job.std-var-cost  = ld-v9
   w-job.std-fix-cost  = ld-v10
   w-job.partial-count = ld-v11
   w-job.std-tot-cost  = ld-v7 + ld-v8 + ld-v9 + ld-v10.

  FIND FIRST fg-bin
      WHERE fg-bin.company  EQ cocode
        AND fg-bin.i-no     EQ w-job.i-no
        AND fg-bin.loc      EQ w-job.loc
        AND fg-bin.loc-bin  EQ w-job.loc-bin
        AND fg-bin.tag      EQ w-job.tag
        AND fg-bin.job-no   EQ w-job.job-no
        AND fg-bin.job-no2  EQ w-job.job-no2
        AND fg-bin.cust-no  EQ w-job.cust-no
      NO-ERROR.

  IF AVAIL fg-bin THEN DO:
    ll-changed = fg-bin.qty           NE ld-v2  OR
                 fg-bin.case-count    NE ld-v4  OR
                 fg-bin.cases-unit    NE ld-v3  OR
                 fg-bin.stack-code    NE ld-v6  OR
                 fg-bin.units-pallet  NE ld-v5  OR
                 fg-bin.partial-count NE ld-v11 OR
                 fg-bin.std-tot-cost  NE w-job.std-tot-cost.

    ASSIGN
     fg-bin.case-count    = ld-v4
     fg-bin.cases-unit    = ld-v3
     fg-bin.unit-count    = ld-v3 * ld-v4
     fg-bin.stack-code    = ld-v6
     fg-bin.units-pallet  = ld-v5

     fg-bin.std-mat-cost = w-job.std-mat-cost
     fg-bin.std-lab-cost = w-job.std-lab-cost
     fg-bin.std-fix-cost = w-job.std-fix-cost
     fg-bin.std-var-cost = w-job.std-var-cost
     fg-bin.avg-cost     = w-job.std-tot-cost
     fg-bin.last-cost    = w-job.std-tot-cost.

    FOR EACH loadtag
        WHERE loadtag.company      EQ fg-bin.company
          AND loadtag.item-type    EQ NO
          AND loadtag.tag-no       EQ fg-bin.tag
          AND loadtag.i-no         EQ fg-bin.i-no
          AND loadtag.is-case-tag  EQ NO
          AND (loadtag.qty-case    NE fg-bin.case-count OR
               loadtag.case-bundle NE fg-bin.cases-unit):
       ASSIGN
        loadtag.qty-case    = fg-bin.case-count
        loadtag.case-bundle = fg-bin.cases-unit .
    END.

    ASSIGN
     fg-bin.std-tot-cost  = w-job.std-tot-cost
     lv-qty               = w-job.qty - fg-bin.qty
     lv-part              = w-job.partial-count - fg-bin.partial-count
     fg-bin.qty           = w-job.qty
     fg-bin.partial-count = w-job.partial-count.

    IF ll-changed THEN DO:
      RUN fg/cre-pchr.p (ROWID(fg-bin), "A", lv-qty, lv-part).

      IF fg-bin.tag NE "" THEN DO:
        FOR EACH loadtag
            WHERE loadtag.company     EQ fg-bin.company
              AND loadtag.item-type   EQ NO
              AND loadtag.tag-no      EQ fg-bin.tag
              AND loadtag.i-no        EQ fg-bin.i-no
              AND loadtag.is-case-tag EQ NO
            TRANSACTION:
          ASSIGN
           loadtag.loc     = fg-bin.loc
           loadtag.loc-bin = fg-bin.loc-bin.
        END.

        ll-changed = NO.

        IF CAN-FIND(FIRST b-fg-bin
                    WHERE b-fg-bin.company EQ fg-bin.company
                      AND b-fg-bin.i-no    EQ fg-bin.i-no
                      AND b-fg-bin.tag     EQ fg-bin.tag
                      AND ROWID(b-fg-bin)  NE ROWID(fg-bin)
                      AND b-fg-bin.qty NE 0) 
                      THEN
          MESSAGE "Generate zero qty for identical Tag# in other bins?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
              UPDATE ll-changed. 

        IF ll-changed THEN
        FOR EACH b-fg-bin
            WHERE b-fg-bin.company EQ fg-bin.company
              AND b-fg-bin.i-no    EQ fg-bin.i-no
              AND b-fg-bin.tag     EQ fg-bin.tag
              AND ROWID(b-fg-bin)  NE ROWID(fg-bin)
            USE-INDEX tag:
          ASSIGN
           b-fg-bin.qty           = 0
           b-fg-bin.partial-count = 0.

          RUN fg/cre-pchr.p (ROWID(b-fg-bin), "C", 0, 0).
        END.
      END.

      RUN fg/d-reqtys.w (ROWID(itemfg), NO).
    END.    
  END.

  IF w-job.job-no NE "" THEN DO:
    for each fg-bin
        where fg-bin.company  eq cocode
          and fg-bin.i-no     eq w-job.i-no
          and fg-bin.job-no   eq w-job.job-no
          and fg-bin.job-no2  eq w-job.job-no2:
      assign
       fg-bin.std-mat-cost = w-job.std-mat-cost
       fg-bin.std-lab-cost = w-job.std-lab-cost
       fg-bin.std-fix-cost = w-job.std-fix-cost
       fg-bin.std-var-cost = w-job.std-var-cost
       fg-bin.std-tot-cost = w-job.std-tot-cost
       fg-bin.avg-cost     = w-job.std-tot-cost
       fg-bin.last-cost    = w-job.std-tot-cost.
    end.

    assign w-job-rec = recid(w-job).

    create tmp-w-job.
    assign
     tmp-w-job.job-no  = w-job.job-no
     tmp-w-job.job-no2 = w-job.job-no2
     tmp-w-job.i-no    = w-job.i-no
     tmp-w-job.j-no    = w-job.j-no
     tmp-w-job.loc     = w-job.loc
     tmp-w-job.loc-bin = w-job.loc-bin
     tmp-w-job.tag     = w-job.tag
     tmp-w-job.case-count = w-job.case-count
     tmp-w-job.cases-unit = w-job.cases-unit
     tmp-w-job.qty = w-job.qty
     tmp-w-job.std-tot-cost = w-job.std-tot-cost
     tmp-w-job.std-mat-cost = w-job.std-mat-cost
     tmp-w-job.std-lab-cost = w-job.std-lab-cost
     tmp-w-job.std-var-cost = w-job.std-var-cost
     tmp-w-job.std-fix-cost = w-job.std-fix-cost
     tmp-w-job.last-cost = w-job.last-cost
     tmp-w-job.sell-uom = w-job.sell-uom.

    for each w-job
        where w-job.i-no     eq tmp-w-job.i-no
          and w-job.job-no   eq tmp-w-job.job-no
          and w-job.job-no2  eq tmp-w-job.job-no2:
      assign
       w-job.std-mat-cost = tmp-w-job.std-mat-cost
       w-job.std-lab-cost = tmp-w-job.std-lab-cost
       w-job.std-fix-cost = tmp-w-job.std-fix-cost
       w-job.std-var-cost = tmp-w-job.std-var-cost
       w-job.std-tot-cost = w-job.std-lab-cost +
                            w-job.std-mat-cost +
                            w-job.std-fix-cost +
                            w-job.std-var-cost.
    end.

    delete tmp-w-job.

    find first w-job where recid(w-job) eq w-job-rec.
  end.

  find first job-hdr where job-hdr.j-no eq w-job.j-no no-error.
  if avail job-hdr then do:
    assign
     job-hdr.std-mat-cost = w-job.std-mat-cost
     job-hdr.std-lab-cost = w-job.std-lab-cost
     job-hdr.std-fix-cost = w-job.std-fix-cost
     job-hdr.std-var-cost = w-job.std-var-cost
     job-hdr.std-tot-cost = job-hdr.std-lab-cost +
                            job-hdr.std-mat-cost +
                            job-hdr.std-fix-cost +
                            job-hdr.std-var-cost.

    release oe-ordl.
    if not v-full-cost then
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.job-no  eq job-hdr.job-no
          and oe-ordl.job-no2 eq job-hdr.job-no2
          and oe-ordl.ord-no  eq job-hdr.ord-no
          and oe-ordl.i-no    eq job-hdr.i-no
        no-error.
    if avail oe-ordl then oe-ordl.cost = job-hdr.std-tot-cost.
  end.

  RUN fg/updfgcs1.p (RECID(itemfg), NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v1 D-Dialog
ON ENTRY OF ld-v1 IN FRAME D-Dialog /* Units */
DO:
  if not avail fg-bin then do with frame {&frame-name}:
    apply "tab" to {&self-name}.
    apply "entry" to ld-v7.
    disable ld-v1 ld-v3 ld-v4 ld-v5 ld-v6 ld-v11.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v1 D-Dialog
ON VALUE-CHANGED OF ld-v1 IN FRAME D-Dialog /* Units */
DO:
  IF LASTKEY NE -1 THEN RUN calc-tot-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v10 D-Dialog
ON VALUE-CHANGED OF ld-v10 IN FRAME D-Dialog /* Fixed Overhead */
DO:
  IF LASTKEY NE -1 THEN RUN calc-tot-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v11 D-Dialog
ON VALUE-CHANGED OF ld-v11 IN FRAME D-Dialog /* Partial Count */
DO:
  IF LASTKEY NE -1 THEN RUN calc-tot-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v4 D-Dialog
ON VALUE-CHANGED OF ld-v4 IN FRAME D-Dialog /* Unit Count */
DO:
  IF LASTKEY NE -1 THEN RUN calc-tot-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v7 D-Dialog
ON VALUE-CHANGED OF ld-v7 IN FRAME D-Dialog /* Material */
DO:
  IF LASTKEY NE -1 THEN RUN calc-tot-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v8 D-Dialog
ON VALUE-CHANGED OF ld-v8 IN FRAME D-Dialog /* Labor */
DO:
  IF LASTKEY NE -1 THEN RUN calc-tot-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v9 D-Dialog
ON VALUE-CHANGED OF ld-v9 IN FRAME D-Dialog /* Variable Overhead */
DO:
  IF LASTKEY NE -1 THEN RUN calc-tot-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
find w-job where recid(w-job) eq ip-recid no-error.

if avail w-job then do:
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq w-job.i-no
      no-lock.

  find first fg-bin
      where fg-bin.company  eq cocode
        and fg-bin.i-no     eq w-job.i-no
        and fg-bin.loc      eq w-job.loc
        and fg-bin.loc-bin  eq w-job.loc-bin
        and fg-bin.tag      eq w-job.tag
        and fg-bin.job-no   eq w-job.job-no
        and fg-bin.job-no2  eq w-job.job-no2
        AND fg-bin.cust-no  EQ w-job.cust-no
      no-error.
  IF AVAIL fg-bin THEN DO:
    IF fg-bin.case-count NE 0 THEN
      ASSIGN
       ld-v1  = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
       ld-v11 = fg-bin.qty - (ld-v1 * fg-bin.case-count).

    ASSIGN
     ld-v3  = fg-bin.cases-unit
     ld-v4  = fg-bin.case-count
     ld-v5  = fg-bin.units-pallet
     ld-v6  = fg-bin.stack-code.
  END.

  ASSIGN
   ld-job  = w-job.job-no-disp
   ld-po   = w-job.po-no
   ld-whse = w-job.loc
   ld-bin  = w-job.loc-bin
   ld-tag  = substr(w-job.tag, 16, 5)
   ld-v2   = w-job.qty
   ld-v7   = w-job.std-mat-cost
   ld-v8   = w-job.std-lab-cost
   ld-v9   = w-job.std-var-cost
   ld-v10  = w-job.std-fix-cost.  
   ld-v12  = (DEC(ld-v7) + DEC(ld-v8) + DEC(ld-v9) + DEC(ld-v10)).
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tot-cost D-Dialog 
PROCEDURE calc-tot-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ld-v12:SCREEN-VALUE = STRING((DEC(ld-v7:SCREEN-VALUE) +
                                  DEC(ld-v8:SCREEN-VALUE) +
                                  DEC(ld-v9:SCREEN-VALUE) +
                                 DEC(ld-v10:SCREEN-VALUE))).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tot-qty D-Dialog 
PROCEDURE calc-tot-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ld-v2:SCREEN-VALUE = STRING((DEC(ld-v1:SCREEN-VALUE) *
                                  DEC(ld-v4:SCREEN-VALUE)) +
                                 DEC(ld-v11:SCREEN-VALUE)).
  END.
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
  DISPLAY ld-job ld-po ld-whse ld-bin ld-tag ld-v1 ld-v4 ld-v3 ld-v11 ld-v2 
          ld-v5 ld-v6 ld-v7 ld-v8 ld-v9 ld-v10 ld-v12 
      WITH FRAME D-Dialog.
  ENABLE RECT-27 ld-v1 ld-v4 ld-v3 ld-v11 ld-v5 ld-v6 ld-v7 ld-v8 ld-v9 ld-v10 
         Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

