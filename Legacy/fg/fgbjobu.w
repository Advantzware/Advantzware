&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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
DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.

/* Local Variable Definitions ---                                       */
{sys/inc/var.i shared}

DEFINE SHARED TEMP-TABLE w-job NO-UNDO
    FIELD job-no-disp   AS CHARACTER
    FIELD job-no        LIKE job-hdr.job-no
    FIELD job-no2       LIKE job-hdr.job-no2
    FIELD po-no         LIKE fg-bin.po-no
    FIELD i-no          LIKE job-hdr.i-no
    FIELD j-no          LIKE job-hdr.j-no
    FIELD loc           LIKE fg-bin.loc
    FIELD loc-bin       LIKE fg-bin.loc-bin
    FIELD tag           LIKE fg-bin.tag
    FIELD cust-no       LIKE fg-bin.cust-no
    FIELD cases         AS INTEGER
    FIELD case-count    LIKE fg-bin.case-count
    FIELD cases-unit    LIKE fg-bin.cases-unit
    FIELD qty           AS INTEGER   FORMAT "->>>,>>9"
    FIELD std-tot-cost  LIKE job-hdr.std-tot-cost
    FIELD std-mat-cost  LIKE job-hdr.std-mat-cost
    FIELD std-lab-cost  LIKE job-hdr.std-lab-cost
    FIELD std-var-cost  LIKE job-hdr.std-var-cost
    FIELD std-fix-cost  LIKE job-hdr.std-fix-cost
    FIELD last-cost     LIKE fg-bin.last-cost
    FIELD sell-uom      LIKE itemfg.sell-uom
    FIELD partial-count LIKE fg-bin.partial-count
    FIELD rel-qty       AS INTEGER   FORMAT "->>>,>>9"
    FIELD bol-qty       AS INTEGER   FORMAT "->>>,>>9"
    FIELD avl-qty       AS INTEGER   FORMAT "->>>,>>9"
    FIELD tot-wt        LIKE fg-bin.tot-wt
    INDEX w-job job-no job-no2 loc loc-bin tag.
  
DEFINE BUFFER tmp-w-job FOR w-job.
DEFINE VARIABLE hPgmReason AS HANDLE NO-UNDO.

{sys/ref/fgoecost.i}

DEFINE VARIABLE hPgmSecurity AS HANDLE  NO-UNDO.
DEFINE VARIABLE lAccess1     AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAccess2     AS LOGICAL NO-UNDO.

RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
RUN epCanAccess IN hPgmSecurity ("browsers/fgijob.w", "Access1", OUTPUT lAccess1).
RUN epCanAccess IN hPgmSecurity ("browsers/fgijob.w", "Access2", OUTPUT lAccess2).
DELETE OBJECT hPgmSecurity.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lReqReasonCode AS LOGICAL NO-UNDO .
RUN sys/ref/nk1look.p (INPUT cocode, "AdjustReason", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lReqReasonCode = LOGICAL(cRtnChar) NO-ERROR.

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
&Scoped-define INTERNAL-TABLES itemfg fg-bin

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH itemfg SHARE-LOCK, ~
      EACH fg-bin OF itemfg SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH itemfg SHARE-LOCK, ~
      EACH fg-bin OF itemfg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog itemfg fg-bin
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog itemfg
&Scoped-define SECOND-TABLE-IN-QUERY-D-Dialog fg-bin


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel ld-v1 ld-v4 btnOK ld-v3 ld-v11 ~
ld-v5 ld-v6 ld-v7 ld-v8 ld-v9 ld-v10 cb_reatype 
&Scoped-Define DISPLAYED-OBJECTS ld-job ld-cust-no ld-po ld-whse ld-bin ~
ld-tag ld-v1 ld-v4 ld-sell-uom ld-v3 ld-v11 ld-v2 ld-v5 ld-v6 ld-tot-wt ~
ld-v7 ld-v8 ld-v9 ld-v10 ld-v12 cb_reatype 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cb_reatype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adjustment Reason:" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-cust-no AS CHARACTER FORMAT "x(8)" 
     LABEL "Cust. #" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-job AS CHARACTER FORMAT "X(9)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-po AS CHARACTER FORMAT "X(9)":U 
     LABEL "PO#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-sell-uom AS CHARACTER FORMAT "x(3)" 
     LABEL "Selling UOM" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-tag AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-tot-wt AS DECIMAL FORMAT ">>,>>9.99" INITIAL 0 
     LABEL "Total Weight" 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v1 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Units" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v10 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Fixed Overhead" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v11 AS DECIMAL FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Partial Count" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v12 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Total Cost" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v2 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "On-Hand Qty in Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v3 AS DECIMAL FORMAT ">>>,>>>":U INITIAL 0 
     LABEL "Units per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v4 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Unit Count" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v5 AS DECIMAL FORMAT ">>>,>>>":U INITIAL 0 
     LABEL "Stacks per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v6 AS CHARACTER FORMAT "X":U INITIAL "0" 
     LABEL "Stacking Code" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v7 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Material" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v8 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Labor" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-v9 AS DECIMAL FORMAT "->>>,>>9.9999":U INITIAL 0 
     LABEL "Variable Overhead" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ld-whse AS CHARACTER FORMAT "X(5)":U 
     LABEL "Whse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 57 BY 25.24.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      itemfg, 
      fg-bin SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btnCancel AT ROW 26.95 COL 50
     ld-job AT ROW 1.48 COL 26 COLON-ALIGNED
     ld-cust-no AT ROW 2.67 COL 26 COLON-ALIGNED HELP
          "Enter Customer Number" WIDGET-ID 16
     ld-po AT ROW 3.86 COL 26 COLON-ALIGNED
     ld-whse AT ROW 5.05 COL 26 COLON-ALIGNED
     ld-bin AT ROW 6.24 COL 26 COLON-ALIGNED
     ld-tag AT ROW 7.43 COL 26 COLON-ALIGNED
     ld-v1 AT ROW 8.62 COL 26 COLON-ALIGNED
     ld-v4 AT ROW 9.81 COL 26 COLON-ALIGNED
     ld-sell-uom AT ROW 11 COL 26 COLON-ALIGNED HELP
          "Enter Selling Unit Of Measure (Ea,M,TON,MSF,MSI,MLI,LBS)" WIDGET-ID 18
     btnOK AT ROW 26.95 COL 41
     ld-v3 AT ROW 12.19 COL 26 COLON-ALIGNED
     ld-v11 AT ROW 13.38 COL 26 COLON-ALIGNED
     ld-v2 AT ROW 14.57 COL 26 COLON-ALIGNED
     ld-v5 AT ROW 15.76 COL 26 COLON-ALIGNED
     ld-v6 AT ROW 16.95 COL 26 COLON-ALIGNED
     ld-tot-wt AT ROW 18.14 COL 26 COLON-ALIGNED HELP
          "Enter Total Weight" WIDGET-ID 20
     ld-v7 AT ROW 19.33 COL 26 COLON-ALIGNED
     ld-v8 AT ROW 20.52 COL 26 COLON-ALIGNED
     ld-v9 AT ROW 21.71 COL 26 COLON-ALIGNED
     ld-v10 AT ROW 22.91 COL 26 COLON-ALIGNED
     ld-v12 AT ROW 24.1 COL 26 COLON-ALIGNED
     cb_reatype AT ROW 25.29 COL 26 COLON-ALIGNED WIDGET-ID 12
     RECT-27 AT ROW 1.24 COL 2
     RECT-28 AT ROW 26.71 COL 40 WIDGET-ID 14
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Finished Good Cost Update"
         CANCEL-BUTTON btnCancel.


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

/* SETTINGS FOR FILL-IN ld-bin IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-cust-no IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-job IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-po IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-sell-uom IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-tag IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-tot-wt IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-v12 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-v2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-whse IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-27 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-28 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "ASI.itemfg,ASI.fg-bin OF ASI.itemfg"
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


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel D-Dialog
ON CHOOSE OF btnCancel IN FRAME D-Dialog /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK D-Dialog
ON CHOOSE OF btnOK IN FRAME D-Dialog /* OK */
DO:
    DEFINE VARIABLE w-job-rec   AS RECID     NO-UNDO.
    DEFINE VARIABLE lv-qty      LIKE fg-bin.qty NO-UNDO.
    DEFINE VARIABLE lv-part     LIKE fg-bin.partial-count NO-UNDO.
    DEFINE VARIABLE ll-changed  AS LOG       NO-UNDO.
    DEFINE VARIABLE cReasonCode AS CHARACTER NO-UNDO .

    DEFINE BUFFER b-fg-bin FOR fg-bin.

    DISABLE TRIGGERS FOR LOAD OF loadtag.
  
    DO WITH FRAME {&frame-name}:
        ASSIGN
            ld-v1 ld-v3 ld-v4 ld-v5 ld-v6 ld-v7 ld-v8 ld-v9 ld-v10 ld-v11
            ld-v2 = (ld-v1 * ld-v4) + ld-v11
            ld-cust-no
            ld-sell-uom
            ld-tot-wt
            .
    END.
    ASSIGN 
        cReasonCode = cb_reatype:SCREEN-VALUE IN FRAME {&frame-name} .

    IF NOT AVAILABLE w-job THEN RETURN.

    IF lReqReasonCode AND (cReasonCode EQ "" OR cReasonCode EQ ?) AND ld-v2 NE w-job.qty THEN DO:
        MESSAGE  "Adjustment Reason is required, please enter..."
          VIEW-AS ALERT-BOX INFORMATION .
      APPLY "entry" TO cb_reatype IN FRAME {&frame-name} .
      RETURN NO-APPLY.
    END.

    
    
    ASSIGN
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
        w-job.std-tot-cost  = ld-v7 + ld-v8 + ld-v9 + ld-v10
        w-job.cust-no       = ld-cust-no
        w-job.sell-uom      = ld-sell-uom
        w-job.tot-wt        = ld-tot-wt
        .
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
    IF AVAILABLE fg-bin THEN DO:
        ASSIGN
            ll-changed = fg-bin.qty  NE ld-v2  OR
                fg-bin.case-count    NE ld-v4  OR
                fg-bin.cases-unit    NE ld-v3  OR
                fg-bin.stack-code    NE ld-v6  OR
                fg-bin.units-pallet  NE ld-v5  OR
                fg-bin.partial-count NE ld-v11 OR
                fg-bin.std-tot-cost  NE w-job.std-tot-cost
            fg-bin.case-count   = ld-v4
            fg-bin.cases-unit   = ld-v3
            fg-bin.unit-count   = ld-v3 * ld-v4
            fg-bin.stack-code   = ld-v6
            fg-bin.units-pallet = ld-v5
            fg-bin.std-mat-cost = w-job.std-mat-cost
            fg-bin.std-lab-cost = w-job.std-lab-cost
            fg-bin.std-fix-cost = w-job.std-fix-cost
            fg-bin.std-var-cost = w-job.std-var-cost
            fg-bin.avg-cost     = w-job.std-tot-cost
            fg-bin.last-cost    = w-job.std-tot-cost
            fg-bin.tot-wt       = w-job.tot-wt
            .
        FOR EACH loadtag
            WHERE loadtag.company      EQ fg-bin.company
              AND loadtag.item-type    EQ NO
              AND loadtag.tag-no       EQ fg-bin.tag
              AND loadtag.i-no         EQ fg-bin.i-no
              AND loadtag.is-case-tag  EQ NO
              AND (loadtag.qty-case    NE fg-bin.case-count OR
                   loadtag.case-bundle NE fg-bin.cases-unit)
            :
            ASSIGN
                loadtag.qty-case    = fg-bin.case-count
                loadtag.case-bundle = fg-bin.cases-unit
                .
        END.

            ASSIGN
                fg-bin.std-tot-cost  = w-job.std-tot-cost
                lv-qty               = w-job.qty - fg-bin.qty
                lv-part              = w-job.partial-count - fg-bin.partial-count
                fg-bin.qty           = w-job.qty
                fg-bin.partial-count = w-job.partial-count
                .
            IF ll-changed THEN DO:
                RUN fg/cre-pchr.p (ROWID(fg-bin), "A", lv-qty, lv-part,cReasonCode).
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
                        loadtag.loc-bin = fg-bin.loc-bin
                        .
                END.
    
                ll-changed = NO.
    
                IF CAN-FIND(FIRST b-fg-bin
                    WHERE b-fg-bin.company EQ fg-bin.company
                      AND b-fg-bin.i-no    EQ fg-bin.i-no
                      AND b-fg-bin.tag     EQ fg-bin.tag
                      AND ROWID(b-fg-bin)  NE ROWID(fg-bin)
                      AND b-fg-bin.qty NE 0) THEN
                MESSAGE "Generate zero qty for identical Tag# in other bins?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE ll-changed. 
                IF ll-changed THEN
                FOR EACH b-fg-bin
                    WHERE b-fg-bin.company EQ fg-bin.company
                      AND b-fg-bin.i-no    EQ fg-bin.i-no
                      AND b-fg-bin.tag     EQ fg-bin.tag
                      AND ROWID(b-fg-bin)  NE ROWID(fg-bin)
                    USE-INDEX tag
                    :
                    ASSIGN
                        b-fg-bin.qty           = 0
                        b-fg-bin.partial-count = 0
                        .
                    RUN fg/cre-pchr.p (ROWID(b-fg-bin), "C", 0, 0,cReasonCode).
                END.
            END.

            RUN fg/d-reqtys.w (ROWID(itemfg), NO).
        END.    
    END.
  
    IF w-job.job-no NE "" THEN DO:
        FOR EACH fg-bin
            WHERE fg-bin.company  EQ cocode
              AND fg-bin.i-no     EQ w-job.i-no
              AND fg-bin.job-no   EQ w-job.job-no
              AND fg-bin.job-no2  EQ w-job.job-no2
            :
            ASSIGN
                fg-bin.std-mat-cost = w-job.std-mat-cost
                fg-bin.std-lab-cost = w-job.std-lab-cost
                fg-bin.std-fix-cost = w-job.std-fix-cost
                fg-bin.std-var-cost = w-job.std-var-cost
                fg-bin.std-tot-cost = w-job.std-tot-cost
                fg-bin.avg-cost     = w-job.std-tot-cost
                fg-bin.last-cost    = w-job.std-tot-cost
                .
        END.

        ASSIGN 
            w-job-rec = RECID(w-job).

        CREATE tmp-w-job.
        ASSIGN
            tmp-w-job.job-no       = w-job.job-no
            tmp-w-job.job-no2      = w-job.job-no2
            tmp-w-job.i-no         = w-job.i-no
            tmp-w-job.j-no         = w-job.j-no
            tmp-w-job.loc          = w-job.loc
            tmp-w-job.loc-bin      = w-job.loc-bin
            tmp-w-job.tag          = w-job.tag
            tmp-w-job.case-count   = w-job.case-count
            tmp-w-job.cases-unit   = w-job.cases-unit
            tmp-w-job.qty          = w-job.qty
            tmp-w-job.std-tot-cost = w-job.std-tot-cost
            tmp-w-job.std-mat-cost = w-job.std-mat-cost
            tmp-w-job.std-lab-cost = w-job.std-lab-cost
            tmp-w-job.std-var-cost = w-job.std-var-cost
            tmp-w-job.std-fix-cost = w-job.std-fix-cost
            tmp-w-job.last-cost    = w-job.last-cost
            tmp-w-job.sell-uom     = w-job.sell-uom
            .

        FOR EACH w-job
            WHERE w-job.i-no     EQ tmp-w-job.i-no
              AND w-job.job-no   EQ tmp-w-job.job-no
              AND w-job.job-no2  EQ tmp-w-job.job-no2
            :
            ASSIGN
                w-job.std-mat-cost = tmp-w-job.std-mat-cost
                w-job.std-lab-cost = tmp-w-job.std-lab-cost
                w-job.std-fix-cost = tmp-w-job.std-fix-cost
                w-job.std-var-cost = tmp-w-job.std-var-cost
                w-job.std-tot-cost = w-job.std-lab-cost
                                   + w-job.std-mat-cost
                                   + w-job.std-fix-cost
                                   + w-job.std-var-cost
                                   .
        END.

        DELETE tmp-w-job.    
        FIND FIRST w-job WHERE RECID(w-job) EQ w-job-rec.
    END.

    FIND FIRST job-hdr WHERE job-hdr.j-no EQ w-job.j-no NO-ERROR.
    IF AVAILABLE job-hdr THEN DO:
        ASSIGN
            job-hdr.std-mat-cost = w-job.std-mat-cost
            job-hdr.std-lab-cost = w-job.std-lab-cost
            job-hdr.std-fix-cost = w-job.std-fix-cost
            job-hdr.std-var-cost = w-job.std-var-cost
            job-hdr.std-tot-cost = job-hdr.std-lab-cost
                                 + job-hdr.std-mat-cost
                                 + job-hdr.std-fix-cost
                                 + job-hdr.std-var-cost
                                 .                          
        RELEASE oe-ordl.
        IF NOT v-full-cost THEN
            FIND FIRST oe-ordl
                WHERE oe-ordl.company EQ cocode
                  AND oe-ordl.job-no  EQ job-hdr.job-no
                  AND oe-ordl.job-no2 EQ job-hdr.job-no2
                  AND oe-ordl.ord-no  EQ job-hdr.ord-no
                  AND oe-ordl.i-no    EQ job-hdr.i-no
                NO-ERROR.
        IF AVAILABLE oe-ordl THEN oe-ordl.cost = job-hdr.std-tot-cost.
    END.
  
    RUN fg/updfgcs1.p (RECID(itemfg), NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-cust-no D-Dialog
ON LEAVE OF ld-cust-no IN FRAME D-Dialog /* Cust. # */
DO:
    IF SELF:SCREEN-VALUE NE "" AND
       SELF:SCREEN-VALUE NE w-job.cust-no THEN DO:
        IF itemfg.cust-no NE SELF:SCREEN-VALUE THEN DO:
            MESSAGE
                "Invalid Customer"
            VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-sell-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-sell-uom D-Dialog
ON LEAVE OF ld-sell-uom IN FRAME D-Dialog /* Selling UOM */
DO:
    IF SELF:SCREEN-VALUE NE "" AND
       SELF:SCREEN-VALUE NE w-job.sell-uom THEN DO:
        IF itemfg.sell-uom NE SELF:SCREEN-VALUE THEN DO:
            MESSAGE
                "Invalid UOM"
            VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-v1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-v1 D-Dialog
ON ENTRY OF ld-v1 IN FRAME D-Dialog /* Units */
DO:
    IF NOT AVAILABLE fg-bin THEN 
    DO WITH FRAME {&frame-name}:
        APPLY "tab" TO {&self-name}.
        APPLY "entry" TO ld-v7.
        DISABLE ld-v1 ld-v3 ld-v4 ld-v5 ld-v6 ld-v11.
        RETURN NO-APPLY.
    END.
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
FIND w-job WHERE RECID(w-job) EQ ip-recid NO-ERROR.

IF AVAILABLE w-job THEN DO:
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ cocode
           AND itemfg.i-no    EQ w-job.i-no.      
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
    IF AVAILABLE fg-bin THEN DO:
        IF fg-bin.case-count NE 0 THEN
            ASSIGN
                ld-v1  = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                ld-v11 = fg-bin.qty - (ld-v1 * fg-bin.case-count)
                .
        ASSIGN
            ld-v3 = fg-bin.cases-unit
            ld-v4 = fg-bin.case-count
            ld-v5 = fg-bin.units-pallet
            ld-v6 = fg-bin.stack-code
            .
    END.

    ASSIGN
        ld-job      = w-job.job-no-disp
        ld-po       = w-job.po-no
        ld-whse     = w-job.loc
        ld-bin      = w-job.loc-bin
        ld-tag      = w-job.tag
        ld-v2       = w-job.qty
        ld-v7       = w-job.std-mat-cost
        ld-v8       = w-job.std-lab-cost
        ld-v9       = w-job.std-var-cost
        ld-v10      = w-job.std-fix-cost  
        ld-v12      = (DEC(ld-v7) + DEC(ld-v8) + DEC(ld-v9) + DEC(ld-v10))
        ld-cust-no  = w-job.cust-no
        ld-sell-uom = w-job.sell-uom
        ld-tot-wt   = w-job.tot-wt
        ld-cust-no:SENSITIVE  = lAccess1
        ld-sell-uom:SENSITIVE = lAccess1
        ld-tot-wt:SENSITIVE   = lAccess1 AND lAccess2
        .
    RUN build-type-list .

    FOR EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.company EQ fg-bin.company
          AND fg-rdtlh.i-no    EQ fg-bin.i-no
          AND fg-rdtlh.loc     EQ fg-bin.loc
          AND fg-rdtlh.loc-bin EQ fg-bin.loc-bin
          AND fg-rdtlh.tag     EQ fg-bin.tag,
        FIRST fg-rcpth NO-LOCK
        WHERE fg-rcpth.r-no      EQ fg-rdtlh.r-no
          AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code
          AND fg-rcpth.i-no      EQ fg-bin.i-no
          AND fg-rcpth.po-no     EQ TRIM(STRING(fg-bin.po-no,">>>>>>>>>>")) 
        BY fg-rcpth.trans-date
        : 
        cb_reatype:SCREEN-VALUE IN FRAME {&frame-name} = fg-rdtlh.reject-code[1] NO-ERROR. 
        LEAVE .
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-type-list D-Dialog 
PROCEDURE build-type-list :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cComboList AS CHARACTER NO-UNDO .
     
    RUN "fg/ReasonCode.p" PERSISTENT SET hPgmReason.
    RUN pBuildReasonCode IN hPgmReason ("ADJ",OUTPUT cComboList).
    DELETE OBJECT hPgmReason.

    DO WITH FRAME {&FRAME-NAME}:
        IF cComboList EQ "" THEN cComboList = ?.
        cb_reatype:LIST-ITEM-PAIRS = cComboList .
    END.

    
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
  DISPLAY ld-job ld-cust-no ld-po ld-whse ld-bin ld-tag ld-v1 ld-v4 ld-sell-uom 
          ld-v3 ld-v11 ld-v2 ld-v5 ld-v6 ld-tot-wt ld-v7 ld-v8 ld-v9 ld-v10 
          ld-v12 cb_reatype 
      WITH FRAME D-Dialog.
  ENABLE btnCancel ld-v1 ld-v4 btnOK ld-v3 ld-v11 ld-v5 ld-v6 ld-v7 ld-v8 ld-v9 
         ld-v10 cb_reatype 
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "itemfg"}
  {src/adm/template/snd-list.i "fg-bin"}

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

