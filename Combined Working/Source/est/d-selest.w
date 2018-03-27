&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*-----------------------------------------------------------------------*/
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
DEF INPUT  PARAM ip-rowid   AS   ROWID          NO-UNDO.
DEF INPUT  PARAM ip-add-eb  AS   LOG            NO-UNDO.
DEF INPUT  PARAM ip-cust    LIKE eb.cust-no     NO-UNDO.
DEF OUTPUT PARAM op-success AS   LOG   INIT YES NO-UNDO.
DEF OUTPUT PARAM op-rowid   AS   ROWID          NO-UNDO.

/* Local Variable Definitions ---                                       */
{sys/inc/var.i SHARED}

DEF VAR ll-master AS LOG NO-UNDO.
DEF VAR ll-spec AS LOG INIT YES NO-UNDO.
DEF VAR ll-dept AS LOG INIT YES NO-UNDO.
DEF VAR ll-form AS LOG NO-UNDO.
DEF VAR lv-est-type AS INT NO-UNDO.
DEF VAR lv-both AS LOG NO-UNDO.

DEF TEMP-TABLE tt-est NO-UNDO
    FIELD selekt AS   LOG LABEL "Selected"
    FIELD eqty   LIKE eb.eqty
    FIELD row-id AS   ROWID
    FIELD est-no LIKE est.est-no 
    FIELD die-no LIKE eb.die-no
    FIELD est-type AS INT.

DEF BUFFER b-tt-est FOR tt-est.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-est eb

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-est.selekt tt-est.eqty eb.est-no eb.die-no eb.part-no eb.stock-no eb.style eb.part-dscr1   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-est.selekt tt-est.eqty   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-est
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-est
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-est, ~
                                   FIRST eb WHERE ROWID(eb) EQ tt-est.row-id NO-LOCK                             BY (IF fi_die-no EQ "" THEN "" ELSE eb.die-no)                             BY eb.est-no BY eb.form-no BY eb.blank-no
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-est, ~
                                   FIRST eb WHERE ROWID(eb) EQ tt-est.row-id NO-LOCK                             BY (IF fi_die-no EQ "" THEN "" ELSE eb.die-no)                             BY eb.est-no BY eb.form-no BY eb.blank-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-est eb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-est
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 eb


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_est-no fi_die-no btn_go BROWSE-2 btn_ok 
&Scoped-Define DISPLAYED-OBJECTS fi_est-no fi_die-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_ok AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_die-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Die#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-est, 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-est.selekt
     tt-est.eqty FORMAT ">,>>>,>>>,>>9"
     eb.est-no   FORMAT "x(8)"
     eb.die-no
     eb.part-no
     eb.stock-no
     eb.style
     eb.part-dscr1
     ENABLE tt-est.selekt tt-est.eqty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 122 BY 13.33
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi_est-no AT ROW 1.24 COL 33 COLON-ALIGNED
     fi_die-no AT ROW 1.24 COL 62 COLON-ALIGNED
     btn_go AT ROW 1.24 COL 89
     BROWSE-2 AT ROW 2.67 COL 2
     btn_ok AT ROW 16.71 COL 53
     SPACE(56.39) SKIP(1.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Selection"
         DEFAULT-BUTTON btn_go.


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
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-2 btn_go D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-est,
                            FIRST eb WHERE ROWID(eb) EQ tt-est.row-id NO-LOCK
                            BY (IF fi_die-no EQ "" THEN "" ELSE eb.die-no)
                            BY eb.est-no BY eb.form-no BY eb.blank-no.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON HELP OF FRAME D-Dialog /* Estimate Selection */
DO:
    def var char-val as cha no-undo.
    def var look-recid as recid no-undo. 
    DEF VAR lw-focus AS HANDLE NO-UNDO.

    lw-focus = FOCUS.
    
    case lw-focus:name :
         when "fi_est-no" then do:
              IF lv-both EQ NO THEN
                 run windows/l-tancst.w (cocode, locode, ip-cust, lv-est-type, lw-focus:SCREEN-VALUE, OUTPUT char-val).
              ELSE
                 run windows/l-tancst2.w (cocode, locode, ip-cust, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            if char-val NE "" THEN DO:
              find first eb where recid(eb) = INT(char-val) no-lock no-error.
              IF AVAIL eb THEN lw-focus:screen-value = eb.est-no.
            END.
         end.
         when "fi_die-no" then do:
            IF lv-both EQ NO THEN
               run windows/l-tndiec.w (cocode, locode, ip-cust, lv-est-type, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            ELSE
               run windows/l-tndiec2.w (cocode, locode, ip-cust, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            if char-val NE "" THEN DO:
              find first eb where recid(eb) = INT(char-val) no-lock no-error.
              IF AVAIL eb THEN lw-focus:screen-value = eb.die-no.
            END.
         end.
    end case.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Estimate Selection */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  op-success = NO.
  APPLY "choose" TO btn_ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go D-Dialog
ON CHOOSE OF btn_go IN FRAME D-Dialog /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN {&displayed-objects}.

    RUN build-table.
    IF NOT CAN-FIND (FIRST tt-est) THEN 
        MESSAGE "Estimate does not qualify as a Master Tandem.  Review that estimate and parts meet the following criteria:" SKIP 
             " * Must be an original estimate (no Estimate # shown in the 'From:' field on the Specs Tab)" SKIP 
             " * All parts must be the same style" SKIP
             " * All parts must be the same dimensions" SKIP 
             " * All parts must be separate forms (no combos)" SKIP 
             " * All forms must have the same dimensions"
             VIEW-AS ALERT-BOX TITLE "Not a Master Tandem Estimate" .
        {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}

    RUN dispatch ("enable").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok D-Dialog
ON CHOOSE OF btn_ok IN FRAME D-Dialog /* Save */
DO:
  DEF BUFFER b-eb   FOR eb.
  DEF BUFFER b-eb1  FOR eb.
  DEF BUFFER b-ef   FOR ef.
  DEF BUFFER b-op   FOR est-op.
  DEF BUFFER bf-eb  FOR eb.
  DEF BUFFER bf-ef  FOR ef.
  DEF BUFFER bf-est FOR est.
  DEF BUFFER b-ref  FOR reftable.

  DEF VAR lv-copied AS ROWID NO-UNDO.
  DEF VAR cecopy-cha AS CHAR INIT "Fold" NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lj AS INT NO-UNDO.
  DEF VAR lv-ef-rowid AS ROWID NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.
  
  RELEASE eb.
  RELEASE ef.
  RELEASE est-qty.
  RELEASE est.

  FIND FIRST tt-est WHERE tt-est.selekt NO-ERROR.

  IF lv-both EQ YES AND AVAIL tt-est THEN
     lv-est-type = IF tt-est.est-type GE 5 THEN 8 ELSE 4.

  IF ip-rowid EQ ? AND AVAIL tt-est THEN
    IF lv-est-type EQ 4 THEN
      RUN est/NewEstimate.p ('F', lv-est-type, OUTPUT ip-rowid).
    ELSE
      RUN est/NewEstimate.p ('C', lv-est-type, OUTPUT ip-rowid).

  op-rowid = ip-rowid.

  FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL eb THEN DO:
    FIND FIRST ef OF eb EXCLUSIVE NO-ERROR.
    lv-ef-rowid = ROWID(ef).
  END.

  IF AVAIL ef THEN
  FIND FIRST est-qty 
      WHERE est-qty.company EQ ef.company
        AND est-qty.est-no  EQ ef.est-no
        AND est-qty.eqty    EQ ef.eqty
      EXCLUSIVE NO-ERROR.

  IF AVAIL est-qty THEN
  FIND FIRST est OF ef EXCLUSIVE NO-ERROR.

  IF AVAIL est AND CAN-FIND(FIRST tt-est WHERE tt-est.selekt) THEN DO:
    op-success = NO.

    FIND b-eb1 WHERE ROWID(b-eb1) EQ ROWID(eb) NO-LOCK.

    FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

    lv-msg = IF ip-add-eb THEN
               "This will copy to a new item on this estimate/order, "
             ELSE
               "This will create estimate with all items selected, ".

    MESSAGE TRIM(lv-msg) SKIP
            "Do you want to continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

    IF ll-ans THEN DO:
      SESSION:SET-WAIT-STATE ("general").

      est.est-type = lv-est-type.

      FOR EACH tt-est WHERE tt-est.selekt,
          FIRST b-eb WHERE ROWID(b-eb) EQ tt-est.row-id NO-LOCK,
          FIRST b-ef OF b-eb NO-LOCK
          BREAK BY b-eb.est-no
                BY b-eb.form-no
                BY b-eb.blank-no:

        lv-copied  = ROWID(b-eb).

        IF FIRST(b-eb.est-no) THEN
          BUFFER-COPY b-ef EXCEPT e-num form-no est-no blank-qty eqty rec_key TO ef.

        ELSE DO:
          /*IF FIRST-OF(b-eb.form-no) THEN DO:*/
            FIND LAST ef 
                WHERE ef.company EQ est-qty.company
                  AND ef.est-no  EQ est-qty.est-no
                  AND ef.eqty    EQ est-qty.eqty
                USE-INDEX est-qty EXCLUSIVE NO-ERROR.
            li = IF AVAIL ef THEN ef.form-no ELSE 0.

            CREATE ef.
            BUFFER-COPY b-ef EXCEPT rec_key TO ef
            ASSIGN
             ef.e-num     = est.e-num
             ef.form-no   = li + 1
             ef.est-no    = est.est-no
             ef.eqty      = est-qty.eqty
             ef.blank-qty = 0
             est.form-qty = est.form-qty + 1
             lv-ef-rowid  = ROWID(ef).
          /*END.*/

          FIND ef WHERE ROWID(ef) EQ lv-ef-rowid EXCLUSIVE.

          FIND LAST eb OF ef USE-INDEX est-qty NO-LOCK NO-ERROR.
          li = IF AVAIL eb THEN eb.blank-no ELSE 0.

          CREATE eb.
          ASSIGN
           eb.est-type  = ef.est-type
           eb.e-num     = est.e-num
           eb.est-no    = est.est-no
           eb.eqty      = ef.eqty
           eb.est-int   = INT(est.est-no)
           eb.form-no   = ef.form-no
           eb.blank-no  = li + 1
           ef.blank-qty = ef.blank-qty + 1.
        END.

        IF FIRST(b-eb.est-no) AND NOT ip-add-eb THEN ef.eqty = tt-est.eqty.

        op-success = YES.

        {est/blankcp2.i}

        FIND CURRENT eb EXCLUSIVE.
        FIND CURRENT ef EXCLUSIVE.
        ASSIGN
         eb.master-est-no = IF eb.est-no EQ b-eb.est-no THEN b-eb.master-est-no ELSE b-eb.est-no
         eb.cust-no       = b-eb.cust-no
         eb.ship-id       = b-eb.ship-id
         eb.bl-qty        = tt-est.eqty
         eb.yld-qty       = tt-est.eqty
         eb.est-type      = est.est-type
         ef.est-type      = est.est-type.

        IF FIRST(b-eb.est-no) AND NOT ip-add-eb THEN est-qty.eqty = tt-est.eqty.

        eb.eqty = ef.eqty.
      END.

      SESSION:SET-WAIT-STATE ("").
    END.  /* ll-ans */
  END.

  ELSE
  IF op-success THEN DO:
    ll-ans = NO.
    MESSAGE "You have not selected any items, do you wish to exit?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
    IF ll-ans THEN op-success = NO.
    ELSE RETURN NO-APPLY. 
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_die-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_die-no D-Dialog
ON LEAVE OF fi_die-no IN FRAME D-Dialog /* Die# */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF {&self-name}:SCREEN-VALUE NE "" THEN APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_die-no D-Dialog
ON RETURN OF fi_die-no IN FRAME D-Dialog /* Die# */
DO:
  APPLY "tab" TO {&self-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no D-Dialog
ON LEAVE OF fi_est-no IN FRAME D-Dialog /* Estimate */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF {&self-name}:SCREEN-VALUE NE "" THEN APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no D-Dialog
ON RETURN OF fi_est-no IN FRAME D-Dialog /* Estimate */
DO:
  APPLY "tab" TO {&self-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ON 'mouse-select-click':U OF tt-est.selekt IN BROWSE {&browse-name} 
DO:
    DEF VAR lv-rowid AS ROWID NO-UNDO.

    IF SELF:SCREEN-VALUE EQ "Yes" THEN SELF:SCREEN-VALUE = "No".
    ELSE DO:
      SELF:SCREEN-VALUE = "Yes".
      lv-rowid = ROWID(tt-est).
      FOR EACH b-tt-est WHERE ip-add-eb OR ROWID(b-tt-est) EQ lv-rowid:
        b-tt-est.selekt = ROWID(b-tt-est) EQ lv-rowid.
      END.
      {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
      REPOSITION {&browse-name} TO ROWID lv-rowid NO-ERROR.
    END.
END.

SESSION:DATA-ENTRY-RETURN = YES.

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL eb THEN lv-est-type = IF eb.est-type GT 4 THEN 8 ELSE 4.

IF lv-est-type EQ 0 THEN DO:
   FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
   ASSIGN
      lv-est-type = IF AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware" THEN 8
                    ELSE IF sys-ctrl.char-fld EQ "Foldware" THEN 4
                    ELSE 9
      lv-both     = IF sys-ctrl.char-fld EQ "Both" THEN YES ELSE NO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-loc LIKE eb.loc INIT "" NO-UNDO.

  EMPTY TEMP-TABLE tt-est.
                             
  IF fi_est-no NE "" THEN
  FOR EACH eb
      WHERE eb.company  EQ cocode
        AND eb.est-no   EQ fi_est-no
        AND eb.die-no   BEGINS fi_die-no
        AND (TRIM(eb.master-est-no) EQ "" OR ip-add-eb)
        AND CAN-FIND(FIRST ef OF eb)
        AND ROWID(eb)   NE ip-rowid
      NO-LOCK,
      FIRST est OF eb WHERE est.e-num EQ 0 NO-LOCK:

      IF lv-both EQ NO AND
         NOT(eb.est-type GE lv-est-type - 3 AND eb.est-type LE lv-est-type) THEN
         NEXT.

    RUN create-tt.
  END.

  ELSE
  IF fi_die-no NE "" THEN DO WHILE TRUE:
    FIND FIRST eb
        WHERE eb.company EQ cocode
          AND eb.loc     GT lv-loc
          AND CAN-FIND(FIRST ef OF eb)
        NO-LOCK NO-ERROR.
    IF NOT AVAIL eb THEN LEAVE.

    lv-loc = eb.loc.

    FOR EACH eb
        WHERE eb.company EQ cocode
          AND eb.loc     EQ lv-loc
          AND eb.die-no  BEGINS fi_die-no
          AND (TRIM(eb.master-est-no) EQ "" OR ip-add-eb)
          AND CAN-FIND(FIRST ef OF eb)
        USE-INDEX die NO-LOCK,
        FIRST est OF eb WHERE est.e-num EQ 0 NO-LOCK:

        IF lv-both EQ NO AND
           NOT(eb.est-type GE lv-est-type - 3 AND
               eb.est-type LE lv-est-type) THEN NEXT.

      RUN create-tt.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt D-Dialog 
PROCEDURE create-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-valid AS LOG NO-UNDO.
  
  FIND FIRST est
      WHERE est.company EQ eb.company
        AND est.est-no  EQ eb.est-no
      NO-LOCK NO-ERROR.

  IF AVAIL est THEN
    IF (lv-both EQ NO AND est.est-type EQ lv-est-type - 3) OR
       (lv-both EQ YES AND (est.est-type EQ 1 OR est.est-type EQ 5)) THEN
       ll-valid = YES.
    ELSE
       RUN ce/com/istandem.p (IF ip-add-eb AND NOT ll-master THEN ip-rowid
                              ELSE ROWID(est), OUTPUT ll-valid).

  IF ll-valid AND eb.stock-no NE "" THEN DO:
      RUN fg/GetItemfgActInact.p (INPUT eb.company,
                                  INPUT eb.stock-no,
                                  OUTPUT ll-valid).
/*     FIND FIRST reftable                        */
/*         WHERE reftable.reftable EQ "FGSTATUS"  */
/*           AND reftable.company  EQ eb.company  */
/*           AND reftable.loc      EQ ""          */
/*           AND reftable.code     EQ eb.stock-no */
/*         NO-LOCK NO-ERROR.                      */
/*     IF AVAIL reftable AND reftable.code2 EQ "I" THEN ll-valid = NO. */
  END.

  IF ll-valid THEN DO:
     CREATE tt-est.
     ASSIGN
      tt-est.eqty   = eb.yld-qty
      tt-est.row-id = ROWID(eb)
      tt-est.est-no = eb.est-no 
      tt-est.die-no = eb.die-no
      tt-est.selekt = NO
      tt-est.est-type = est.est-type.
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
  DISPLAY fi_est-no fi_die-no 
      WITH FRAME D-Dialog.
  ENABLE fi_est-no fi_die-no btn_go BROWSE-2 btn_ok 
      WITH FRAME D-Dialog.
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
  IF ip-add-eb THEN DO WITH FRAME {&FRAME-NAME}:
    DISABLE fi_est-no fi_die-no btn_go.
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
  DEF BUFFER b-eb FOR eb.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF ip-add-eb THEN DO:
    FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAIL eb THEN DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
       fi_est-no:SCREEN-VALUE = eb.est-no
       fi_est-no              = eb.est-no.

      FOR EACH b-eb
          WHERE b-eb.company       EQ eb.company
            AND b-eb.est-no        EQ eb.est-no
            AND b-eb.master-est-no NE "" 
            AND b-eb.master-est-no NE eb.est-no
          NO-LOCK:
        ASSIGN
         fi_est-no:SCREEN-VALUE = b-eb.master-est-no
         fi_est-no              = b-eb.master-est-no
         ll-master              = YES.
        LEAVE.
      END.

      ASSIGN
       fi_die-no:SCREEN-VALUE = ""
       fi_die-no              = "".

      RUN build-table.
    END.

    ELSE ip-add-eb = NO.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "tt-est"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no D-Dialog 
PROCEDURE valid-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fi_est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(fi_est-no:SCREEN-VALUE))) +
                             TRIM(fi_est-no:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

