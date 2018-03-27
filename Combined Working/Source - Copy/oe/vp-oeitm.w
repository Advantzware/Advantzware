&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\vp-oeitm.w
  
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

/* Local Variable Definitions ---                                       */
{methods/prgsecdt.i}
{sys/inc/VAR.i "new shared"}
ASSIGN cocode = g_company
       locode = g_loc.

DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.

DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF NEW SHARED var nufile AS LOG INIT YES NO-UNDO.
DEF NEW SHARED var v-create-job AS LOG INIT YES NO-UNDO.
DEF NEW SHARED var v-qty-mod AS LOG INIT YES NO-UNDO.

DEF VAR char-hdl AS cha no-undo.
DEF VAR ll-canceled AS LOG NO-UNDO.
DEF VAR v-out-rowid-list AS CHAR NO-UNDO.
DEF VAR ll-has-components AS LOG NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.

{oe/tt-item-qty-price.i}

DEF TEMP-TABLE w-matrix NO-UNDO
    FIELD qty     AS DEC
    FIELD uom     AS CHAR
    FIELD price   AS DEC
    FIELD price-m AS DEC.

DO TRANSACTION:
  {sys/inc/addrelse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ordl oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl, oe-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-39 Btn-View Btn-Save Btn-Add Btn-Delete ~
btn-price btn-stat btn-his Btn-Rebuild Btn-Update 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Add 
     LABEL "&Add" 
     SIZE 13 BY 1.29
     FONT 4.

DEFINE BUTTON Btn-Delete 
     LABEL "&Delete" 
     SIZE 13 BY 1.29
     FONT 4.

DEFINE BUTTON btn-his 
     LABEL "&History" 
     SIZE 13 BY 1.29.

DEFINE BUTTON btn-price 
     LABEL "&Price" 
     SIZE 13 BY 1.29.

DEFINE BUTTON Btn-Rebuild 
     LABEL "Rebuild &Job Stds" 
     SIZE 18 BY 1.29
     FONT 4.

DEFINE BUTTON Btn-Save 
     LABEL "&Update" 
     SIZE 13 BY 1.29
     FONT 4.

DEFINE BUTTON btn-stat 
     LABEL "S&tat" 
     SIZE 13 BY 1.29.

DEFINE BUTTON Btn-Update 
     LABEL "Price &Mtx" 
     SIZE 13 BY 1.29
     FONT 4.

DEFINE BUTTON Btn-View 
     LABEL "&View" 
     SIZE 13 BY 1.29
     FONT 4.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn-View AT ROW 1.24 COL 2
     Btn-Save AT ROW 1.24 COL 15
     Btn-Add AT ROW 1.24 COL 28
     Btn-Delete AT ROW 1.24 COL 41
     btn-price AT ROW 1.24 COL 54
     btn-stat AT ROW 1.24 COL 67
     btn-his AT ROW 1.24 COL 80
     Btn-Rebuild AT ROW 1.24 COL 93
     Btn-Update AT ROW 1.24 COL 111
     RECT-39 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ordl,ASI.oe-ord
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.29
         WIDTH              = 150.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add V-table-Win
ON CHOOSE OF Btn-Add IN FRAME F-Main /* Add */
DO:
  RUN add-rebuild ("ADD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete V-table-Win
ON CHOOSE OF Btn-Delete IN FRAME F-Main /* Delete */
DO:
  RUN delete-process (INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-his
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-his V-table-Win
ON CHOOSE OF btn-his IN FRAME F-Main /* History */
DO:
   run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
   run select-his in widget-handle(char-hdl).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-price V-table-Win
ON CHOOSE OF btn-price IN FRAME F-Main /* Price */
DO:
  IF AVAIL oe-ordl AND NOT oe-ordl.is-a-component THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
    RUN select-price IN WIDGET-HANDLE(char-hdl).

    RUN reopen-oe-ord-query.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Rebuild
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Rebuild V-table-Win
ON CHOOSE OF Btn-Rebuild IN FRAME F-Main /* Rebuild Job Stds */
DO:
  RUN add-rebuild ("REBUILD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save V-table-Win
ON CHOOSE OF Btn-Save IN FRAME F-Main /* Update */
DO:
  DEF VAR lv-prev-qty LIKE oe-ordl.qty NO-UNDO.

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  IF AVAIL oe-ordl AND NOT oe-ordl.is-a-component THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

    lv-prev-qty = oe-ordl.qty.

    RUN set-header.

    RUN oe/d-oeitem.w (RECID(oe-ordl), oe-ordl.ord-no,"Update",INPUT TABLE tt-item-qty-price,
                       OUTPUT v-out-rowid-list, OUTPUT ll-canceled).

    FIND CURRENT oe-ordl NO-LOCK NO-ERROR.

    IF lv-prev-qty NE oe-ordl.qty AND ll-has-components THEN
      RUN update-components IN WIDGET-HANDLE(char-hdl).

    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.

    RUN record-updated IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ordl)).

    /*RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), IF AVAIL b-oe-ordl THEN RECID(b-oe-ordl) ELSE ?).*/

    /*RUN reopen-oe-ord-query. */

      /* If user has selected multiple records during 'add', then run proper validation on them via d-oeitem */
      IF v-out-rowid-list NE "" THEN DO:
          DO li = 1 TO NUM-ENTRIES(v-out-rowid-list):
            IF v-out-rowid-list = "" THEN
                LEAVE.
            lv-rowid = TO-ROWID(ENTRY(li, v-out-rowid-list)).

            RUN update-process (INPUT lv-rowid).
          END.
          v-out-rowid-list = "".
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-stat V-table-Win
ON CHOOSE OF btn-stat IN FRAME F-Main /* Stat */
DO:
   run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
   run select-stat in widget-handle(char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Update V-table-Win
ON CHOOSE OF Btn-Update IN FRAME F-Main /* Price Mtx */
DO:
  DEF VAR ll-ans AS LOG NO-UNDO.

  IF AVAIL oe-ordl THEN DO:
    MESSAGE "This will create/update FG price matrix record from this Order Line Item" SKIP
            "Do you want to continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-ans.

    IF ll-ans THEN
    FOR FIRST itemfg
        WHERE itemfg.company EQ oe-ordl.company
          AND itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK:

      RUN oe/updprmtx.p (ROWID(itemfg),
                         oe-ord.cust-no,
                         oe-ordl.qty,
                         oe-ordl.pr-uom,
                         oe-ordl.price).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-View
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-View V-table-Win
ON CHOOSE OF Btn-View IN FRAME F-Main /* View */
DO:
    run oe/d-oeitem.w (recid(oe-ordl), oe-ordl.ord-no, "View",INPUT TABLE tt-item-qty-price,
                       OUTPUT v-out-rowid-list, OUTPUT ll-canceled).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-auto V-table-Win 
PROCEDURE add-auto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "choose" TO btn-add IN FRAME {&FRAME-NAME} .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-rebuild V-table-Win 
PROCEDURE add-rebuild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-function AS CHAR NO-UNDO.

  DEF VAR ll-combo AS LOG NO-UNDO.
  DEF VAR lv-job AS CHAR NO-UNDO.
  DEF VAR li AS INT.
  DEF VAR lv-rowid AS ROWID.

  DEF BUFFER b-oe-ordl FOR oe-ordl.

  IF AVAIL oe-ord THEN
  DO:
    ASSIGN
     ll-canceled = NO
     lv-job      = oe-ord.job-no + "-" + STRING(oe-ord.job-no2,"99").
   
    IF AVAIL oe-ordl THEN
      lv-job = oe-ordl.job-no + "-" + STRING(oe-ordl.job-no2,"99").
   
    RELEASE est.
   
    IF oe-ord.est-no NE "" OR (AVAIL oe-ordl AND oe-ordl.est-no NE "") THEN
    FIND FIRST est NO-LOCK
        WHERE est.company EQ oe-ord.company
          AND est.est-no  EQ (IF AVAIL oe-ordl        AND
                                 oe-ordl.est-no NE "" THEN oe-ordl.est-no
                                                      ELSE oe-ord.est-no)
        NO-ERROR.
   
    IF ip-function EQ "REBUILD" THEN DO:
      ll-canceled = AVAIL est.
          
      IF ll-canceled THEN
        MESSAGE "Do you wish rebuild job standards for Job#: " + TRIM(lv-job)
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE ll-canceled.
   
      ll-canceled = NOT ll-canceled. 
   
      IF ll-canceled THEN ip-function = "CANCEL".
    END.
    
    ELSE ll-combo = AVAIL est AND
                    (est.est-type EQ 3 OR est.est-type EQ 4 OR est.est-type EQ 8).
   
    IF ll-combo THEN
      RUN oe/d-addlin.w (CAN-FIND(FIRST eb
                                  WHERE eb.company       EQ oe-ord.company
                                    AND eb.est-no        EQ est.est-no
                                    AND eb.master-est-no NE "" 
                                    AND eb.master-est-no NE est.est-no),
                         OUTPUT ip-function).
   
    IF ip-function EQ "REBUILD" THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
   
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN get-link-handle IN adm-broker-hdl(WIDGET-HANDLE(char-hdl),"record-source", OUTPUT char-hdl).
   
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
        SESSION:SET-WAIT-STATE ("general").
   
        IF ll-combo THEN DO:
          RUN order-from-est IN WIDGET-HANDLE(char-hdl) (?).
   
          RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
          IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
            RUN reopen-query IN WIDGET-HANDLE(char-hdl) (?).
   
            FOR EACH b-oe-ordl OF oe-ord
                WHERE b-oe-ordl.est-no EQ oe-ord.est-no
                  AND NOT CAN-FIND(FIRST eb
                                   WHERE eb.company EQ b-oe-ordl.company
                                     AND eb.est-no  EQ b-oe-ordl.est-no
                                     AND eb.cust-no EQ b-oe-ordl.cust-no
                                     AND (eb.part-no EQ b-oe-ordl.part-no OR
                                          (eb.stock-no EQ b-oe-ordl.i-no AND eb.stock-no NE "")))
                NO-LOCK:
              RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), RECID(b-oe-ordl)).
              IF ROWID(oe-ordl) EQ ROWID(b-oe-ordl) THEN
                RUN delete-item IN WIDGET-HANDLE(char-hdl) (NO, NO).
            END.
          END.
        END.
   
        ELSE DO:
          ASSIGN
           fil_id       = RECID(oe-ordl)
           nufile       = YES
           v-create-job = YES
           v-qty-mod    = YES.
   
          FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) NO-LOCK.
          RUN oe/estupl.p.
        END.
   
        SESSION:SET-WAIT-STATE ("").
      END.
    END.
   
    ELSE
    IF ip-function NE "CANCEL" THEN
      RUN oe/d-oeitem.w (?, oe-ord.ord-no, ip-function, INPUT TABLE tt-item-qty-price, 
                         OUTPUT v-out-rowid-list, OUTPUT ll-canceled).

    IF NOT ll-canceled THEN DO:
      RELEASE b-oe-ordl.
   
      FOR EACH b-oe-ordl OF oe-ord NO-LOCK BY b-oe-ordl.line DESC:
        LEAVE.
      END.
      fil_id = IF AVAIL b-oe-ordl THEN RECID(b-oe-ordl) ELSE
               IF AVAIL oe-ordl   THEN RECID(oe-ordl)   ELSE ?.
   
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
      RUN reopen-query IN WIDGET-HANDLE(char-hdl) (?).
      RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), fil_id).
      
      /* If user has selected multiple records during 'add', then run proper validation on them via d-oeitem */
      IF v-out-rowid-list NE "" THEN DO:
          DO li = 1 TO NUM-ENTRIES(v-out-rowid-list):
            IF v-out-rowid-list = "" THEN
                LEAVE.
            lv-rowid = TO-ROWID(ENTRY(li, v-out-rowid-list)).
            RUN update-process (INPUT lv-rowid).
          END.
          v-out-rowid-list = "".
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "oe-ordl"}
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}
  {src/adm/template/row-find.i "oe-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-process V-table-Win 
PROCEDURE delete-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-prompt AS LOG NO-UNDO.
  DEF VAR lv-set-line LIKE oe-ordl.set-hdr-line NO-UNDO.
  DEF VAR ll-comps AS LOG NO-UNDO.

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER c-oe-ordl FOR oe-ordl.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) AND
     AVAIL oe-ordl                         AND
     NOT oe-ordl.is-a-component            THEN DO:

    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.

    ll-comps = NO.

    /* delete components */
    delete-comps: DO TRANSACTION.
      FOR EACH c-oe-ordl {sys/inc/ordlcomp.i c-oe-ordl b-oe-ordl}
          BREAK BY c-oe-ordl.line:
        ll-comps = YES.
        RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), RECID(c-oe-ordl)).       
        IF ROWID(oe-ordl) EQ ROWID(c-oe-ordl) THEN DO:
          RUN delete-item IN WIDGET-HANDLE(char-hdl) (FIRST(c-oe-ordl.line), YES).
          IF ROWID(oe-ordl) EQ ROWID(c-oe-ordl) THEN DO:
            IF TRUE THEN UNDO delete-comps.
            RUN dispatch IN WIDGET-HANDLE(char-hdl) ("open-query").
            LEAVE.
          END.
        END.
      END.
    END. /* delete-comps */
    
    RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), RECID(b-oe-ordl)).
    IF ip-prompt THEN DO:
        IF NOT CAN-FIND(FIRST c-oe-ordl {sys/inc/ordlcomp.i c-oe-ordl b-oe-ordl}) THEN
             RUN delete-item IN WIDGET-HANDLE(char-hdl) (NOT ll-comps, ll-comps).
    END.
    ELSE DO:
        IF NOT CAN-FIND(FIRST c-oe-ordl {sys/inc/ordlcomp.i c-oe-ordl b-oe-ordl}) THEN
             RUN delete-item IN WIDGET-HANDLE(char-hdl) (NO, ll-comps).
    END.
    /*
    RUN reopen-oe-ord-query.
    */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-bol AS LOG NO-UNDO.
  DEF VAR ll-inv AS LOG NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.
  DEF VAR lAccess AS LOG NO-UNDO.
  DEF VAR lAccessClose AS LOG NO-UNDO.
  DEF VAR cAccessList AS CHAR NO-UNDO.

  

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT AVAIL oe-ord AND AVAIL oe-ordl THEN
  FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

  DO WITH FRAME {&FRAME-NAME}:
    
    IF AVAIL oe-ord AND NOT oe-ord.opened THEN
      DISABLE Btn-Save Btn-Add Btn-Delete Btn-Price .
    ELSE DO:
      ENABLE Btn-Save Btn-Add Btn-Delete Btn-Price .

      FOR EACH oe-rel NO-LOCK
          WHERE oe-rel.company EQ oe-ord.company
            AND oe-rel.ord-no  EQ oe-ord.ord-no
            AND oe-rel.link-no EQ 0
          USE-INDEX ord-item
          BREAK BY oe-ord.ord-no:

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

        IF CAN-DO("S,L,I",lv-stat) THEN DO:
          
          IF oe-rel.s-code EQ "I" THEN ll-inv = YES.
                                                 ELSE ll-bol = YES.

          IF ll-bol AND ll-inv THEN LEAVE.
        END.
      END.
      /*
      IF NOT ll-inv AND NOT ll-bol THEN DISABLE Btn-Bol.
      ELSE
      IF ll-inv THEN
        IF ll-bol THEN Btn-Bol:LABEL = "&BOL/Invoice".
                  ELSE Btn-Bol:LABEL = "&Invoice".
      */            
    END.
  END.

  IF NOT AVAIL oe-ord OR oe-ord.opened THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT v-can-create THEN ASSIGN  btn-add:SENSITIVE = NO.
    IF NOT v-can-update THEN ASSIGN btn-save:SENSITIVE = NO
/*                                    btn-price:SENSITIVE = NO */
/*                                     btn-stat:SENSITIVE = NO */
/*                                   btn-update:SENSITIVE = NO */
                                .
    IF NOT v-can-delete THEN btn-delete:SENSITIVE = NO.
    IF v-can-create              AND
       btn-Save:LABEL EQ "&Save" THEN btn-save:SENSITIVE = YES.

    /*IF NOT v-do-bol THEN btn-bol:SENSITIVE = NO.*/
    
    RUN methods/prgsecur.p(INPUT "OEItmPrc",
                             INPUT "Update", /* based on run, create, update, delete or all */
                             INPUT NO,    /* use the directory in addition to the program */
                             INPUT NO,    /* Show a message if not authorized */
                             INPUT NO,    /* Group overrides user security? */
                             OUTPUT lAccess, /* Allowed? Yes/NO */
                             OUTPUT lAccessClose, /* used in template/windows.i  */
                             OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
    IF NOT lAccess THEN btn-price:SENSITIVE = NO.
    RUN methods/prgsecur.p(INPUT "OEItmSta",
                             INPUT "Update", /* based on run, create, update, delete or all */
                             INPUT NO,    /* use the directory in addition to the program */
                             INPUT NO,    /* Show a message if not authorized */
                             INPUT NO,    /* Group overrides user security? */
                             OUTPUT lAccess, /* Allowed? Yes/NO */
                             OUTPUT lAccessClose, /* used in template/windows.i  */
                             OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
    IF NOT lAccess THEN btn-stat:SENSITIVE = NO.
    RUN methods/prgsecur.p(INPUT "OEItmHis",
                             INPUT "Update", /* based on run, create, update, delete or all */
                             INPUT NO,    /* use the directory in addition to the program */
                             INPUT NO,    /* Show a message if not authorized */
                             INPUT NO,    /* Group overrides user security? */
                             OUTPUT lAccess, /* Allowed? Yes/NO */
                             OUTPUT lAccessClose, /* used in template/windows.i  */
                             OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
    IF NOT lAccess THEN btn-his:SENSITIVE = NO.
    RUN methods/prgsecur.p(INPUT "OEItmJob",
                             INPUT "Update", /* based on run, create, update, delete or all */
                             INPUT NO,    /* use the directory in addition to the program */
                             INPUT NO,    /* Show a message if not authorized */
                             INPUT NO,    /* Group overrides user security? */
                             OUTPUT lAccess, /* Allowed? Yes/NO */
                             OUTPUT lAccessClose, /* used in template/windows.i  */
                             OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
    IF NOT lAccess THEN btn-Rebuild:SENSITIVE = NO.
    RUN methods/prgsecur.p(INPUT "OEItmPrm",
                             INPUT "Update", /* based on run, create, update, delete or all */
                             INPUT NO,    /* use the directory in addition to the program */
                             INPUT NO,    /* Show a message if not authorized */
                             INPUT NO,    /* Group overrides user security? */
                             OUTPUT lAccess, /* Allowed? Yes/NO */
                             OUTPUT lAccessClose, /* used in template/windows.i  */
                             OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
    IF NOT lAccess THEN btn-update:SENSITIVE = NO.
    

    IF NOT v-can-run THEN DISABLE ALL.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-oe-ord-query V-table-Win 
PROCEDURE reopen-oe-ord-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR no-undo.
      
  
  run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
  run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl).
  run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl).

  run reopen-query1 in widget-handle(char-hdl) (rowid(oe-ordl)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "oe-ordl"}
  {src/adm/template/snd-list.i "oe-ord"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-header V-table-Win 
PROCEDURE set-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ll-has-components = NO.
                      /*AVAIL oe-ordl AND
                      CAN-FIND(FIRST {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-item V-table-Win 
PROCEDURE update-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  run oe/d-oeitem.w (recid(oe-ordl), oe-ordl.ord-no,"Update",INPUT TABLE tt-item-qty-price,
                     OUTPUT v-out-rowid-list, OUTPUT ll-canceled).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-process V-table-Win 
PROCEDURE update-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.
  DEF VAR lv-prev-qty LIKE oe-ordl.qty NO-UNDO.
  DEF VAR v-rowid-list AS CHAR NO-UNDO.
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  FIND oe-ordl WHERE ROWID(oe-ordl) EQ ipr-ordl-row EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL oe-ordl THEN
      RETURN NO-APPLY.

  IF AVAIL oe-ordl AND NOT oe-ordl.is-a-component THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

    lv-prev-qty = oe-ordl.qty.

    RUN set-header.

    RUN oe/d-oeitem.w (RECID(oe-ordl), oe-ordl.ord-no,"Update-3",INPUT TABLE tt-item-qty-price,
                       OUTPUT v-rowid-list, OUTPUT ll-canceled).
    IF ll-canceled THEN DO:
        RUN delete-process (INPUT NO).
    END.
    ELSE DO:
        FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
    
        IF lv-prev-qty NE oe-ordl.qty AND ll-has-components THEN
          RUN update-components IN WIDGET-HANDLE(char-hdl).
    
        FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.
    
        RUN record-updated IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ordl)).
    
        /*RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), IF AVAIL b-oe-ordl THEN RECID(b-oe-ordl) ELSE ?).*/
    
        /*RUN reopen-oe-ord-query. */
    END.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

