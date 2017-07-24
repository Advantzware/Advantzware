&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewerid/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

{custom/globdefs.i}
{sys/inc/var.i "new shared"}
assign cocode = g_company
       locode = g_loc.

{sys/inc/oereordr.i}

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
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.i-no oe-ordl.ord-no oe-ordl.est-no 
&Scoped-define ENABLED-TABLES oe-ordl
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-Define ENABLED-OBJECTS RECT-37 RECT-38 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.i-no oe-ordl.i-name oe-ordl.ord-no ~
oe-ordl.est-no 
&Scoped-define DISPLAYED-TABLES oe-ordl
&Scoped-define FIRST-DISPLAYED-TABLE oe-ordl
&Scoped-Define DISPLAYED-OBJECTS li-on-hand li-on-order li-alloc ~
li-backorder li-avail li-reorder li-comm 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,List-4,List-5,F1   */

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
DEFINE VARIABLE li-alloc AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE li-avail AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE li-backorder AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE li-comm AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1 NO-UNDO.

DEFINE VARIABLE li-on-hand AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE li-on-order AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE li-reorder AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 2.38.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 4.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ordl.i-no AT ROW 1.24 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     oe-ordl.i-name AT ROW 1.24 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     oe-ordl.ord-no AT ROW 1.24 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-ordl.est-no AT ROW 1.24 COL 129 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     li-on-hand AT ROW 3.38 COL 14.6 COLON-ALIGNED NO-LABEL
     li-on-order AT ROW 3.38 COL 33.4 COLON-ALIGNED NO-LABEL
     li-alloc AT ROW 3.38 COL 52.8 COLON-ALIGNED NO-LABEL
     li-backorder AT ROW 3.38 COL 72 COLON-ALIGNED NO-LABEL
     li-avail AT ROW 3.38 COL 91 COLON-ALIGNED NO-LABEL
     li-reorder AT ROW 3.38 COL 110 COLON-ALIGNED NO-LABEL
     li-comm AT ROW 3.38 COL 129.2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "Committed" VIEW-AS TEXT
          SIZE 12.4 BY .62 AT ROW 2.67 COL 131.2 WIDGET-ID 4
     "FG Qtys" VIEW-AS TEXT
          SIZE 10 BY 1.91 AT ROW 2.67 COL 5
     "On Hand" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.67 COL 16.6
     "Allocated" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.67 COL 54.8
     "Available" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.67 COL 93
     "Backorder" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.67 COL 74
     "Reorder" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.67 COL 112
     "On Order" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.67 COL 35.4
     RECT-37 AT ROW 2.43 COL 2
     RECT-38 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ordl
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
         HEIGHT             = 4.19
         WIDTH              = 146.6.
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

/* SETTINGS FOR FILL-IN oe-ordl.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-ordl.i-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-alloc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-avail IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-backorder IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-comm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-on-hand IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-on-order IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-reorder IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-alloc-qty V-table-Win 
PROCEDURE calc-alloc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param v-alloc as int no-undo.
def var v-type as cha no-undo.

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER s-code FOR reftable.

find first itemfg where itemfg.company = cocode and
                        itemfg.i-no = oe-ordl.i-no
                        no-lock no-error.                          
v-alloc = 0.
FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK.

for each b-oe-ordl where b-oe-ordl.company eq cocode
                   and b-oe-ordl.i-no    eq itemfg.i-no
                   and b-oe-ordl.opened  eq yes
            use-index item no-lock,   
    first oe-ord where oe-ord.company eq cocode
          and oe-ord.ord-no  eq b-oe-ordl.ord-no
          and oe-ord.type ne "T"
          use-index ord-no no-lock:

    for each oe-rel where oe-rel.company eq cocode
          and oe-rel.ord-no  eq b-oe-ordl.ord-no
          and oe-rel.i-no    eq b-oe-ordl.i-no
          and oe-rel.line    eq b-oe-ordl.line
          use-index ord-item no-lock:

        FIND FIRST s-code WHERE
                   s-code.reftable EQ "oe-rel.s-code" AND
                   s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
                   NO-LOCK NO-ERROR.

       {oe/rel-stat.i v-type}

       if v-type eq "P" then DO:
          IF AVAIL s-code AND s-code.CODE <> "I" THEN
             ASSIGN v-alloc = v-alloc + oe-rel.qty.
       END.
       else
       if v-type eq "Z" and not oe-ctrl.u-inv THEN
       for each inv-line 
           where inv-line.company eq cocode
             and inv-line.ord-no  eq oe-boll.ord-no
             and inv-line.b-no    eq oe-boll.b-no
             and inv-line.i-no    eq oe-boll.i-no
             and inv-line.line    eq oe-boll.line
           no-lock:                                     
           ASSIGN v-alloc = v-alloc + inv-line.ship-qty.
       end.
    end.

    FOR EACH oe-rell
        WHERE oe-rell.company EQ cocode
          AND oe-rell.ord-no  EQ b-oe-ordl.ord-no
          AND oe-rell.i-no    EQ b-oe-ordl.i-no
          AND oe-rell.line    EQ b-oe-ordl.line
          AND CAN-FIND(FIRST oe-relh
                       WHERE oe-relh.r-no    EQ oe-rell.r-no
                         AND oe-relh.posted  EQ NO
                         AND oe-relh.deleted EQ NO)
        USE-INDEX ord-no NO-LOCK:

        IF oe-rell.s-code <> "I" THEN 
           ASSIGN v-alloc = v-alloc + oe-rell.qty.  
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgqtys V-table-Win 
PROCEDURE display-fgqtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var v-use-rel as log no-undo.
 DEFINE VARIABLE iCommit AS INTEGER NO-UNDO.

  find first sys-ctrl where sys-ctrl.company eq oe-ordl.company
                      and sys-ctrl.name    eq "OEREORDR"
       no-lock no-error.
  v-use-rel = if avail sys-ctrl then sys-ctrl.log-fld else no.

  find first itemfg where itemfg.company = oe-ordl.company and
                          itemfg.i-no = oe-ordl.i-no
                          no-lock NO-ERROR.
  if not avail itemfg then return error.

  /*if v-use-rel then run calc-alloc-qty (output li-alloc).
  else li-alloc = itemfg.q-alloc.*/

 ASSIGN iCommit =  0.
  FOR EACH oe-rel NO-LOCK WHERE oe-rel.company EQ oe-ordl.company
                            AND oe-rel.ord-no  EQ oe-ordl.ord-no
                            AND oe-rel.i-no    EQ oe-ordl.i-no 
                            AND oe-rel.LINE    EQ oe-ordl.LINE 
                            AND oe-rel.link-no EQ 0 :
      iCommit = iCommit + oe-rel.tot-qty .

  END.
  ASSIGN li-comm = iCommit .

  IF oereordr-log OR oereordr-log EQ ? THEN
      RUN oe/oereordr.p (BUFFER itemfg, INPUT oereordr-log, OUTPUT li-alloc).
  ELSE li-alloc = itemfg.q-alloc.

  if avail itemfg then assign li-on-hand = itemfg.q-onh
                              li-on-order = itemfg.q-ono
                              /*li-alloc = itemfg.q-alloc */
                              li-backorder = itemfg.q-back
                              li-avail = itemfg.q-onh + /*itemfg.q-ono*/
                                         (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg.q-ono) - li-alloc
                              li-reorder = itemfg.ord-level.
  else assign li-on-hand = 0
              li-on-order = 0
              li-alloc = 0
              li-backorder = 0
              li-avail = 0
              li-reorder = 0
              li-comm  = 0.

 display li-on-hand
         li-on-order li-alloc 
         li-backorder 
         li-avail 
         li-reorder
         li-comm with frame {&frame-name}.

 li-reorder:bgcolor = if (li-on-hand + li-on-order - li-alloc) le li-reorder and li-reorder gt 0
                      then 12 else ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"oe-qtys-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN get-qtys IN WIDGET-HANDLE(char-hdl) (OUTPUT li-on-hand,
                                             OUTPUT li-on-order,
                                             OUTPUT li-alloc,
                                             OUTPUT li-backorder,
                                             OUTPUT li-avail,
                                             OUTPUT li-reorder).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run display-fgqtys.

  DO WITH FRAME {&FRAME-NAME}:
    li-reorder:BGCOLOR =
      IF (li-on-hand + li-on-order - li-alloc) LE li-reorder AND
         li-reorder GT 0                                     THEN 12 ELSE ?.
  END.

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

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

