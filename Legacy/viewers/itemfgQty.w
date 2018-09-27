&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

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
{custom/gcompany.i}
{custom/gloc.i}

&SCOPED-DEFINE itemfg2-maint itemfg2-maint
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

def var uom-list as cha init "C,CS,EA,L,M" no-undo.
DEF VAR v-prior-i-no AS CHAR NO-UNDO.
DEF VAR v-whseadded AS LOG NO-UNDO.
DEFINE VARIABLE h_w-inqord AS HANDLE      NO-UNDO.
&Scoped-define List-buttons btn_onh btn_ono btn_all

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
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btAddLoc cbLoc btn_onh btn_ono btn_all 
&Scoped-Define DISPLAYED-FIELDS itemfg.beg-bal 
&Scoped-define DISPLAYED-TABLES itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg
&Scoped-Define DISPLAYED-OBJECTS cbLoc dq-onh dq-ono dq-alloc dq-back ~
dq-avail 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS itemfg.beg-bal dq-onh dq-ono dq-alloc ~
dq-back dq-avail 
&Scoped-define List-5 itemfg.beg-bal dq-onh dq-ono dq-alloc dq-back 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-alloc V-table-Win 
FUNCTION get-alloc RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btAddLoc 
     LABEL "+" 
     SIZE 6 BY .95
     FONT 6.

DEFINE BUTTON btn_all 
     LABEL "Alloc to Orders" 
     SIZE 20 BY 1.43.

DEFINE BUTTON btn_onh 
     LABEL "On Hand" 
     SIZE 20 BY 1.43.

DEFINE BUTTON btn_ono 
     LABEL "Job/PO On Ord" 
     SIZE 20 BY 1.43.

DEFINE VARIABLE cbLoc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Warehouse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "ALL","ALL",
                     "Texas","Texas",
                     "NJ","NJ"
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE dq-alloc AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE dq-avail AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE dq-back AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE dq-onh AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE dq-ono AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 130 BY 4.76.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 123 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btAddLoc AT ROW 4.57 COL 81 WIDGET-ID 4
     cbLoc AT ROW 4.48 COL 46 COLON-ALIGNED WIDGET-ID 2
     btn_onh AT ROW 1.48 COL 28
     btn_ono AT ROW 1.48 COL 48
     btn_all AT ROW 1.48 COL 68
     itemfg.beg-bal AT ROW 2.91 COL 6 COLON-ALIGNED NO-LABEL FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 
     dq-onh AT ROW 2.91 COL 26 COLON-ALIGNED NO-LABEL
     dq-ono AT ROW 2.91 COL 46 COLON-ALIGNED NO-LABEL
     dq-alloc AT ROW 2.91 COL 66 COLON-ALIGNED NO-LABEL
     dq-back AT ROW 2.91 COL 86 COLON-ALIGNED NO-LABEL
     dq-avail AT ROW 2.91 COL 106 COLON-ALIGNED NO-LABEL
     "Qty" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 2.91 COL 2
          FGCOLOR 9 
     "Begin Balance" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.71 COL 8
     "Backordered" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 1.71 COL 89
     "Available" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 1.71 COL 108
     RECT-22 AT ROW 1 COL 1
     RECT-24 AT ROW 1.24 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.itemfg
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
         HEIGHT             = 4.76
         WIDTH              = 130.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN itemfg.beg-bal IN FRAME F-Main
   NO-ENABLE 2 5 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN dq-alloc IN FRAME F-Main
   NO-ENABLE 2 5                                                        */
/* SETTINGS FOR FILL-IN dq-avail IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN dq-back IN FRAME F-Main
   NO-ENABLE 2 5                                                        */
/* SETTINGS FOR FILL-IN dq-onh IN FRAME F-Main
   NO-ENABLE 2 5                                                        */
/* SETTINGS FOR FILL-IN dq-ono IN FRAME F-Main
   NO-ENABLE 2 5                                                        */
/* SETTINGS FOR RECTANGLE RECT-22 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-24 IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
/*  def var char-val as cha no-undo.                                                    */
/*                                                                                      */
/*  case focus:name:                                                                    */
/*    when 'vend-no' or when 'vend2-no' then do:                                        */
/*      APPLY 'entry' TO FOCUS.                                                         */
/*      run windows/l-vendno.w (gcompany, "", focus:screen-value, output char-val).     */
/*      if char-val <> "" then focus:screen-value = entry(1,char-val).                  */
/*                                                                                      */
/*    end.                                                                              */
/*    when "pur-uom" then do:                                                           */
/*      run windows/l-stduom.w (gcompany,uom-list, focus:screen-value, output char-val).*/
/*      if char-val <> "" then focus:screen-value = caps(entry(1,char-val)).            */
/*    end.                                                                              */
/*  end case.                                                                           */
/*                                                                                      */
/*  RETURN NO-APPLY.                                                                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAddLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddLoc V-table-Win
ON CHOOSE OF btAddLoc IN FRAME F-Main /* + */
DO:
  DEF VAR C-Win AS HANDLE.
  C-Win = CURRENT-WINDOW.
  SESSION:SUPPRESS-WARNINGS = TRUE.
  C-Win:SHOW-IN-TASKBAR=FALSE.
  C-Win:SENSITIVE = FALSE.

  v-whseadded = NO.
  IF AVAIL itemfg THEN
    RUN windows/addfgloc.w (INPUT ROWID(itemfg), OUTPUT v-whseadded).
  IF v-whseadded THEN
      RUN reset-cbloc.
  C-Win:SHOW-IN-TASKBAR=TRUE.
  C-Win:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_all V-table-Win
ON CHOOSE OF btn_all IN FRAME F-Main /* Alloc to Orders */
DO:
  IF itemfg.q-alloc NE 0 THEN RUN oe/w-inqord.w PERSISTENT SET h_w-inqord (ROWID(itemfg), YES).
  IF VALID-HANDLE(h_w-inqord) THEN
    RUN adm-initialize IN h_w-inqord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_onh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_onh V-table-Win
ON CHOOSE OF btn_onh IN FRAME F-Main /* On Hand */
DO:
  IF itemfg.q-onh NE 0 THEN
  RUN fg/w-inqonh.w (ROWID(itemfg), NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ono
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ono V-table-Win
ON CHOOSE OF btn_ono IN FRAME F-Main /* Job/PO On Ord */
DO:
  IF itemfg.q-ono NE 0 THEN DO:
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ itemfg.company
          AND job-hdr.i-no    EQ itemfg.i-no
          AND job-hdr.opened  EQ YES
          AND CAN-FIND(FIRST job WHERE job.company EQ job-hdr.company
                                   AND job.job     EQ job-hdr.job
                                   AND job.job-no  EQ job-hdr.job-no
                                   AND job.job-no2 EQ job-hdr.job-no2)
        NO-LOCK NO-ERROR.
    IF AVAIL job-hdr THEN 
        RUN jc/w-inqjob.w (ROWID(itemfg), YES).
    ELSE DO:
        FIND FIRST fg-set WHERE fg-set.company EQ itemfg.company
                            AND fg-set.part-no EQ itemfg.i-no
                          NO-LOCK NO-ERROR.
        IF AVAIL fg-set THEN
        RUN jc/w-inqjbc.w (ROWID(itemfg), YES).
    END.

    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ itemfg.company
          AND po-ordl.i-no      EQ itemfg.i-no
          AND po-ordl.item-type EQ NO
          AND po-ordl.opened    EQ YES
          AND CAN-FIND(FIRST po-ord WHERE po-ord.company EQ po-ordl.company
                                      AND po-ord.po-no   EQ po-ordl.po-no)
        NO-LOCK NO-ERROR.
    IF AVAIL po-ordl THEN
    RUN po/w-inqpo.w (ROWID(itemfg), YES).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLoc V-table-Win
ON VALUE-CHANGED OF cbLoc IN FRAME F-Main /* Warehouse */
DO:
  ASSIGN cbLoc.
  RUN local-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dq-alloc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dq-alloc V-table-Win
ON VALUE-CHANGED OF dq-alloc IN FRAME F-Main
DO:
  RUN calc-q-avail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dq-onh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dq-onh V-table-Win
ON VALUE-CHANGED OF dq-onh IN FRAME F-Main
DO:
  RUN calc-q-avail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dq-ono
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dq-ono V-table-Win
ON VALUE-CHANGED OF dq-ono IN FRAME F-Main
DO:
  RUN calc-q-avail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}
{custom/getloc.i}

assign
 cocode = gcompany
 locode = gloc.

RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).

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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-q-avail V-table-Win 
PROCEDURE calc-q-avail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  DO WITH FRAME {&FRAME-NAME}:
    itemfg.q-avail:SCREEN-VALUE = STRING(DEC(itemfg.q-onh:SCREEN-VALUE) +
                                         DEC(itemfg.q-ono:SCREEN-VALUE) -
                                         DEC(itemfg.q-alloc:SCREEN-VALUE),
                                         itemfg.q-avail:FORMAT).
  END.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty V-table-Win 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.

  IF AVAIL itemfg THEN
    run fg/d-reqtys.w (ROWID(itemfg), yes).

  run get-link-handle in adm-broker-hdl (this-procedure, "record-source", output char-hdl).

  run repo-query in widget-handle(char-hdl) (ROWID(itemfg)).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-eb FOR eb.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF cbLoc NE "ALL" THEN DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
        AND itemfg-loc.i-no EQ itemfg.i-no
        AND itemfg-loc.loc  EQ cbLoc EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL itemfg-loc THEN 
        ASSIGN
        itemfg-loc.q-alloc = INTEGER(dq-alloc:SCREEN-VALUE)
        itemfg-loc.q-avail = INTEGER(dq-avail:SCREEN-VALUE)
        itemfg-loc.q-back  = INTEGER(dq-back:SCREEN-VALUE)
        itemfg-loc.q-onh   = INTEGER(dq-onh:SCREEN-VALUE)
        itemfg-loc.q-ono   = INTEGER(dq-ono:SCREEN-VALUE) 
        .
    RELEASE itemfg-loc.
    RETURN.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      
      itemfg.q-onh = int(dq-onh:SCREEN-VALUE) .
  END.

 /*Task# 04121312*/
 FIND FIRST fg-set WHERE fg-set.company = itemfg.company 
     AND fg-set.set-no = itemfg.i-no NO-LOCK NO-ERROR.

 IF AVAIL itemfg AND AVAIL fg-set THEN
   FOR EACH eb NO-LOCK      
       WHERE eb.company EQ itemfg.company
         AND eb.cust-no EQ itemfg.cust-no
         AND eb.stock-no EQ itemfg.i-no:

     FIND bf-eb WHERE ROWID(bf-eb) EQ ROWID(eb) EXCLUSIVE NO-WAIT NO-ERROR.
     IF AVAIL bf-eb THEN DO:
       ASSIGN bf-eb.pur-man = itemfg.pur-man.
     END.
   END. /* each eb */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&list-5}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-itemfg FOR itemfg.

  DEF VAR v-alloc-save AS DEC NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  def var v-q-alloc like itemfg.q-alloc NO-UNDO.
  def var v-q-back  like itemfg.q-back NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL itemfg THEN DO WITH TRANSACTION:
    {sys/inc/oereordr.i}

    FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    IF AVAIL b-itemfg THEN
    b-itemfg.q-avail = b-itemfg.q-onh +
                       (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE b-itemfg.q-ono) -
                       b-itemfg.q-alloc.
    RELEASE b-itemfg.
    FIND CURRENT itemfg NO-LOCK.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF AVAIL itemfg THEN DO:
      IF v-prior-i-no EQ "" OR v-prior-i-no NE itemfg.i-no THEN DO WITH FRAME {&FRAME-NAME}:
          /* Empty the selection-list or combo-box */
          ASSIGN
              cbLoc:LIST-ITEM-PAIRS = ?
              cbLoc:SCREEN-VALUE = "":U
              .
          cbLoc:ADD-LAST("ALL", "ALL"). 
          IF AVAIL itemfg THEN DO:    
              FOR EACH itemfg-loc NO-LOCK
                  WHERE itemfg-loc.company EQ itemfg.company
                    AND itemfg-loc.i-no    EQ itemfg.i-no,
                  FIRST loc NO-LOCK
                  WHERE loc.company EQ itemfg-loc.company
                    AND loc.loc     EQ itemfg-loc.loc
                  :
                  cbLoc:ADD-LAST(itemfg-loc.loc + " " + replace(loc.dscr, ",", " "), itemfg-loc.loc).            
              END.
          END.
          ASSIGN
              cbLoc:SCREEN-VALUE = "ALL"
              cbLoc = "ALL"
              .
      END.
      v-prior-i-no = itemfg.i-no.

      RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT v-q-alloc, OUTPUT v-q-back). 
      ASSIGN
          dq-back:SCREEN-VALUE  = STRING(v-q-back)
          dq-alloc:SCREEN-VALUE = STRING(v-q-alloc)
/*          itemfg.q-alloc        = v-q-alloc*/
/*          itemfg.q-back         = v-q-back */
          dq-ono:SCREEN-VALUE   = STRING(itemfg.q-ono)
          dq-onh:SCREEN-VALUE   = STRING(itemfg.q-onh)
          dq-avail:SCREEN-VALUE = STRING(itemfg.q-avail)
/*          .                                                        */
/*      IF dq-alloc:SCREEN-VALUE EQ "0" OR                           */
/*         dq-alloc:SCREEN-VALUE EQ "" THEN DO:                      */
/*         v-alloc-save = INTEGER(dq-alloc:SCREEN-VALUE).            */
/*         IF v-alloc-save NE INTEGER(dq-alloc:SCREEN-VALUE) THEN DO:*/
/*             ASSIGN*/
                 dq-avail:SCREEN-VALUE = STRING(DEC(itemfg.q-onh)
                                       + DEC(itemfg.q-ono)
                                       - DEC(itemfg.q-alloc),
                                         dq-avail:FORMAT)
/*                 itemfg.q-avail = DEC(itemfg.q-onh)  */
/*                                + DEC(itemfg.q-ono)  */
/*                                - DEC(itemfg.q-alloc)*/
                                .
/*         END.*/
/*      END.*/
    
      IF cbLoc NE "ALL" AND AVAIL itemfg THEN DO:
        FIND FIRST itemfg-loc NO-LOCK
             WHERE itemfg-loc.company EQ itemfg.company
               AND itemfg-loc.i-no    EQ itemfg.i-no
               AND itemfg-loc.loc     EQ cbLoc
             NO-ERROR.
        IF AVAIL itemfg-loc THEN do:
            RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT v-q-alloc, OUTPUT v-q-back).    
            ASSIGN
                dq-alloc:SCREEN-VALUE = STRING(v-q-alloc) /*STRING(get-alloc())*/
                dq-back:SCREEN-VALUE = STRING(v-q-back) /*STRING(itemfg-loc.q-back)*/
/*                itemfg.q-alloc = v-q-alloc /*STRING(get-alloc())*/*/
/*                itemfg.q-back = v-q-back /*STRING(itemfg-loc.q-back)*/*/
                dq-onh:SCREEN-VALUE = STRING(itemfg-loc.q-onh)
/*                itemfg.q-onh = itemfg-loc.q-onh*/
                dq-ono:SCREEN-VALUE = STRING(itemfg-loc.q-ono)
/*                itemfg.q-ono = itemfg-loc.q-ono*/
                dq-avail:SCREEN-VALUE = STRING(DEC(itemfg.q-onh)
                                      + DEC(itemfg.q-ono)
                                      - DEC(itemfg.q-alloc),
                                        dq-avail:FORMAT)
/*                itemfg.q-avail = DEC(itemfg.q-onh)  */
/*                               + DEC(itemfg.q-ono)  */
/*                               - DEC(itemfg.q-alloc)*/
                               .
        END.
      END.
  END. /* avail itemfg */

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit V-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(h_w-inqord) THEN
    DELETE OBJECT h_w-inqord.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide V-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages as log.
    RUN dispatch IN THIS-PROCEDURE (IF vlChangePages THEN
                                      'update-record':U
                                    ELSE
                                      'cancel-record':U).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-return AS LOG NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:

      /* Empty the selection-list or combo-box */
      cbLoc:LIST-ITEM-PAIRS = ?.
      cbLoc:SCREEN-VALUE = "":U.
      v-return = cbLoc:ADD-LAST("ALL", "ALL"). 
      IF AVAIL itemfg THEN DO:
        FOR EACH itemfg-loc 
          WHERE itemfg-loc.company EQ itemfg.i-no
            AND itemfg-loc.i-no EQ itemfg.i-no
          NO-LOCK:
          v-return = cbLoc:ADD-LAST(itemfg-loc.loc, itemfg-loc.loc).            
        END.
      END.
      ASSIGN 
          cbLoc:SCREEN-VALUE = "ALL"
          cbLoc = "ALL"
          .
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE override-qty V-table-Win 
PROCEDURE override-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "tableio-source", OUTPUT char-hdl).

  RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("").

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.

    ENABLE {&list-5}.

    ENABLE {&list-buttons}.

    APPLY "entry" TO itemfg.beg-bal.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-cbloc V-table-Win 
PROCEDURE reset-cbloc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-return AS LOG NO-UNDO.
  IF AVAIL itemfg THEN DO WITH FRAME {&FRAME-NAME}:

      /* Empty the selection-list or combo-box */
      cbLoc:LIST-ITEM-PAIRS = ?.
      cbLoc:SCREEN-VALUE = "":U.
      v-return = cbLoc:ADD-LAST("ALL", "ALL"). 
      FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
           AND itemfg-loc.i-no EQ itemfg.i-no
          NO-LOCK:
          v-return = cbLoc:ADD-LAST(itemfg-loc.loc, itemfg-loc.loc).            
      END.

      cbLoc:SCREEN-VALUE = "ALL".
      cbLoc = "ALL".

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
  {src/adm/template/snd-list.i "itemfg"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-alloc V-table-Win 
FUNCTION get-alloc RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF BUFFER b-itemfg FOR itemfg.
  DEF BUFFER b2-itemfg FOR itemfg.
  DEF BUFFER b-itemfg-loc FOR itemfg-loc.
  DEF BUFFER b2-itemfg-loc FOR itemfg-loc.

  DEF VAR lv-q-all AS DECIMAL NO-UNDO.

  ASSIGN
   lv-q-all = 0.

  IF NOT AVAIL itemfg THEN
      RETURN 0.

  FIND FIRST b-itemfg
      WHERE ROWID(b-itemfg) = ROWID(itemfg)
      NO-LOCK NO-ERROR.
  FIND FIRST fg-set WHERE fg-set.company = itemfg.company
                      AND fg-set.part-no = itemfg.i-no
                    NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-set THEN
    FIND FIRST fg-set WHERE fg-set.company = itemfg.company
                        AND fg-set.set-no = itemfg.i-no
                      NO-LOCK NO-ERROR.
  /* If this is not set-related, then just return */
  IF NOT AVAIL fg-set THEN
      /* return 0*/ lv-q-all = 0.
  FIND FIRST b-itemfg-loc 
    WHERE b-itemfg-loc.company EQ itemfg.company
      AND b-itemfg-loc.i-no EQ itemfg.i-no
      AND b-itemfg-loc.loc  EQ cbLoc
    NO-LOCK NO-ERROR.
  IF cbLoc EQ "ALL" AND AVAIL b-itemfg THEN
     ASSIGN lv-q-all = b-itemfg.q-alloc.
    ELSE IF cbLoc NE "ALL" AND AVAIL b-itemfg-loc THEN
      lv-q-all = b-itemfg-loc.q-alloc.

    IF AVAIL b-itemfg AND b-itemfg.isaset = NO      
       AND lv-q-all = 0 THEN DO:

    IF cbLoc EQ "ALL" AND AVAIL(fg-set) THEN DO:
          FIND FIRST b2-itemfg
              WHERE b2-itemfg.company EQ fg-set.company
                AND b2-itemfg.i-no    EQ fg-set.set-no
                AND b2-itemfg.isaset  EQ YES
              NO-LOCK NO-ERROR.

          IF AVAIL b2-itemfg THEN DO:
            FOR EACH oe-ordl WHERE oe-ordl.company = fg-set.company 
                               AND oe-ordl.i-no = b2-itemfg.i-no
                             NO-LOCK,
              EACH oe-rel WHERE oe-rel.company = oe-ordl.company
                      AND oe-rel.ord-no = oe-ordl.ord-no
                      AND oe-rel.i-no  = oe-ordl.i-no
                    NO-LOCK.
              lv-q-all = lv-q-all + (b2-itemfg.q-alloc * fg-set.QtyPerSet).
              LEAVE. /* q-alloc contains value for all orders */
            END.
          END.
    END.
    ELSE IF avail(fg-set) THEN  DO:
        FIND FIRST b2-itemfg-loc 
          WHERE b2-itemfg-loc.company EQ fg-set.company
            AND b2-itemfg-loc.i-no EQ fg-set.set-no
            /* AND b2-itemfg.isaset EQ YES */
            AND b2-itemfg-loc.loc  EQ cbLoc 
          NO-LOCK NO-ERROR.

          IF AVAIL b2-itemfg-loc THEN DO:


            /* check of oe-rel seems to be here to confirm rel qty is real */
            FOR EACH oe-ordl WHERE oe-ordl.company = fg-set.company 
                               AND oe-ordl.i-no = fg-set.set-no
                             NO-LOCK,
              EACH oe-rel WHERE oe-rel.company = oe-ordl.company
                      AND oe-rel.ord-no = oe-ordl.ord-no
                      AND oe-rel.i-no  = oe-ordl.i-no
                      AND oe-rel.spare-char-1 EQ cbLoc
                    NO-LOCK.
              lv-q-all = lv-q-all + (b2-itemfg-loc.q-alloc * fg-set.QtyPerSet).
              LEAVE. /* q-alloc contains value for all orders */
            END. /* Each Ordl */
          END. /* avail b2-itemfg-loc */

    END. /* ... else if avail(fg-set) */

  END. /* if cbloc EQ ALL */

  RETURN lv-q-all.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

