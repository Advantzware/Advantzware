&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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
&Scoped-define EXTERNAL-TABLES itemfg-loc
&Scoped-define FIRST-EXTERNAL-TABLE itemfg-loc


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg-loc.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Definitions for FRAME F-Main                                         */
&Scoped-define FIELDS-IN-QUERY-F-Main itemfg.cust-no itemfg.i-name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F-Main itemfg.cust-no itemfg.i-name 
&Scoped-define ENABLED-TABLES-IN-QUERY-F-Main itemfg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F-Main itemfg
&Scoped-define QUERY-STRING-F-Main FOR EACH itemfg WHERE itemfg.company = itemfg-loc.company ~
  AND itemfg.i-no = itemfg-loc.i-no ~
      AND itemfg.company = itemfg-loc.company ~
and itemfg.i-no = itemfg-loc.i-no NO-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH itemfg WHERE itemfg.company = itemfg-loc.company ~
  AND itemfg.i-no = itemfg-loc.i-no ~
      AND itemfg.company = itemfg-loc.company ~
and itemfg.i-no = itemfg-loc.i-no NO-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main itemfg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.cust-no itemfg-loc.i-no itemfg.i-name 
&Scoped-define ENABLED-TABLES itemfg itemfg-loc
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-define SECOND-ENABLED-TABLE itemfg-loc
&Scoped-Define ENABLED-OBJECTS RECT-4 
&Scoped-Define DISPLAYED-FIELDS itemfg.cust-no itemfg-loc.i-no ~
itemfg.i-name itemfg-loc.q-onh itemfg-loc.q-ono itemfg-loc.q-alloc ~
itemfg-loc.q-back itemfg-loc.q-avail itemfg-loc.ord-level ~
itemfg-loc.ord-min itemfg-loc.ord-max 
&Scoped-define DISPLAYED-TABLES itemfg itemfg-loc
&Scoped-define FIRST-DISPLAYED-TABLE itemfg
&Scoped-define SECOND-DISPLAYED-TABLE itemfg-loc
&Scoped-Define DISPLAYED-OBJECTS cust_name 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-alloc V-table-Win 
FUNCTION get-alloc RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE cust_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118.4 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-Main FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     itemfg.cust-no AT ROW 1.24 COL 19 COLON-ALIGNED
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust_name AT ROW 1.24 COL 37 COLON-ALIGNED NO-LABEL
     itemfg-loc.i-no AT ROW 2.19 COL 19 COLON-ALIGNED
          LABEL "Item No"
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     itemfg.i-name AT ROW 2.19 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     itemfg-loc.q-onh AT ROW 4.33 COL 3 COLON-ALIGNED NO-LABEL FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     itemfg-loc.q-ono AT ROW 4.33 COL 18 COLON-ALIGNED NO-LABEL FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     itemfg-loc.q-alloc AT ROW 4.33 COL 33 COLON-ALIGNED NO-LABEL FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     itemfg-loc.q-back AT ROW 4.33 COL 48 COLON-ALIGNED NO-LABEL FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     itemfg-loc.q-avail AT ROW 4.33 COL 63 COLON-ALIGNED NO-LABEL FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     itemfg-loc.ord-level AT ROW 4.33 COL 78 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     itemfg-loc.ord-min AT ROW 4.33 COL 90 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     itemfg-loc.ord-max AT ROW 4.33 COL 103 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     "ReordMin" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.62 COL 92
     "Jobs/POs" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.62 COL 21
     "Backorder" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 3.62 COL 50
     "Allocated" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 3.62 COL 36
     "ReordMax" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.62 COL 105
     "On Hand" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.62 COL 7
     "ReordLev" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 3.62 COL 80
     "Available" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.62 COL 66
     RECT-4 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.itemfg-loc
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
         HEIGHT             = 6.86
         WIDTH              = 118.4.
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
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:SENSITIVE        = FALSE.

/* SETTINGS FOR FILL-IN itemfg.cust-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg-loc.i-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg-loc.ord-level IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg-loc.ord-max IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg-loc.ord-min IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg-loc.q-alloc IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg-loc.q-avail IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg-loc.q-back IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg-loc.q-onh IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg-loc.q-ono IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "ASI.itemfg WHERE ASI.itemfg-loc <external> ..."
     _Options          = "NO-LOCK"
     _JoinCode[1]      = "ASI.itemfg.company = ASI.itemfg-loc.company
  AND ASI.itemfg.i-no = ASI.itemfg-loc.i-no"
     _Where[1]         = "ASI.itemfg.company = itemfg-loc.company
and itemfg.i-no = itemfg-loc.i-no"
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
  {src/adm/template/row-list.i "itemfg-loc"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg-loc"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME f-main:
      itemfg-loc.q-alloc:SCREEN-VALUE = STRING(get-alloc()).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query V-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
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
  {src/adm/template/snd-list.i "itemfg-loc"}
  {src/adm/template/snd-list.i "itemfg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-pass-loc V-table-Win 
PROCEDURE set-pass-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipc-loc AS CHAR NO-UNDO.
RUN local-display-fields.
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
FUNCTION get-alloc RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF BUFFER b-itemfg FOR itemfg.
  DEF BUFFER b2-itemfg FOR itemfg.
  DEF BUFFER b-itemfg-loc FOR itemfg-loc.
  DEF BUFFER b2-itemfg-loc FOR itemfg-loc.
  /* To make the procedure the same as that in fgijob.w */
  DEF VAR lc-pass-loc AS CHAR INIT "ALL".
  DEF VAR lv-q-all AS INT NO-UNDO.

  ASSIGN
   lv-q-all = 0.

  IF NOT AVAIL itemfg THEN
      RETURN 0.
  IF NOT AVAIL itemfg-loc THEN
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
  /* wfk - don't know why this is returning if not a set, it was */
  /*       causing the values to appear as a 0                   */
/*   IF NOT AVAIL fg-set THEN */
/*       RETURN 0.            */

  FIND FIRST b-itemfg-loc 
    WHERE b-itemfg-loc.company EQ itemfg.company
      AND b-itemfg-loc.i-no EQ itemfg.i-no
      AND b-itemfg-loc.loc  EQ itemfg-loc.loc
    NO-LOCK NO-ERROR.
  IF lc-pass-loc EQ "ALL" AND AVAIL b-itemfg THEN
     ASSIGN lv-q-all = b-itemfg.q-alloc.
    ELSE IF lc-pass-loc NE "ALL" AND AVAIL b-itemfg-loc THEN
      lv-q-all = b-itemfg-loc.q-alloc.
  IF AVAIL b-itemfg AND b-itemfg.isaset = NO      
       AND lv-q-all = 0 THEN DO:

    IF lc-pass-loc EQ "ALL" AND AVAIL(fg-set) THEN DO:
          FIND FIRST b2-itemfg
              WHERE b2-itemfg.company EQ fg-set.company
                AND b2-itemfg.i-no    EQ fg-set.set-no
               /* AND b2-itemfg.isaset  EQ YES */
              NO-LOCK NO-ERROR.
    
          IF AVAIL b2-itemfg THEN DO:
            FOR EACH oe-ordl WHERE oe-ordl.company = fg-set.company 
                               AND oe-ordl.i-no = b2-itemfg.i-no
                             NO-LOCK,
              EACH oe-rel WHERE oe-rel.company = oe-ordl.company
                      AND oe-rel.ord-no = oe-ordl.ord-no
                      AND oe-rel.i-no  = oe-ordl.i-no
                    NO-LOCK.
              lv-q-all = lv-q-all + (b2-itemfg.q-alloc * fg-set.part-qty).
              LEAVE. /* q-alloc contains value for all orders */
            END.
          END.
    END.
    ELSE IF AVAIL(fg-set) THEN DO:
        FIND FIRST b2-itemfg-loc 
          WHERE b2-itemfg-loc.company EQ fg-set.company
            AND b2-itemfg-loc.i-no EQ fg-set.set-no
            /* AND b2-itemfg.isaset EQ YES */
            AND b2-itemfg-loc.loc  EQ lc-pass-loc 
          NO-LOCK NO-ERROR.
          
          IF AVAIL b2-itemfg-loc THEN DO:
          

            /* check of oe-rel seems to be here to confirm rel qty is real */
            FOR EACH oe-ordl WHERE oe-ordl.company = fg-set.company 
                               AND oe-ordl.i-no = fg-set.set-no
                             NO-LOCK,
              EACH oe-rel WHERE oe-rel.company = oe-ordl.company
                      AND oe-rel.ord-no = oe-ordl.ord-no
                      AND oe-rel.i-no  = oe-ordl.i-no
                      AND oe-rel.spare-char-1 EQ lc-pass-loc
                    NO-LOCK.
              lv-q-all = lv-q-all + (b2-itemfg-loc.q-alloc * fg-set.part-qty).
              LEAVE. /* q-alloc contains value for all orders */
            END. /* Each Ordl */
          END. /* avail b2-itemfg-loc */
    END. /* ... else */      
  END. /* avail b-itemfg */

  RETURN lv-q-all.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

