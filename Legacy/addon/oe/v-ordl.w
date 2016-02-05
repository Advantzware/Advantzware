&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.ord-no oe-ordl.est-type ~
oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date oe-ordl.stat ~
oe-ordl.i-no oe-ordl.part-no oe-ordl.i-name oe-ordl.part-dscr1 ~
oe-ordl.i-dscr oe-ordl.part-dscr2 oe-ordl.qty oe-ordl.po-no oe-ordl.tax ~
oe-ordl.price oe-ordl.pr-uom oe-ordl.cost oe-ordl.disc oe-ordl.req-code ~
oe-ordl.req-date oe-ordl.prom-code oe-ordl.prom-date oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.po-no-po oe-ordl.s-man[2] ~
oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.vend-no oe-ordl.s-man[3] ~
oe-ordl.s-pct[3] oe-ordl.s-comm[3] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}ord-no ~{&FP2}ord-no ~{&FP3}~
 ~{&FP1}est-type ~{&FP2}est-type ~{&FP3}~
 ~{&FP1}est-no ~{&FP2}est-no ~{&FP3}~
 ~{&FP1}job-no ~{&FP2}job-no ~{&FP3}~
 ~{&FP1}job-no2 ~{&FP2}job-no2 ~{&FP3}~
 ~{&FP1}ord-date ~{&FP2}ord-date ~{&FP3}~
 ~{&FP1}stat ~{&FP2}stat ~{&FP3}~
 ~{&FP1}i-no ~{&FP2}i-no ~{&FP3}~
 ~{&FP1}part-no ~{&FP2}part-no ~{&FP3}~
 ~{&FP1}i-name ~{&FP2}i-name ~{&FP3}~
 ~{&FP1}part-dscr1 ~{&FP2}part-dscr1 ~{&FP3}~
 ~{&FP1}i-dscr ~{&FP2}i-dscr ~{&FP3}~
 ~{&FP1}part-dscr2 ~{&FP2}part-dscr2 ~{&FP3}~
 ~{&FP1}qty ~{&FP2}qty ~{&FP3}~
 ~{&FP1}po-no ~{&FP2}po-no ~{&FP3}~
 ~{&FP1}tax ~{&FP2}tax ~{&FP3}~
 ~{&FP1}price ~{&FP2}price ~{&FP3}~
 ~{&FP1}pr-uom ~{&FP2}pr-uom ~{&FP3}~
 ~{&FP1}cost ~{&FP2}cost ~{&FP3}~
 ~{&FP1}disc ~{&FP2}disc ~{&FP3}~
 ~{&FP1}req-code ~{&FP2}req-code ~{&FP3}~
 ~{&FP1}req-date ~{&FP2}req-date ~{&FP3}~
 ~{&FP1}prom-code ~{&FP2}prom-code ~{&FP3}~
 ~{&FP1}prom-date ~{&FP2}prom-date ~{&FP3}~
 ~{&FP1}s-man[1] ~{&FP2}s-man[1] ~{&FP3}~
 ~{&FP1}s-pct[1] ~{&FP2}s-pct[1] ~{&FP3}~
 ~{&FP1}s-comm[1] ~{&FP2}s-comm[1] ~{&FP3}~
 ~{&FP1}po-no-po ~{&FP2}po-no-po ~{&FP3}~
 ~{&FP1}s-man[2] ~{&FP2}s-man[2] ~{&FP3}~
 ~{&FP1}s-pct[2] ~{&FP2}s-pct[2] ~{&FP3}~
 ~{&FP1}s-comm[2] ~{&FP2}s-comm[2] ~{&FP3}~
 ~{&FP1}vend-no ~{&FP2}vend-no ~{&FP3}~
 ~{&FP1}s-man[3] ~{&FP2}s-man[3] ~{&FP3}~
 ~{&FP1}s-pct[3] ~{&FP2}s-pct[3] ~{&FP3}~
 ~{&FP1}s-comm[3] ~{&FP2}s-comm[3] ~{&FP3}
&Scoped-define ENABLED-TABLES oe-ordl oe-ord
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-define SECOND-ENABLED-TABLE oe-ord
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.ord-no oe-ordl.est-type ~
oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date oe-ordl.stat ~
oe-ordl.i-no oe-ordl.part-no oe-ordl.i-name oe-ordl.part-dscr1 ~
oe-ordl.i-dscr oe-ordl.part-dscr2 oe-ordl.qty oe-ordl.po-no oe-ordl.tax ~
oe-ordl.price oe-ordl.pr-uom oe-ordl.cost oe-ordl.disc oe-ordl.req-code ~
oe-ordl.req-date oe-ordl.prom-code oe-ordl.prom-date oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.po-no-po oe-ordl.s-man[2] ~
oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.vend-no oe-ordl.s-man[3] ~
oe-ordl.s-pct[3] oe-ordl.s-comm[3] 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 17.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ordl.ord-no AT ROW 1.95 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.est-type AT ROW 1.95 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ordl.est-no AT ROW 1.95 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.job-no AT ROW 1.95 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.job-no2 AT ROW 1.95 COL 96 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-ord.ord-date AT ROW 1.95 COL 109 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-ordl.stat AT ROW 1.95 COL 138 COLON-ALIGNED
          LABEL "Line Status"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ordl.i-no AT ROW 3.86 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-ordl.part-no AT ROW 4.81 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-ordl.i-name AT ROW 5.05 COL 15 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-ordl.part-dscr1 AT ROW 6 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-ordl.i-dscr AT ROW 6.24 COL 15 COLON-ALIGNED
          LABEL "Item Dscr"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-ordl.part-dscr2 AT ROW 7.19 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-ordl.qty AT ROW 7.43 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     oe-ordl.po-no AT ROW 8.38 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-ordl.tax AT ROW 8.38 COL 121 COLON-ALIGNED
          LABEL "Taxable"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ordl.price AT ROW 8.62 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     oe-ordl.pr-uom AT ROW 8.62 COL 48 COLON-ALIGNED
          LABEL "UOM"
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     oe-ordl.cost AT ROW 9.81 COL 15 COLON-ALIGNED
          LABEL "Cost/M"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-ordl.disc AT ROW 9.81 COL 45 COLON-ALIGNED
          LABEL "Disc"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     oe-ordl.req-code AT ROW 10.52 COL 86 COLON-ALIGNED
          LABEL "Requested"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     oe-ordl.req-date AT ROW 10.52 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ordl.prom-code AT ROW 11.71 COL 86 COLON-ALIGNED
          LABEL "Promised"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     oe-ordl.prom-date AT ROW 11.71 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ordl.s-man[1] AT ROW 13.86 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.s-pct[1] AT ROW 13.86 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-ordl.s-comm[1] AT ROW 13.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.po-no-po AT ROW 13.86 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ordl.s-man[2] AT ROW 15.05 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.s-pct[2] AT ROW 15.05 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-ordl.s-comm[2] AT ROW 15.05 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.vend-no AT ROW 15.05 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-ordl.s-man[3] AT ROW 16.24 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.s-pct[3] AT ROW 16.24 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-ordl.s-comm[3] AT ROW 16.24 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     "SalesRep" VIEW-AS TEXT
          SIZE 11 BY .71 AT ROW 13.14 COL 7
     "-" VIEW-AS TEXT
          SIZE 2 BY .95 AT ROW 1.95 COL 96
     RECT-1 AT ROW 1 COL 1
     "Comm.%" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 13.14 COL 40
     "% of Sales" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 13.14 COL 21
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-ordl.cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.disc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.i-dscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.pr-uom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.prom-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.req-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.stat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.tax IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
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


