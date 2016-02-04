&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers\oe-ldetail.w

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
&Scoped-define EXTERNAL-TABLES oe-ordl oe-ord itemfg
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl, oe-ord, itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.est-no oe-ordl.i-no oe-ordl.i-name ~
oe-ordl.qty oe-ordl.part-dscr2 oe-ordl.price oe-ordl.part-dscr1 ~
oe-ordl.cas-cnt oe-ordl.po-no oe-ordl.tax oe-ordl.disc oe-ordl.req-code ~
oe-ordl.req-date oe-ordl.prom-code oe-ordl.prom-date oe-ordl.po-no-po ~
oe-ordl.vend-no oe-ordl.s-man[1] oe-ordl.s-pct[1] oe-ordl.s-man[2] ~
oe-ordl.s-pct[2] oe-ordl.s-man[3] oe-ordl.s-pct[3] oe-ord.ord-no ~
oe-ord.type itemfg.q-ono 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}est-no ~{&FP2}est-no ~{&FP3}~
 ~{&FP1}i-no ~{&FP2}i-no ~{&FP3}~
 ~{&FP1}i-name ~{&FP2}i-name ~{&FP3}~
 ~{&FP1}qty ~{&FP2}qty ~{&FP3}~
 ~{&FP1}part-dscr2 ~{&FP2}part-dscr2 ~{&FP3}~
 ~{&FP1}price ~{&FP2}price ~{&FP3}~
 ~{&FP1}part-dscr1 ~{&FP2}part-dscr1 ~{&FP3}~
 ~{&FP1}cas-cnt ~{&FP2}cas-cnt ~{&FP3}~
 ~{&FP1}po-no ~{&FP2}po-no ~{&FP3}~
 ~{&FP1}tax ~{&FP2}tax ~{&FP3}~
 ~{&FP1}disc ~{&FP2}disc ~{&FP3}~
 ~{&FP1}req-code ~{&FP2}req-code ~{&FP3}~
 ~{&FP1}req-date ~{&FP2}req-date ~{&FP3}~
 ~{&FP1}prom-code ~{&FP2}prom-code ~{&FP3}~
 ~{&FP1}prom-date ~{&FP2}prom-date ~{&FP3}~
 ~{&FP1}po-no-po ~{&FP2}po-no-po ~{&FP3}~
 ~{&FP1}vend-no ~{&FP2}vend-no ~{&FP3}~
 ~{&FP1}s-man[1] ~{&FP2}s-man[1] ~{&FP3}~
 ~{&FP1}s-pct[1] ~{&FP2}s-pct[1] ~{&FP3}~
 ~{&FP1}s-man[2] ~{&FP2}s-man[2] ~{&FP3}~
 ~{&FP1}s-pct[2] ~{&FP2}s-pct[2] ~{&FP3}~
 ~{&FP1}s-man[3] ~{&FP2}s-man[3] ~{&FP3}~
 ~{&FP1}s-pct[3] ~{&FP2}s-pct[3] ~{&FP3}~
 ~{&FP1}ord-no ~{&FP2}ord-no ~{&FP3}~
 ~{&FP1}type ~{&FP2}type ~{&FP3}~
 ~{&FP1}q-ono ~{&FP2}q-ono ~{&FP3}
&Scoped-define ENABLED-TABLES oe-ordl oe-ord itemfg
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-define SECOND-ENABLED-TABLE oe-ord
&Scoped-define THIRD-ENABLED-TABLE itemfg
&Scoped-Define ENABLED-OBJECTS RECT-2 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.est-no oe-ordl.i-no oe-ordl.i-name ~
oe-ordl.qty oe-ordl.part-dscr2 oe-ordl.price oe-ordl.part-dscr1 ~
oe-ordl.cas-cnt oe-ordl.po-no oe-ordl.tax oe-ordl.disc oe-ordl.req-code ~
oe-ordl.req-date oe-ordl.prom-code oe-ordl.prom-date oe-ordl.po-no-po ~
oe-ordl.vend-no oe-ordl.s-man[1] oe-ordl.s-pct[1] oe-ordl.s-comm[1] ~
oe-ordl.s-man[2] oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.s-man[3] ~
oe-ordl.s-pct[3] oe-ordl.s-comm[3] oe-ord.ord-no oe-ord.type oe-ord.e-num ~
oe-ord.job-no oe-ord.job-no2 oe-ord.ord-date oe-ord.stat itemfg.i-no ~
itemfg.q-onh itemfg.q-ono itemfg.q-alloc itemfg.q-back itemfg.q-avail ~
itemfg.ord-level oe-ordl.part-no oe-ordl.cost oe-ordl.t-cost oe-ordl.job-no ~
oe-ordl.job-no2 

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
DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 130 BY 14.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ordl.est-no AT ROW 7.91 COL 16 COLON-ALIGNED
          LABEL "Est. #"
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     oe-ordl.i-no AT ROW 9.1 COL 16 COLON-ALIGNED
          LABEL "Item #"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ordl.i-name AT ROW 9.1 COL 79 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ordl.qty AT ROW 10.29 COL 16 COLON-ALIGNED
          LABEL "Qty"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     oe-ordl.part-dscr2 AT ROW 10.29 COL 79 COLON-ALIGNED
          LABEL "Item Description"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ordl.price AT ROW 11.48 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     oe-ordl.part-dscr1 AT ROW 11.48 COL 79 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ordl.cas-cnt AT ROW 12.67 COL 47 COLON-ALIGNED
          LABEL "Count"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-ordl.po-no AT ROW 12.67 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ordl.tax AT ROW 12.67 COL 106 COLON-ALIGNED
          LABEL "Taxable"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-ordl.disc AT ROW 13.86 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ordl.req-code AT ROW 13.86 COL 79 COLON-ALIGNED
          LABEL "Requested"
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     oe-ordl.req-date AT ROW 13.86 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-ordl.prom-code AT ROW 15.05 COL 79 COLON-ALIGNED
          LABEL "Promised"
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     oe-ordl.prom-date AT ROW 15.05 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-ordl.po-no-po AT ROW 16.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.vend-no AT ROW 16.24 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-ordl.s-man[1] AT ROW 18.62 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.6 BY 1
     oe-ordl.s-pct[1] AT ROW 18.62 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-comm[1] AT ROW 18.62 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-man[2] AT ROW 19.81 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.6 BY 1
     oe-ordl.s-pct[2] AT ROW 19.81 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-comm[2] AT ROW 19.81 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-man[3] AT ROW 21 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.6 BY 1
     oe-ordl.s-pct[3] AT ROW 21 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-comm[3] AT ROW 21 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.ord-no AT ROW 1.95 COL 9 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 10.4 BY .62
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ord.type AT ROW 1.95 COL 28 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 4.2 BY .62
     oe-ord.e-num AT ROW 1.95 COL 42 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 10.4 BY .62
     oe-ord.job-no AT ROW 1.95 COL 62 COLON-ALIGNED
          LABEL "Job"
           VIEW-AS TEXT 
          SIZE 11.6 BY .62
     oe-ord.job-no2 AT ROW 1.95 COL 76 COLON-ALIGNED
          LABEL "-"
           VIEW-AS TEXT 
          SIZE 4.8 BY .62
     oe-ord.ord-date AT ROW 1.95 COL 96 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 13.2 BY .62
     oe-ord.stat AT ROW 1.95 COL 123 COLON-ALIGNED
          LABEL "Status"
           VIEW-AS TEXT 
          SIZE 4.2 BY .62
     itemfg.i-no AT ROW 3.14 COL 9 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 17 BY .62
     itemfg.q-onh AT ROW 6.24 COL 4 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 15 BY .62
     itemfg.q-ono AT ROW 6.24 COL 26 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 15 BY .62
     itemfg.q-alloc AT ROW 6.24 COL 48 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 15 BY .62
     itemfg.q-back AT ROW 6.24 COL 70 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 15 BY .62
     itemfg.q-avail AT ROW 6.24 COL 92 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 15 BY .62
     itemfg.ord-level AT ROW 6.24 COL 114 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 15 BY .62
     oe-ordl.part-no AT ROW 7.91 COL 79 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 17 BY .62
     oe-ordl.cost AT ROW 12.67 COL 16 COLON-ALIGNED
          LABEL "Cost/M"
           VIEW-AS TEXT 
          SIZE 23 BY .62
     oe-ordl.t-cost AT ROW 15.05 COL 16 COLON-ALIGNED
          LABEL "Ext. Price"
           VIEW-AS TEXT 
          SIZE 21.6 BY .62
     oe-ordl.job-no AT ROW 16.24 COL 79 COLON-ALIGNED
          LABEL "Job #"
           VIEW-AS TEXT 
          SIZE 11.6 BY .62
     oe-ordl.job-no2 AT ROW 16.24 COL 92 COLON-ALIGNED
          LABEL "-"
           VIEW-AS TEXT 
          SIZE 4.8 BY .62
     "Level" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 5.05 COL 116
     "Reorder" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 4.33 COL 116
     "Available" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 5.05 COL 94
     "Total" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.33 COL 94
     "Backorder" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 5.05 COL 72
     "Total" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.33 COL 72
     "To Orders" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 5.05 COL 50
     "Allocated" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 4.33 COL 50
     "On Hand" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 5.05 COL 7
     "On Order" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 5.05 COL 28
     "Jobs/PO" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 4.33 COL 28
     "Total" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 4.33 COL 7
     "Comm. %" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 17.67 COL 41
     "SalesRep" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 17.67 COL 21 RIGHT-ALIGNED
     "% of Sales" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 17.67 COL 25
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-2 AT ROW 7.67 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ordl,ASI.oe-ord,ASI.itemfg
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
         HEIGHT             = 22.52
         WIDTH              = 135.
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

/* SETTINGS FOR FILL-IN oe-ordl.cas-cnt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.cost IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.e-num IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.est-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.i-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.i-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.job-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.job-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.job-no2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ordl.job-no2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.ord-date IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.ord-level IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.prom-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.q-alloc IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN itemfg.q-avail IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.q-back IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.q-onh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.req-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.stat IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ordl.t-cost IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ordl.tax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TEXT-LITERAL "SalesRep"
          SIZE 10 BY .62 AT ROW 17.67 COL 21 RIGHT-ALIGNED              */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartViewerCues" V-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartViewer,uib,49270
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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
  {src/adm/template/row-list.i "oe-ord"}
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}
  {src/adm/template/row-find.i "oe-ord"}
  {src/adm/template/row-find.i "itemfg"}

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
  {src/adm/template/snd-list.i "oe-ord"}
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


