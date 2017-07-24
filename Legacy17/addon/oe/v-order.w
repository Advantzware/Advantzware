&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

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
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ord.ord-no oe-ord.type oe-ord.e-num ~
oe-ord.job-no oe-ord.job-no2 oe-ord.ord-date oe-ord.stat oe-ord.bill-to ~
oe-ord.sold-no oe-ord.sman[1] oe-ord.sname[1] oe-ord.s-pct[1] ~
oe-ord.sman[2] oe-ord.sname[2] oe-ord.s-pct[2] oe-ord.sman[3] ~
oe-ord.sname[3] oe-ord.s-pct[3] oe-ord.po-no oe-ord.prod-date ~
oe-ord.contact oe-ord.over-pct oe-ord.under-pct oe-ord.pord-no ~
oe-ord.tax-gr oe-ord.due-code oe-ord.due-date oe-ord.cc-type ~
oe-ord.cc-expiration oe-ord.last-date oe-ord.cc-num oe-ord.terms ~
oe-ord.cc-auth oe-ord.cust-name oe-ord.sold-name oe-ord.addr[1] ~
oe-ord.sold-addr[1] oe-ord.addr[2] oe-ord.sold-addr[2] oe-ord.city ~
oe-ord.state oe-ord.zip oe-ord.sold-city oe-ord.sold-state oe-ord.sold-zip ~
oe-ord.s-comm[1] oe-ord.s-comm[2] oe-ord.s-comm[3] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}ord-no ~{&FP2}ord-no ~{&FP3}~
 ~{&FP1}type ~{&FP2}type ~{&FP3}~
 ~{&FP1}e-num ~{&FP2}e-num ~{&FP3}~
 ~{&FP1}job-no ~{&FP2}job-no ~{&FP3}~
 ~{&FP1}job-no2 ~{&FP2}job-no2 ~{&FP3}~
 ~{&FP1}ord-date ~{&FP2}ord-date ~{&FP3}~
 ~{&FP1}stat ~{&FP2}stat ~{&FP3}~
 ~{&FP1}bill-to ~{&FP2}bill-to ~{&FP3}~
 ~{&FP1}sold-no ~{&FP2}sold-no ~{&FP3}~
 ~{&FP1}sman[1] ~{&FP2}sman[1] ~{&FP3}~
 ~{&FP1}sname[1] ~{&FP2}sname[1] ~{&FP3}~
 ~{&FP1}s-pct[1] ~{&FP2}s-pct[1] ~{&FP3}~
 ~{&FP1}sman[2] ~{&FP2}sman[2] ~{&FP3}~
 ~{&FP1}sname[2] ~{&FP2}sname[2] ~{&FP3}~
 ~{&FP1}s-pct[2] ~{&FP2}s-pct[2] ~{&FP3}~
 ~{&FP1}sman[3] ~{&FP2}sman[3] ~{&FP3}~
 ~{&FP1}sname[3] ~{&FP2}sname[3] ~{&FP3}~
 ~{&FP1}s-pct[3] ~{&FP2}s-pct[3] ~{&FP3}~
 ~{&FP1}po-no ~{&FP2}po-no ~{&FP3}~
 ~{&FP1}prod-date ~{&FP2}prod-date ~{&FP3}~
 ~{&FP1}contact ~{&FP2}contact ~{&FP3}~
 ~{&FP1}over-pct ~{&FP2}over-pct ~{&FP3}~
 ~{&FP1}under-pct ~{&FP2}under-pct ~{&FP3}~
 ~{&FP1}pord-no ~{&FP2}pord-no ~{&FP3}~
 ~{&FP1}tax-gr ~{&FP2}tax-gr ~{&FP3}~
 ~{&FP1}due-code ~{&FP2}due-code ~{&FP3}~
 ~{&FP1}due-date ~{&FP2}due-date ~{&FP3}~
 ~{&FP1}cc-type ~{&FP2}cc-type ~{&FP3}~
 ~{&FP1}cc-expiration ~{&FP2}cc-expiration ~{&FP3}~
 ~{&FP1}last-date ~{&FP2}last-date ~{&FP3}~
 ~{&FP1}cc-num ~{&FP2}cc-num ~{&FP3}~
 ~{&FP1}terms ~{&FP2}terms ~{&FP3}~
 ~{&FP1}cc-auth ~{&FP2}cc-auth ~{&FP3}~
 ~{&FP1}cust-name ~{&FP2}cust-name ~{&FP3}~
 ~{&FP1}sold-name ~{&FP2}sold-name ~{&FP3}~
 ~{&FP1}addr[1] ~{&FP2}addr[1] ~{&FP3}~
 ~{&FP1}sold-addr[1] ~{&FP2}sold-addr[1] ~{&FP3}~
 ~{&FP1}addr[2] ~{&FP2}addr[2] ~{&FP3}~
 ~{&FP1}sold-addr[2] ~{&FP2}sold-addr[2] ~{&FP3}~
 ~{&FP1}city ~{&FP2}city ~{&FP3}~
 ~{&FP1}state ~{&FP2}state ~{&FP3}~
 ~{&FP1}zip ~{&FP2}zip ~{&FP3}~
 ~{&FP1}sold-city ~{&FP2}sold-city ~{&FP3}~
 ~{&FP1}sold-state ~{&FP2}sold-state ~{&FP3}~
 ~{&FP1}sold-zip ~{&FP2}sold-zip ~{&FP3}~
 ~{&FP1}s-comm[1] ~{&FP2}s-comm[1] ~{&FP3}~
 ~{&FP1}s-comm[2] ~{&FP2}s-comm[2] ~{&FP3}~
 ~{&FP1}s-comm[3] ~{&FP2}s-comm[3] ~{&FP3}
&Scoped-define ENABLED-TABLES oe-ord
&Scoped-define FIRST-ENABLED-TABLE oe-ord
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS oe-ord.ord-no oe-ord.type oe-ord.e-num ~
oe-ord.job-no oe-ord.job-no2 oe-ord.ord-date oe-ord.stat oe-ord.bill-to ~
oe-ord.sold-no oe-ord.sman[1] oe-ord.sname[1] oe-ord.s-pct[1] ~
oe-ord.sman[2] oe-ord.sname[2] oe-ord.s-pct[2] oe-ord.sman[3] ~
oe-ord.sname[3] oe-ord.s-pct[3] oe-ord.po-no oe-ord.prod-date ~
oe-ord.contact oe-ord.over-pct oe-ord.under-pct oe-ord.pord-no ~
oe-ord.tax-gr oe-ord.due-code oe-ord.due-date oe-ord.cc-type ~
oe-ord.cc-expiration oe-ord.last-date oe-ord.cc-num oe-ord.terms ~
oe-ord.cc-auth oe-ord.cust-name oe-ord.sold-name oe-ord.addr[1] ~
oe-ord.sold-addr[1] oe-ord.addr[2] oe-ord.sold-addr[2] oe-ord.city ~
oe-ord.state oe-ord.zip oe-ord.sold-city oe-ord.sold-state oe-ord.sold-zip ~
oe-ord.s-comm[1] oe-ord.s-comm[2] oe-ord.s-comm[3] 

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
     oe-ord.ord-no AT ROW 1.24 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ord.type AT ROW 1.24 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ord.e-num AT ROW 1.24 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ord.job-no AT ROW 1.24 COL 72 COLON-ALIGNED
          LABEL "Job #"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ord.job-no2 AT ROW 1.24 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-ord.ord-date AT ROW 1.24 COL 107 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-ord.stat AT ROW 1.24 COL 136 COLON-ALIGNED
          LABEL "Status"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ord.bill-to AT ROW 3.38 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-ord.sold-no AT ROW 3.38 COL 89 COLON-ALIGNED
          LABEL "Sold to"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ord.sman[1] AT ROW 8.62 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     oe-ord.sname[1] AT ROW 8.62 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     oe-ord.s-pct[1] AT ROW 8.62 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ord.sman[2] AT ROW 9.57 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     oe-ord.sname[2] AT ROW 9.57 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     oe-ord.s-pct[2] AT ROW 9.57 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ord.sman[3] AT ROW 10.52 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     oe-ord.sname[3] AT ROW 10.52 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     oe-ord.s-pct[3] AT ROW 10.52 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ord.po-no AT ROW 11.95 COL 19 COLON-ALIGNED
          LABEL "Cust PO#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-ord.prod-date AT ROW 11.95 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.contact AT ROW 12.91 COL 19 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ord.over-pct AT ROW 12.91 COL 95 COLON-ALIGNED
          LABEL "Over"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.under-pct AT ROW 12.91 COL 120 COLON-ALIGNED
          LABEL "Under"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.pord-no AT ROW 13.86 COL 19 COLON-ALIGNED
          LABEL "Prev Ord#"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ord.tax-gr AT ROW 13.86 COL 95 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     oe-ord.due-code AT ROW 14.81 COL 19 COLON-ALIGNED
          LABEL "Due Date"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     oe-ord.due-date AT ROW 14.81 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ord.cc-type AT ROW 14.81 COL 95 COLON-ALIGNED
          LABEL "Payment Type"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ord.cc-expiration AT ROW 14.81 COL 120 COLON-ALIGNED
          LABEL "Expire"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.last-date AT ROW 15.76 COL 19 COLON-ALIGNED
          LABEL "Last Ship"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-ord.cc-num AT ROW 15.76 COL 95 COLON-ALIGNED
          LABEL "Account #"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     oe-ord.terms AT ROW 16.71 COL 19 COLON-ALIGNED
          LABEL "Pay Terms"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ord.cc-auth AT ROW 16.71 COL 95 COLON-ALIGNED
          LABEL "Ref #"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-ord.cust-name AT ROW 4.33 COL 9 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 38 BY .62
     oe-ord.sold-name AT ROW 4.33 COL 89 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 38 BY .62
     oe-ord.addr[1] AT ROW 5.05 COL 9 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 38 BY .62
     oe-ord.sold-addr[1] AT ROW 5.05 COL 89 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 38 BY .62
     oe-ord.addr[2] AT ROW 5.76 COL 9 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 38 BY .62
     oe-ord.sold-addr[2] AT ROW 5.76 COL 89 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 38 BY .62
     oe-ord.city AT ROW 6.48 COL 9 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 20 BY .62
     oe-ord.state AT ROW 6.48 COL 29 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 4.4 BY .62
     oe-ord.zip AT ROW 6.48 COL 34 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 14 BY .62
     oe-ord.sold-city AT ROW 6.48 COL 89 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 20 BY .62
     oe-ord.sold-state AT ROW 6.48 COL 109 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 4.4 BY .62
     oe-ord.sold-zip AT ROW 6.48 COL 113 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 14 BY .62
     oe-ord.s-comm[1] AT ROW 8.62 COL 104 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 9.2 BY .62
     oe-ord.s-comm[2] AT ROW 9.57 COL 104 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 9.2 BY .62
     oe-ord.s-comm[3] AT ROW 10.52 COL 104 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 9.2 BY .62
     "% of Sales" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 7.91 COL 81
     "Comm. %" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 7.91 COL 106
     RECT-1 AT ROW 1 COL 1
     "Name" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 7.91 COL 27
     "SalesRep" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 7.91 COL 11
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ord
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

/* SETTINGS FOR FILL-IN oe-ord.cc-auth IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-expiration IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-num IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-type IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.contact IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.due-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.job-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.job-no2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.last-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.over-pct IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.po-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.pord-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.stat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.tax-gr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.terms IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.under-pct IN FRAME F-Main
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
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ord"}

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
  {src/adm/template/snd-list.i "oe-ord"}

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


