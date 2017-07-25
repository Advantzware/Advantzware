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

  File: windows\oe-edit1.w

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ord.ord-no oe-ord.est-type oe-ord.est-no ~
oe-ord.job-no oe-ord.job-no2 oe-ord.ord-date oe-ord.stat oe-ord.cust-no ~
oe-ord.sold-id oe-ord.sman[1] oe-ord.s-pct[1] oe-ord.s-comm[1] ~
oe-ord.sman[2] oe-ord.s-pct[2] oe-ord.s-comm[2] oe-ord.sman[3] ~
oe-ord.s-pct[3] oe-ord.s-comm[3] oe-ord.po-no oe-ord.prod-date ~
oe-ord.tax-gr oe-ord.contact oe-ord.carrier oe-ord.cc-type oe-ord.pord-no ~
oe-ord.fob-code oe-ord.cc-name oe-ord.last-date oe-ord.frt-pay ~
oe-ord.cc-num oe-ord.due-code oe-ord.due-date oe-ord.over-pct ~
oe-ord.cc-expiration oe-ord.terms oe-ord.under-pct oe-ord.cc-auth 
&Scoped-define ENABLED-TABLES oe-ord
&Scoped-define FIRST-ENABLED-TABLE oe-ord
&Scoped-Define DISPLAYED-FIELDS oe-ord.ord-no oe-ord.est-type oe-ord.est-no ~
oe-ord.job-no oe-ord.job-no2 oe-ord.ord-date oe-ord.stat oe-ord.cust-no ~
oe-ord.sold-id oe-ord.cust-name oe-ord.sold-name oe-ord.addr[1] ~
oe-ord.sold-addr[1] oe-ord.addr[2] oe-ord.sold-addr[2] oe-ord.city ~
oe-ord.state oe-ord.zip oe-ord.sold-city oe-ord.sold-state oe-ord.sold-zip ~
oe-ord.sman[1] oe-ord.sname[1] oe-ord.s-pct[1] oe-ord.s-comm[1] ~
oe-ord.sman[2] oe-ord.sname[2] oe-ord.s-pct[2] oe-ord.s-comm[2] ~
oe-ord.sman[3] oe-ord.sname[3] oe-ord.s-pct[3] oe-ord.s-comm[3] ~
oe-ord.po-no oe-ord.prod-date oe-ord.tax-gr oe-ord.contact oe-ord.carrier ~
oe-ord.cc-type oe-ord.pord-no oe-ord.fob-code oe-ord.cc-name ~
oe-ord.last-date oe-ord.frt-pay oe-ord.cc-num oe-ord.due-code ~
oe-ord.due-date oe-ord.over-pct oe-ord.cc-expiration oe-ord.terms ~
oe-ord.under-pct oe-ord.cc-auth 
&Scoped-define DISPLAYED-TABLES oe-ord
&Scoped-define FIRST-DISPLAYED-TABLE oe-ord


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

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ord.ord-no AT ROW 1.48 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.est-type AT ROW 1.48 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     oe-ord.est-no AT ROW 1.48 COL 56 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.job-no AT ROW 1.48 COL 85 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-ord.job-no2 AT ROW 1.48 COL 98 COLON-ALIGNED
          LABEL "-"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     oe-ord.ord-date AT ROW 1.48 COL 117 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-ord.stat AT ROW 1.48 COL 147 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-ord.cust-no AT ROW 4.57 COL 25 COLON-ALIGNED
          LABEL "Bill To:"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-ord.sold-id AT ROW 4.57 COL 100 COLON-ALIGNED
          LABEL "Sold To:"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-ord.cust-name AT ROW 5.76 COL 25 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ord.sold-name AT ROW 5.76 COL 100 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ord.addr[1] AT ROW 6.95 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ord.sold-addr[1] AT ROW 6.95 COL 100 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ord.addr[2] AT ROW 8.14 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ord.sold-addr[2] AT ROW 8.14 COL 100 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     oe-ord.city AT ROW 9.33 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.state AT ROW 9.33 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     oe-ord.zip AT ROW 9.33 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     oe-ord.sold-city AT ROW 9.33 COL 100 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.sold-state AT ROW 9.33 COL 118 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     oe-ord.sold-zip AT ROW 9.33 COL 124 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-ord.sman[1] AT ROW 12.43 COL 26 COLON-ALIGNED
          LABEL "SalesRep"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[1] AT ROW 12.43 COL 55 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     oe-ord.s-pct[1] AT ROW 12.43 COL 99 COLON-ALIGNED
          LABEL "% of Sales"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[1] AT ROW 12.43 COL 122 COLON-ALIGNED
          LABEL "Comm. %"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.sman[2] AT ROW 13.62 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[2] AT ROW 13.62 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     oe-ord.s-pct[2] AT ROW 13.62 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ord.s-comm[2] AT ROW 13.62 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.sman[3] AT ROW 14.81 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[3] AT ROW 14.81 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     oe-ord.s-pct[3] AT ROW 14.81 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[3] AT ROW 14.81 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.po-no AT ROW 17.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.prod-date AT ROW 17.19 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-ord.tax-gr AT ROW 17.19 COL 116 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.contact AT ROW 18.38 COL 19 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     oe-ord.carrier AT ROW 18.38 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     oe-ord.cc-type AT ROW 18.38 COL 116 COLON-ALIGNED
          LABEL "Payment Type"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-ord.pord-no AT ROW 19.57 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.fob-code AT ROW 19.57 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     oe-ord.cc-name AT ROW 19.57 COL 116 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     oe-ord.last-date AT ROW 20.76 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-ord.frt-pay AT ROW 20.76 COL 73 COLON-ALIGNED
          LABEL "Frt Pay"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-ord.cc-num AT ROW 20.76 COL 116 COLON-ALIGNED
          LABEL "Account Number"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     oe-ord.due-code AT ROW 21.95 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     oe-ord.due-date AT ROW 21.95 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-ord.over-pct AT ROW 21.95 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     oe-ord.cc-expiration AT ROW 21.95 COL 116 COLON-ALIGNED
          LABEL "Expiration"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ord.terms AT ROW 23.14 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     oe-ord.under-pct AT ROW 23.14 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     oe-ord.cc-auth AT ROW 23.14 COL 116 COLON-ALIGNED
          LABEL "Ref #"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 25.76
         WIDTH              = 156.2.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-ord.addr[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.addr[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-auth IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-expiration IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-num IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-type IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.contact IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cust-name IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.cust-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-ord.frt-pay IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.job-no2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.s-comm[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.s-pct[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sman[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sname[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.sname[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sname[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-addr[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.sold-addr[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ord.tax-gr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.zip IN FRAME F-Main
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

