&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
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
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES vend-whse-trans-hist
&Scoped-define FIRST-EXTERNAL-TABLE vend-whse-trans-hist


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR vend-whse-trans-hist.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS vend-whse-trans-hist.r-no ~
vend-whse-trans-hist.create-date vend-whse-trans-hist.trans-type ~
vend-whse-trans-hist.create-userid vend-whse-trans-hist.trans-date ~
vend-whse-trans-hist.trans-qty vend-whse-trans-hist.item-po-no ~
vend-whse-trans-hist.item-line-no vend-whse-trans-hist.cust-part-no ~
vend-whse-trans-hist.fg-item-no vend-whse-trans-hist.vendor-code ~
vend-whse-trans-hist.vendor-plant-code ~
vend-whse-trans-hist.vendor-dept-code vend-whse-trans-hist.vend-ord-no ~
vend-whse-trans-hist.vend-job-no vend-whse-trans-hist.vend-job-no2 ~
vend-whse-trans-hist.sell-price vend-whse-trans-hist.plant-tot-oh-qty 
&Scoped-define DISPLAYED-TABLES vend-whse-trans-hist
&Scoped-define FIRST-DISPLAYED-TABLE vend-whse-trans-hist


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
     vend-whse-trans-hist.r-no AT ROW 1.19 COL 31 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     vend-whse-trans-hist.create-date AT ROW 1.19 COL 88 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Created On" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.trans-type AT ROW 2.19 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 28
          LABEL "Transaction Type" FORMAT "x(1)"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     vend-whse-trans-hist.create-userid AT ROW 2.19 COL 88 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "Created By" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.trans-date AT ROW 3.19 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 24
          LABEL "Transaction Date" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.trans-qty AT ROW 4.19 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 26
          LABEL "Transaction Quantity" FORMAT "->>,>>>,>>9.9<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.item-po-no AT ROW 5.19 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 16
          LABEL "Customers PO#" FORMAT "x(9)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.item-line-no AT ROW 6.24 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 14
          LABEL "Customers PO Line#" FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     vend-whse-trans-hist.cust-part-no AT ROW 7.19 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 8
          LABEL "Customers Part#" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.fg-item-no AT ROW 8.19 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 12
          LABEL "Suppliers FG Item" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.vendor-code AT ROW 9.19 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 40
          LABEL "Customers A/P Code" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.vendor-plant-code AT ROW 10.29 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 44
          LABEL "Customers Plant ID" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.vendor-dept-code AT ROW 11.33 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 42
          LABEL "Customers Dept Code" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.vend-ord-no AT ROW 12.38 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 38
          LABEL "Suppliers Order#" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     vend-whse-trans-hist.vend-job-no AT ROW 13.52 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 34
          LABEL "Suppliers Job#" FORMAT "x(9)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     vend-whse-trans-hist.vend-job-no2 AT ROW 13.52 COL 46 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 5.4 BY 1
     vend-whse-trans-hist.sell-price AT ROW 14.62 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 22
          LABEL "Suppliers Item Sell Price" FORMAT ">,>>>,>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     vend-whse-trans-hist.plant-tot-oh-qty AT ROW 15.71 COL 31 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: vend-whse-trans-hist
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
         HEIGHT             = 17.14
         WIDTH              = 144.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN vend-whse-trans-hist.create-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.create-userid IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.cust-part-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.fg-item-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.item-line-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.item-po-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.plant-tot-oh-qty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.r-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.sell-price IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.trans-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.trans-qty IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.trans-type IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.vend-job-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.vend-job-no2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.vend-ord-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.vendor-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.vendor-dept-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-whse-trans-hist.vendor-plant-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
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
  {src/adm/template/row-list.i "vend-whse-trans-hist"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "vend-whse-trans-hist"}

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
  {src/adm/template/snd-list.i "vend-whse-trans-hist"}

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

