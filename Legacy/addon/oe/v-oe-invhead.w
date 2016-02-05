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
&Scoped-define EXTERNAL-TABLES inv-head shipto
&Scoped-define FIRST-EXTERNAL-TABLE inv-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-head, shipto.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS inv-head.inv-no inv-head.inv-date ~
inv-head.bol-no inv-head.printed inv-head.stat inv-head.bill-to ~
shipto.ship-no inv-head.cust-name shipto.ship-name inv-head.addr[1] ~
shipto.ship-addr[1] inv-head.addr[2] shipto.ship-addr[2] inv-head.city ~
inv-head.state inv-head.zip shipto.ship-city shipto.ship-state ~
shipto.ship-zip inv-head.tax-gr inv-head.terms inv-head.carrier ~
inv-head.fob-code inv-head.frt-pay inv-head.t-inv-weight inv-head.t-inv-tax ~
inv-head.t-inv-freight inv-head.f-bill inv-head.tot-ord inv-head.t-comm ~
inv-head.t-inv-cost 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}inv-no ~{&FP2}inv-no ~{&FP3}~
 ~{&FP1}inv-date ~{&FP2}inv-date ~{&FP3}~
 ~{&FP1}bol-no ~{&FP2}bol-no ~{&FP3}~
 ~{&FP1}printed ~{&FP2}printed ~{&FP3}~
 ~{&FP1}stat ~{&FP2}stat ~{&FP3}~
 ~{&FP1}bill-to ~{&FP2}bill-to ~{&FP3}~
 ~{&FP1}ship-no ~{&FP2}ship-no ~{&FP3}~
 ~{&FP1}cust-name ~{&FP2}cust-name ~{&FP3}~
 ~{&FP1}ship-name ~{&FP2}ship-name ~{&FP3}~
 ~{&FP1}addr[1] ~{&FP2}addr[1] ~{&FP3}~
 ~{&FP1}ship-addr[1] ~{&FP2}ship-addr[1] ~{&FP3}~
 ~{&FP1}addr[2] ~{&FP2}addr[2] ~{&FP3}~
 ~{&FP1}ship-addr[2] ~{&FP2}ship-addr[2] ~{&FP3}~
 ~{&FP1}city ~{&FP2}city ~{&FP3}~
 ~{&FP1}state ~{&FP2}state ~{&FP3}~
 ~{&FP1}zip ~{&FP2}zip ~{&FP3}~
 ~{&FP1}ship-city ~{&FP2}ship-city ~{&FP3}~
 ~{&FP1}ship-state ~{&FP2}ship-state ~{&FP3}~
 ~{&FP1}ship-zip ~{&FP2}ship-zip ~{&FP3}~
 ~{&FP1}tax-gr ~{&FP2}tax-gr ~{&FP3}~
 ~{&FP1}terms ~{&FP2}terms ~{&FP3}~
 ~{&FP1}carrier ~{&FP2}carrier ~{&FP3}~
 ~{&FP1}fob-code ~{&FP2}fob-code ~{&FP3}~
 ~{&FP1}frt-pay ~{&FP2}frt-pay ~{&FP3}~
 ~{&FP1}t-inv-weight ~{&FP2}t-inv-weight ~{&FP3}~
 ~{&FP1}t-inv-tax ~{&FP2}t-inv-tax ~{&FP3}~
 ~{&FP1}t-inv-freight ~{&FP2}t-inv-freight ~{&FP3}~
 ~{&FP1}f-bill ~{&FP2}f-bill ~{&FP3}~
 ~{&FP1}tot-ord ~{&FP2}tot-ord ~{&FP3}~
 ~{&FP1}t-comm ~{&FP2}t-comm ~{&FP3}~
 ~{&FP1}t-inv-cost ~{&FP2}t-inv-cost ~{&FP3}
&Scoped-define ENABLED-TABLES inv-head shipto
&Scoped-define FIRST-ENABLED-TABLE inv-head
&Scoped-define SECOND-ENABLED-TABLE shipto
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS inv-head.inv-no inv-head.inv-date ~
inv-head.bol-no inv-head.printed inv-head.stat inv-head.bill-to ~
shipto.ship-no inv-head.cust-name shipto.ship-name inv-head.addr[1] ~
shipto.ship-addr[1] inv-head.addr[2] shipto.ship-addr[2] inv-head.city ~
inv-head.state inv-head.zip shipto.ship-city shipto.ship-state ~
shipto.ship-zip inv-head.tax-gr inv-head.terms inv-head.carrier ~
inv-head.fob-code inv-head.frt-pay inv-head.t-inv-weight inv-head.t-inv-tax ~
inv-head.t-inv-freight inv-head.f-bill inv-head.tot-ord inv-head.t-comm ~
inv-head.t-inv-cost 

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
     inv-head.inv-no AT ROW 1.48 COL 13 COLON-ALIGNED
          LABEL "Invoice #"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     inv-head.inv-date AT ROW 1.48 COL 51 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     inv-head.bol-no AT ROW 1.48 COL 92 COLON-ALIGNED
          LABEL "BOL #"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     inv-head.printed AT ROW 1.48 COL 133 COLON-ALIGNED
          LABEL "Printed"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     inv-head.stat AT ROW 2.67 COL 133 COLON-ALIGNED
          LABEL "Status"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     inv-head.bill-to AT ROW 3.14 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     shipto.ship-no AT ROW 3.14 COL 79 COLON-ALIGNED
          LABEL "Ship To"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     inv-head.cust-name AT ROW 4.33 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     shipto.ship-name AT ROW 4.33 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-head.addr[1] AT ROW 5.52 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     shipto.ship-addr[1] AT ROW 5.52 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-head.addr[2] AT ROW 6.71 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     shipto.ship-addr[2] AT ROW 6.71 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-head.city AT ROW 7.91 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-head.state AT ROW 7.91 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     inv-head.zip AT ROW 7.91 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     shipto.ship-city AT ROW 7.91 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     shipto.ship-state AT ROW 7.91 COL 100 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     shipto.ship-zip AT ROW 7.91 COL 105 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     inv-head.tax-gr AT ROW 11 COL 14 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     inv-head.terms AT ROW 11 COL 46 COLON-ALIGNED
          LABEL "Payment Terms"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     inv-head.carrier AT ROW 11 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     inv-head.fob-code AT ROW 11 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     inv-head.frt-pay AT ROW 11 COL 132 COLON-ALIGNED
          LABEL "Freight Terms"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     inv-head.t-inv-weight AT ROW 14.33 COL 17 COLON-ALIGNED
          LABEL "Weight"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-head.t-inv-tax AT ROW 14.33 COL 117 COLON-ALIGNED
          LABEL "Tax"
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     inv-head.t-inv-freight AT ROW 15.52 COL 17 COLON-ALIGNED
          LABEL "Freight"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     inv-head.f-bill AT ROW 15.52 COL 41 COLON-ALIGNED
          LABEL "Bill?"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     inv-head.tot-ord AT ROW 15.52 COL 111 COLON-ALIGNED
          LABEL "Invoice Total"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     inv-head.t-comm AT ROW 16.71 COL 17 COLON-ALIGNED
          LABEL "Commissions"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-head.t-inv-cost AT ROW 16.71 COL 111 COLON-ALIGNED
          LABEL "Item Cost"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RECT-1 AT ROW 1 COL 1
     "======================================" VIEW-AS TEXT
          SIZE 54 BY .95 AT ROW 12.43 COL 90
     " I N V O I C E    T O T A L S" VIEW-AS TEXT
          SIZE 34 BY 1.19 AT ROW 12.43 COL 55
     "======================================" VIEW-AS TEXT
          SIZE 52 BY .95 AT ROW 12.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.inv-head,ASI.shipto
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
         HEIGHT             = 17.24
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

/* SETTINGS FOR FILL-IN inv-head.bol-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.f-bill IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.frt-pay IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.inv-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.inv-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.printed IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.ship-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.stat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-comm IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-freight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-tax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-weight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.tax-gr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.terms IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.tot-ord IN FRAME F-Main
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

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartViewerCues" V-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartViewer,uib,49270
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
  {src/adm/template/row-list.i "inv-head"}
  {src/adm/template/row-list.i "shipto"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-head"}
  {src/adm/template/row-find.i "shipto"}

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
  {src/adm/template/snd-list.i "inv-head"}
  {src/adm/template/snd-list.i "shipto"}

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


