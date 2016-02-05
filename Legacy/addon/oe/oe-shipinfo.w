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
&Scoped-define EXTERNAL-TABLES oe-ordl oe-ord oe-rel oe-relh
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl, oe-ord, oe-rel, oe-relh.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.ord-no oe-ordl.est-type ~
oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date oe-ordl.stat ~
oe-ordl.i-no oe-rel.ship-no oe-rel.ship-id oe-relh.rel-date ~
oe-rel.ship-addr[1] oe-ordl.req-code oe-ordl.req-date oe-rel.ship-city ~
oe-rel.ship-state oe-rel.ship-zip oe-ordl.prom-code oe-ordl.prom-date ~
oe-ordl.qty oe-ordl.rel-qty oe-ord.frt-pay oe-ord.fob-code oe-ordl.ship-qty ~
oe-ord.carrier oe-ordl.qty-on-bo oe-ordl.t-weight oe-ordl.rel-stat ~
oe-ordl.t-freight oe-ord.f-bill 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}ord-no ~{&FP2}ord-no ~{&FP3}~
 ~{&FP1}est-type ~{&FP2}est-type ~{&FP3}~
 ~{&FP1}est-no ~{&FP2}est-no ~{&FP3}~
 ~{&FP1}job-no ~{&FP2}job-no ~{&FP3}~
 ~{&FP1}job-no2 ~{&FP2}job-no2 ~{&FP3}~
 ~{&FP1}ord-date ~{&FP2}ord-date ~{&FP3}~
 ~{&FP1}stat ~{&FP2}stat ~{&FP3}~
 ~{&FP1}i-no ~{&FP2}i-no ~{&FP3}~
 ~{&FP1}ship-no ~{&FP2}ship-no ~{&FP3}~
 ~{&FP1}ship-id ~{&FP2}ship-id ~{&FP3}~
 ~{&FP1}rel-date ~{&FP2}rel-date ~{&FP3}~
 ~{&FP1}ship-addr[1] ~{&FP2}ship-addr[1] ~{&FP3}~
 ~{&FP1}req-code ~{&FP2}req-code ~{&FP3}~
 ~{&FP1}req-date ~{&FP2}req-date ~{&FP3}~
 ~{&FP1}ship-city ~{&FP2}ship-city ~{&FP3}~
 ~{&FP1}ship-state ~{&FP2}ship-state ~{&FP3}~
 ~{&FP1}ship-zip ~{&FP2}ship-zip ~{&FP3}~
 ~{&FP1}prom-code ~{&FP2}prom-code ~{&FP3}~
 ~{&FP1}prom-date ~{&FP2}prom-date ~{&FP3}~
 ~{&FP1}qty ~{&FP2}qty ~{&FP3}~
 ~{&FP1}rel-qty ~{&FP2}rel-qty ~{&FP3}~
 ~{&FP1}frt-pay ~{&FP2}frt-pay ~{&FP3}~
 ~{&FP1}fob-code ~{&FP2}fob-code ~{&FP3}~
 ~{&FP1}ship-qty ~{&FP2}ship-qty ~{&FP3}~
 ~{&FP1}carrier ~{&FP2}carrier ~{&FP3}~
 ~{&FP1}qty-on-bo ~{&FP2}qty-on-bo ~{&FP3}~
 ~{&FP1}t-weight ~{&FP2}t-weight ~{&FP3}~
 ~{&FP1}rel-stat ~{&FP2}rel-stat ~{&FP3}~
 ~{&FP1}t-freight ~{&FP2}t-freight ~{&FP3}~
 ~{&FP1}f-bill ~{&FP2}f-bill ~{&FP3}
&Scoped-define ENABLED-TABLES oe-ordl oe-ord oe-rel oe-relh
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-define SECOND-ENABLED-TABLE oe-ord
&Scoped-define THIRD-ENABLED-TABLE oe-rel
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-1 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.ord-no oe-ordl.est-type ~
oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date oe-ordl.stat ~
oe-ordl.i-no oe-rel.ship-no oe-rel.ship-id oe-relh.rel-date ~
oe-rel.ship-addr[1] oe-ordl.req-code oe-ordl.req-date oe-rel.ship-city ~
oe-rel.ship-state oe-rel.ship-zip oe-ordl.prom-code oe-ordl.prom-date ~
oe-ordl.qty oe-ordl.rel-qty oe-ord.frt-pay oe-ord.fob-code oe-ordl.ship-qty ~
oe-ord.carrier oe-ordl.qty-on-bo oe-ordl.t-weight oe-ordl.rel-stat ~
oe-ordl.t-freight oe-ord.f-bill 

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

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 142 BY 11.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ordl.ord-no AT ROW 1.48 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.est-type AT ROW 1.48 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ordl.est-no AT ROW 1.48 COL 60 COLON-ALIGNED
          LABEL "Est. #"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.job-no AT ROW 1.48 COL 82 COLON-ALIGNED
          LABEL "Job #"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.job-no2 AT ROW 1.48 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-ord.ord-date AT ROW 1.48 COL 107 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-ordl.stat AT ROW 1.48 COL 136 COLON-ALIGNED
          LABEL "Status"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ordl.i-no AT ROW 2.67 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-rel.ship-no AT ROW 5.52 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     oe-rel.ship-id AT ROW 5.52 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-relh.rel-date AT ROW 6.24 COL 100 COLON-ALIGNED
          LABEL "Release Date"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-rel.ship-addr[1] AT ROW 6.71 COL 23 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-ordl.req-code AT ROW 7.43 COL 100 COLON-ALIGNED
          LABEL "Requested"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-ordl.req-date AT ROW 7.43 COL 105 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-rel.ship-city AT ROW 7.91 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-rel.ship-state AT ROW 7.91 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-rel.ship-zip AT ROW 7.91 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ordl.prom-code AT ROW 8.62 COL 100 COLON-ALIGNED
          LABEL "Promised"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-ordl.prom-date AT ROW 8.62 COL 105 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-ordl.qty AT ROW 9.1 COL 23 COLON-ALIGNED
          LABEL "Qty Ordered"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     oe-ordl.rel-qty AT ROW 10.29 COL 23 COLON-ALIGNED
          LABEL "Qty Released"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-ord.frt-pay AT ROW 10.29 COL 100 COLON-ALIGNED
          LABEL "Freight Pay"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-ord.fob-code AT ROW 10.29 COL 113 COLON-ALIGNED
          LABEL "FOB"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.ship-qty AT ROW 11.48 COL 23 COLON-ALIGNED
          LABEL "Qty Shipped"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-ord.carrier AT ROW 11.48 COL 100 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.qty-on-bo AT ROW 12.67 COL 23 COLON-ALIGNED
          LABEL "Qty Back Ordered"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     oe-ordl.t-weight AT ROW 12.67 COL 100 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ordl.rel-stat AT ROW 13.86 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     oe-ordl.t-freight AT ROW 13.86 COL 100 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.f-bill AT ROW 13.86 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     RECT-2 AT ROW 4.81 COL 2
     "Shipping Information" VIEW-AS TEXT
          SIZE 25 BY .71 AT ROW 4.1 COL 58
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ordl,ASI.oe-ord,ASI.oe-rel,ASI.oe-relh
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

/* SETTINGS FOR FILL-IN oe-ordl.est-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.fob-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.frt-pay IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.job-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.prom-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.qty-on-bo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-relh.rel-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.rel-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.req-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rel.ship-addr[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.ship-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.stat IN FRAME F-Main
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
  {src/adm/template/row-list.i "oe-ord"}
  {src/adm/template/row-list.i "oe-rel"}
  {src/adm/template/row-list.i "oe-relh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}
  {src/adm/template/row-find.i "oe-ord"}
  {src/adm/template/row-find.i "oe-rel"}
  {src/adm/template/row-find.i "oe-relh"}

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
  {src/adm/template/snd-list.i "oe-rel"}
  {src/adm/template/snd-list.i "oe-relh"}

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


