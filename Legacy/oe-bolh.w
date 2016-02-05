&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-bolh oe-boll oe-boll-qty oe-ship oe-ord ~
shipto soldto
&Scoped-define FIRST-EXTERNAL-TABLE oe-bolh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-bolh, oe-boll, oe-boll-qty, oe-ship, oe-ord, shipto, soldto.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-bolh.bol-no oe-bolh.bol-date ~
oe-bolh.rel-no oe-bolh.printed oe-bolh.frt-pay oe-ship.hold oe-bolh.po-no ~
oe-bolh.carrier oe-bolh.trailer oe-bolh.cust-no oe-bolh.ship-id ~
oe-boll.i-no oe-boll.tag oe-bolh.loc oe-boll.loc-bin oe-boll.cases ~
oe-boll.qty-case oe-boll.partial oe-boll.job-no oe-boll.job-no2 oe-boll.p-c ~
oe-boll-qty.qty oe-boll.weight oe-boll.qty oe-bolh.tot-pallets ~
oe-bolh.freight oe-bolh.cwt oe-bolh.tot-wt soldto.sold-name ~
shipto.ship-name oe-ord.sold-addr[1] shipto.ship-addr[1] ~
oe-ord.sold-addr[2] shipto.ship-addr[2] oe-ord.sold-city oe-ord.sold-state ~
oe-ord.sold-zip shipto.ship-city shipto.ship-state shipto.ship-zip 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}bol-no ~{&FP2}bol-no ~{&FP3}~
 ~{&FP1}bol-date ~{&FP2}bol-date ~{&FP3}~
 ~{&FP1}rel-no ~{&FP2}rel-no ~{&FP3}~
 ~{&FP1}printed ~{&FP2}printed ~{&FP3}~
 ~{&FP1}frt-pay ~{&FP2}frt-pay ~{&FP3}~
 ~{&FP1}hold ~{&FP2}hold ~{&FP3}~
 ~{&FP1}po-no ~{&FP2}po-no ~{&FP3}~
 ~{&FP1}carrier ~{&FP2}carrier ~{&FP3}~
 ~{&FP1}trailer ~{&FP2}trailer ~{&FP3}~
 ~{&FP1}cust-no ~{&FP2}cust-no ~{&FP3}~
 ~{&FP1}ship-id ~{&FP2}ship-id ~{&FP3}~
 ~{&FP1}i-no ~{&FP2}i-no ~{&FP3}~
 ~{&FP1}tag ~{&FP2}tag ~{&FP3}~
 ~{&FP1}loc ~{&FP2}loc ~{&FP3}~
 ~{&FP1}loc-bin ~{&FP2}loc-bin ~{&FP3}~
 ~{&FP1}cases ~{&FP2}cases ~{&FP3}~
 ~{&FP1}qty-case ~{&FP2}qty-case ~{&FP3}~
 ~{&FP1}partial ~{&FP2}partial ~{&FP3}~
 ~{&FP1}job-no ~{&FP2}job-no ~{&FP3}~
 ~{&FP1}job-no2 ~{&FP2}job-no2 ~{&FP3}~
 ~{&FP1}p-c ~{&FP2}p-c ~{&FP3}~
 ~{&FP1}qty ~{&FP2}qty ~{&FP3}~
 ~{&FP1}weight ~{&FP2}weight ~{&FP3}~
 ~{&FP1}qty ~{&FP2}qty ~{&FP3}~
 ~{&FP1}tot-pallets ~{&FP2}tot-pallets ~{&FP3}~
 ~{&FP1}freight ~{&FP2}freight ~{&FP3}~
 ~{&FP1}cwt ~{&FP2}cwt ~{&FP3}~
 ~{&FP1}tot-wt ~{&FP2}tot-wt ~{&FP3}~
 ~{&FP1}sold-name ~{&FP2}sold-name ~{&FP3}~
 ~{&FP1}ship-name ~{&FP2}ship-name ~{&FP3}~
 ~{&FP1}sold-addr[1] ~{&FP2}sold-addr[1] ~{&FP3}~
 ~{&FP1}ship-addr[1] ~{&FP2}ship-addr[1] ~{&FP3}~
 ~{&FP1}sold-addr[2] ~{&FP2}sold-addr[2] ~{&FP3}~
 ~{&FP1}ship-addr[2] ~{&FP2}ship-addr[2] ~{&FP3}~
 ~{&FP1}sold-city ~{&FP2}sold-city ~{&FP3}~
 ~{&FP1}sold-state ~{&FP2}sold-state ~{&FP3}~
 ~{&FP1}sold-zip ~{&FP2}sold-zip ~{&FP3}~
 ~{&FP1}ship-city ~{&FP2}ship-city ~{&FP3}~
 ~{&FP1}ship-state ~{&FP2}ship-state ~{&FP3}~
 ~{&FP1}ship-zip ~{&FP2}ship-zip ~{&FP3}
&Scoped-define ENABLED-TABLES oe-bolh oe-ship oe-boll oe-boll-qty soldto ~
shipto oe-ord
&Scoped-define FIRST-ENABLED-TABLE oe-bolh
&Scoped-define SECOND-ENABLED-TABLE oe-ship
&Scoped-define THIRD-ENABLED-TABLE oe-boll
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-1 
&Scoped-Define DISPLAYED-FIELDS oe-bolh.bol-no oe-bolh.bol-date ~
oe-bolh.rel-no oe-bolh.printed oe-bolh.frt-pay oe-ship.hold oe-bolh.po-no ~
oe-bolh.carrier oe-bolh.trailer oe-bolh.cust-no oe-bolh.ship-id ~
oe-boll.i-no oe-boll.tag oe-bolh.loc oe-boll.loc-bin oe-boll.cases ~
oe-boll.qty-case oe-boll.partial oe-boll.job-no oe-boll.job-no2 oe-boll.p-c ~
oe-boll-qty.qty oe-boll.weight oe-boll.qty oe-bolh.tot-pallets ~
oe-bolh.freight oe-bolh.cwt oe-bolh.tot-wt soldto.sold-name ~
shipto.ship-name oe-ord.sold-addr[1] shipto.ship-addr[1] ~
oe-ord.sold-addr[2] shipto.ship-addr[2] oe-ord.sold-city oe-ord.sold-state ~
oe-ord.sold-zip shipto.ship-city shipto.ship-state shipto.ship-zip 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 151 BY 9.52.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 151 BY 9.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-bolh.bol-no AT ROW 2.91 COL 19 COLON-ALIGNED
          LABEL "Bill of Lading No:"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-bolh.bol-date AT ROW 2.91 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-bolh.rel-no AT ROW 2.91 COL 126 COLON-ALIGNED
          LABEL "Rel No"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     oe-bolh.printed AT ROW 2.91 COL 144 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-bolh.frt-pay AT ROW 4.1 COL 126 COLON-ALIGNED
          LABEL "Frt Pay"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-ship.hold AT ROW 4.1 COL 140 COLON-ALIGNED
          LABEL "Hold"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-bolh.po-no AT ROW 5.29 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-bolh.carrier AT ROW 5.29 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     oe-bolh.trailer AT ROW 5.29 COL 126 COLON-ALIGNED
          LABEL "Trailer No"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-bolh.cust-no AT ROW 6.71 COL 19 COLON-ALIGNED
          LABEL "Bill To"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-bolh.ship-id AT ROW 6.71 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-boll.i-no AT ROW 15.29 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-boll.tag AT ROW 15.29 COL 22 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-bolh.loc AT ROW 15.29 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     oe-boll.loc-bin AT ROW 15.29 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-boll.cases AT ROW 15.29 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-boll.qty-case AT ROW 15.29 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     oe-boll.partial AT ROW 15.29 COL 107 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     oe-boll.job-no AT ROW 15.29 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-boll.job-no2 AT ROW 15.29 COL 133 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     oe-boll.p-c AT ROW 15.29 COL 144 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-boll-qty.qty AT ROW 22.91 COL 15 COLON-ALIGNED
          LABEL "Total Item Qty"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-boll.weight AT ROW 22.91 COL 49 COLON-ALIGNED
          LABEL "Item Weight"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-boll.qty AT ROW 22.91 COL 122 COLON-ALIGNED
          LABEL "Total BOL Qty"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     oe-bolh.tot-pallets AT ROW 24.1 COL 15 COLON-ALIGNED
          LABEL "Pallets"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-bolh.freight AT ROW 24.1 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     oe-bolh.cwt AT ROW 24.1 COL 88 COLON-ALIGNED
          LABEL "Rate / 100 Wt"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-bolh.tot-wt AT ROW 24.1 COL 122 COLON-ALIGNED
          LABEL "Total WT"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     soldto.sold-name AT ROW 7.91 COL 19 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 32 BY .62
     shipto.ship-name AT ROW 7.91 COL 99 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 32 BY .62
     oe-ord.sold-addr[1] AT ROW 8.62 COL 19 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 32 BY .62
     shipto.ship-addr[1] AT ROW 8.62 COL 99 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 32 BY .62
     oe-ord.sold-addr[2] AT ROW 9.33 COL 19 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 32 BY .62
     shipto.ship-addr[2] AT ROW 9.33 COL 99 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 32 BY .62
     oe-ord.sold-city AT ROW 10.05 COL 19 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 17 BY .62
     oe-ord.sold-state AT ROW 10.05 COL 36 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 6.4 BY .62
     oe-ord.sold-zip AT ROW 10.05 COL 43 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 11 BY .62
     shipto.ship-city AT ROW 10.05 COL 99 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 17 BY .62
     shipto.ship-state AT ROW 10.05 COL 116 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 6.4 BY .62
     shipto.ship-zip AT ROW 10.05 COL 123 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 11 BY .62
     "Whs" VIEW-AS TEXT
          SIZE 7 BY .71 AT ROW 14.33 COL 42
     "Item #" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 14.33 COL 9
     "Bin Loc" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 14.33 COL 56
     "Partial" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 14.33 COL 111
     "Tag" VIEW-AS TEXT
          SIZE 7 BY .71 AT ROW 14.33 COL 27
     "P/C" VIEW-AS TEXT
          SIZE 4 BY .95 AT ROW 14.33 COL 146
     "Qty / Unit" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 14.33 COL 91
     "-" VIEW-AS TEXT
          SIZE 2 BY .95 AT ROW 15.29 COL 133
     "Job No" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 14.33 COL 129
     "Units" VIEW-AS TEXT
          SIZE 7 BY .71 AT ROW 14.33 COL 74
     RECT-2 AT ROW 13.14 COL 2
     "B I L L  O F  L A D I N G  P R O C E S S I N G" VIEW-AS TEXT
          SIZE 48 BY 1.43 AT ROW 1.24 COL 55
          FONT 14
     RECT-1 AT ROW 2.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-bolh,ASI.oe-boll,ASI.oe-boll-qty,ASI.oe-ship,ASI.oe-ord,ASI.shipto,ASI.soldto
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
         HEIGHT             = 24.86
         WIDTH              = 155.6.
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

/* SETTINGS FOR FILL-IN oe-bolh.bol-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.cust-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.cwt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.frt-pay IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ship.hold IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-boll.qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-boll-qty.qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.rel-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.tot-pallets IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.tot-wt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.trailer IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-boll.weight IN FRAME F-Main
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
  {src/adm/template/row-list.i "oe-bolh"}
  {src/adm/template/row-list.i "oe-boll"}
  {src/adm/template/row-list.i "oe-boll-qty"}
  {src/adm/template/row-list.i "oe-ship"}
  {src/adm/template/row-list.i "oe-ord"}
  {src/adm/template/row-list.i "shipto"}
  {src/adm/template/row-list.i "soldto"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-bolh"}
  {src/adm/template/row-find.i "oe-boll"}
  {src/adm/template/row-find.i "oe-boll-qty"}
  {src/adm/template/row-find.i "oe-ship"}
  {src/adm/template/row-find.i "oe-ord"}
  {src/adm/template/row-find.i "shipto"}
  {src/adm/template/row-find.i "soldto"}

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
  {src/adm/template/snd-list.i "oe-bolh"}
  {src/adm/template/snd-list.i "oe-boll"}
  {src/adm/template/snd-list.i "oe-boll-qty"}
  {src/adm/template/snd-list.i "oe-ship"}
  {src/adm/template/snd-list.i "oe-ord"}
  {src/adm/template/snd-list.i "shipto"}
  {src/adm/template/snd-list.i "soldto"}

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


