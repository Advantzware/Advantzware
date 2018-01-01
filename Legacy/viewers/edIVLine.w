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
&Scoped-define EXTERNAL-TABLES EDIVLine
&Scoped-define FIRST-EXTERNAL-TABLE EDIVLine


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDIVLine.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDIVLine.Line EDIVLine.Company ~
EDIVLine.Partner EDIVLine.Invoice-no EDIVLine.Item-no EDIVLine.By-code ~
EDIVLine.Description[1] EDIVLine.Seq EDIVLine.SF-Code ~
EDIVLine.Description[2] EDIVLine.ship-stat EDIVLine.Uom-code ~
EDIVLine.Cust-item-no EDIVLine.Price-basis EDIVLine.Size-desc ~
EDIVLine.Color-desc EDIVLine.Cust-po-line EDIVLine.UPC EDIVLine.Pack-size ~
EDIVLine.Product-type EDIVLine.Config-code EDIVLine.Qty-ord-orig ~
EDIVLine.Unit-price EDIVLine.Item-disc-amount EDIVLine.Qty-shipped ~
EDIVLine.Selling-price EDIVLine.Item-wght-each EDIVLine.Item-net ~
EDIVLine.Size-qual[1] EDIVLine.Dimension[1] EDIVLine.Item-ctn-wght ~
EDIVLine.Item-gross EDIVLine.Size-qual[2] EDIVLine.Dimension[2] ~
EDIVLine.Size-qual[3] EDIVLine.Dimension[3] EDIVLine.Qty-var ~
EDIVLine.Bo-flag EDIVLine.Item-ctn-cube EDIVLine.Item-each-cube ~
EDIVLine.Special-svc-code EDIVLine.Taxable 
&Scoped-define ENABLED-TABLES EDIVLine
&Scoped-define FIRST-ENABLED-TABLE EDIVLine
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 
&Scoped-Define DISPLAYED-FIELDS EDIVLine.Line EDIVLine.Company ~
EDIVLine.Partner EDIVLine.Invoice-no EDIVLine.Item-no EDIVLine.By-code ~
EDIVLine.Description[1] EDIVLine.Seq EDIVLine.SF-Code ~
EDIVLine.Description[2] EDIVLine.ship-stat EDIVLine.Uom-code ~
EDIVLine.Cust-item-no EDIVLine.Price-basis EDIVLine.Size-desc ~
EDIVLine.Color-desc EDIVLine.Cust-po-line EDIVLine.UPC EDIVLine.Pack-size ~
EDIVLine.Product-type EDIVLine.Config-code EDIVLine.Qty-ord-orig ~
EDIVLine.Unit-price EDIVLine.Item-disc-amount EDIVLine.Qty-shipped ~
EDIVLine.Selling-price EDIVLine.Item-wght-each EDIVLine.Item-net ~
EDIVLine.Size-qual[1] EDIVLine.Dimension[1] EDIVLine.Item-ctn-wght ~
EDIVLine.Item-gross EDIVLine.Size-qual[2] EDIVLine.Dimension[2] ~
EDIVLine.Size-qual[3] EDIVLine.Dimension[3] EDIVLine.Qty-var ~
EDIVLine.Bo-flag EDIVLine.Item-ctn-cube EDIVLine.Item-each-cube ~
EDIVLine.rec_key EDIVLine.Special-svc-code EDIVLine.Taxable 
&Scoped-define DISPLAYED-TABLES EDIVLine
&Scoped-define FIRST-DISPLAYED-TABLE EDIVLine


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
Partner|y|y|asi.EDIVLine.Partner
company||y|asi.EDIVLine.company
rec_key||y|asi.EDIVLine.rec_key
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Partner",
     Keys-Supplied = "Partner,company,rec_key"':U).
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
     SIZE 141 BY 3.1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 4.76.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     EDIVLine.Line AT ROW 1.24 COL 45.4 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDIVLine.Company AT ROW 1.24 COL 64.6 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDIVLine.Partner AT ROW 1.24 COL 88.8 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     EDIVLine.Invoice-no AT ROW 1.24 COL 113 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 28.4 BY 1
     EDIVLine.Item-no AT ROW 1.29 COL 16.2 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     EDIVLine.By-code AT ROW 2.52 COL 88 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDIVLine.Description[1] AT ROW 2.67 COL 18 COLON-ALIGNED WIDGET-ID 16
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDIVLine.Seq AT ROW 2.67 COL 118 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDIVLine.SF-Code AT ROW 3.52 COL 88 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDIVLine.Description[2] AT ROW 3.67 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDIVLine.ship-stat AT ROW 3.67 COL 118 COLON-ALIGNED WIDGET-ID 68
          LABEL "Ship Stat"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVLine.Uom-code AT ROW 4.52 COL 88 COLON-ALIGNED WIDGET-ID 84
          LABEL "Uom"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVLine.Cust-item-no AT ROW 4.67 COL 18 COLON-ALIGNED WIDGET-ID 12
          LABEL "Cust Item No"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDIVLine.Price-basis AT ROW 4.67 COL 118 COLON-ALIGNED WIDGET-ID 50
          LABEL "Price Basis"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVLine.Size-desc AT ROW 5.67 COL 18 COLON-ALIGNED WIDGET-ID 70
          LABEL "Size Desc"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     EDIVLine.Color-desc AT ROW 5.67 COL 58.8 COLON-ALIGNED WIDGET-ID 6
          LABEL "Color Desc"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     EDIVLine.Cust-po-line AT ROW 5.81 COL 101 COLON-ALIGNED WIDGET-ID 14
          LABEL "Cust Po line"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDIVLine.UPC AT ROW 6.67 COL 18.2 COLON-ALIGNED WIDGET-ID 86
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVLine.Pack-size AT ROW 6.67 COL 59 COLON-ALIGNED WIDGET-ID 46
          LABEL "Pack Size"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDIVLine.Product-type AT ROW 6.76 COL 101 COLON-ALIGNED WIDGET-ID 52
          LABEL "Product type"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDIVLine.Config-code AT ROW 6.81 COL 129.2 COLON-ALIGNED WIDGET-ID 10
          LABEL "Config Code"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     EDIVLine.Qty-ord-orig AT ROW 8.52 COL 20 COLON-ALIGNED WIDGET-ID 54
          LABEL "Qty Orig Ord"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     EDIVLine.Unit-price AT ROW 8.62 COL 56.4 COLON-ALIGNED WIDGET-ID 82
          LABEL "Unit Price" FORMAT "->>,>>>,>>9.999999"
          VIEW-AS FILL-IN 
          SIZE 20.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     EDIVLine.Item-disc-amount AT ROW 8.62 COL 100 COLON-ALIGNED WIDGET-ID 32
          LABEL "Discount Amount"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVLine.Qty-shipped AT ROW 9.52 COL 20 COLON-ALIGNED WIDGET-ID 56
          LABEL "Qty Shipped"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     EDIVLine.Selling-price AT ROW 9.57 COL 56.4 COLON-ALIGNED WIDGET-ID 62
          LABEL "Selling Price"
          VIEW-AS FILL-IN 
          SIZE 20.6 BY 1
     EDIVLine.Item-wght-each AT ROW 11.81 COL 17 COLON-ALIGNED WIDGET-ID 42
          LABEL "Each"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     EDIVLine.Item-net AT ROW 11.81 COL 48.2 COLON-ALIGNED WIDGET-ID 38
          LABEL "Net"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVLine.Size-qual[1] AT ROW 11.95 COL 124.4 COLON-ALIGNED WIDGET-ID 72
          LABEL "Dim Qualifier"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVLine.Dimension[1] AT ROW 12 COL 91 COLON-ALIGNED WIDGET-ID 20
          LABEL "Dimensions"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDIVLine.Item-ctn-wght AT ROW 12.76 COL 17 COLON-ALIGNED WIDGET-ID 30
          LABEL "Carton"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     EDIVLine.Item-gross AT ROW 12.81 COL 48.4 COLON-ALIGNED WIDGET-ID 36
          LABEL "Gross"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVLine.Size-qual[2] AT ROW 12.95 COL 124.4 COLON-ALIGNED NO-LABEL WIDGET-ID 74
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVLine.Dimension[2] AT ROW 13 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDIVLine.Size-qual[3] AT ROW 13.95 COL 124.4 COLON-ALIGNED NO-LABEL WIDGET-ID 76
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVLine.Dimension[3] AT ROW 14 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDIVLine.Qty-var AT ROW 14.33 COL 17.4 COLON-ALIGNED WIDGET-ID 58
          LABEL "Qty Var"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVLine.Bo-flag AT ROW 14.43 COL 56 COLON-ALIGNED WIDGET-ID 2
          LABEL "Back Order"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVLine.Item-ctn-cube AT ROW 14.95 COL 124.4 COLON-ALIGNED WIDGET-ID 28
          LABEL "Ctn Cube"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDIVLine.Item-each-cube AT ROW 15 COL 91 COLON-ALIGNED WIDGET-ID 34
          LABEL "Cube EA"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDIVLine.rec_key AT ROW 16.48 COL 12 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     EDIVLine.Special-svc-code AT ROW 16.71 COL 64 COLON-ALIGNED WIDGET-ID 78
          LABEL "Special Svc Code"
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     EDIVLine.Taxable AT ROW 16.71 COL 89 COLON-ALIGNED WIDGET-ID 80
     "Dimensions" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 11.24 COL 78 WIDGET-ID 96
          FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Weights" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 11.24 COL 5 WIDGET-ID 94
          FGCOLOR 9 
     RECT-1 AT ROW 1 COL 1.2
     RECT-2 AT ROW 8.05 COL 3 WIDGET-ID 88
     RECT-3 AT ROW 11.48 COL 3 WIDGET-ID 90
     RECT-4 AT ROW 11.48 COL 74 WIDGET-ID 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDIVLine
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
         HEIGHT             = 17.43
         WIDTH              = 144.8.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN EDIVLine.Bo-flag IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Color-desc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Config-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Cust-item-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Cust-po-line IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Description[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Dimension[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Item-ctn-cube IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Item-ctn-wght IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Item-disc-amount IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Item-each-cube IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Item-gross IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Item-net IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Item-wght-each IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Pack-size IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Price-basis IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Product-type IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Qty-ord-orig IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Qty-shipped IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Qty-var IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.rec_key IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Selling-price IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.ship-stat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Size-desc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Size-qual[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVLine.Special-svc-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX EDIVLine.Taxable IN FRAME F-Main
   ALIGN-C VIEW-AS                                                      */
/* SETTINGS FOR FILL-IN EDIVLine.Unit-price IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN EDIVLine.Uom-code IN FRAME F-Main
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'Partner':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = EDIVLine
           &WHERE = "WHERE EDIVLine.Partner eq key-value"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "EDIVLine"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDIVLine"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "Partner" "EDIVLine" "Partner"}
  {src/adm/template/sndkycas.i "company" "EDIVLine" "company"}
  {src/adm/template/sndkycas.i "rec_key" "EDIVLine" "rec_key"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "EDIVLine"}

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

