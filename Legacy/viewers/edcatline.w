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
&Scoped-define EXTERNAL-TABLES EDCatline
&Scoped-define FIRST-EXTERNAL-TABLE EDCatline


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDCatline.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDCatline.Commodity-code EDCatline.Pack-wght ~
EDCatline.Commodity-Grouping EDCatline.Partner EDCatline.Commodity-qual ~
EDCatline.Price EDCatline.Config-code EDCatline.Qty ~
EDCatline.Description[1] EDCatline.Qty-Max EDCatline.Description[2] ~
EDCatline.Qty-Min EDCatline.Dim-uom EDCatline.rec_key ~
EDCatline.Dimensions[1] EDCatline.Color-ID EDCatline.Seq ~
EDCatline.Dimensions[2] EDCatline.Size-ID EDCatline.Dimensions[3] ~
EDCatline.Uom-code EDCatline.Inner-units EDCatline.UPC EDCatline.Last-line ~
EDCatline.Vendor-Item EDCatline.Line EDCatline.Volume-uom EDCatline.Lines ~
EDCatline.Wght-uom EDCatline.Pack-size EDCatline.PID-Code ~
EDCatline.Pack-Volume 
&Scoped-define ENABLED-TABLES EDCatline
&Scoped-define FIRST-ENABLED-TABLE EDCatline
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS EDCatline.Commodity-code ~
EDCatline.Pack-wght EDCatline.Commodity-Grouping EDCatline.Partner ~
EDCatline.Commodity-qual EDCatline.Price EDCatline.Config-code ~
EDCatline.Qty EDCatline.Description[1] EDCatline.Qty-Max ~
EDCatline.Description[2] EDCatline.Qty-Min EDCatline.Dim-uom ~
EDCatline.rec_key EDCatline.Dimensions[1] EDCatline.Color-ID EDCatline.Seq ~
EDCatline.Dimensions[2] EDCatline.Size-ID EDCatline.Dimensions[3] ~
EDCatline.Uom-code EDCatline.Inner-units EDCatline.UPC EDCatline.Last-line ~
EDCatline.Vendor-Item EDCatline.Line EDCatline.Volume-uom EDCatline.Lines ~
EDCatline.Wght-uom EDCatline.Pack-size EDCatline.PID-Code ~
EDCatline.Pack-Volume 
&Scoped-define DISPLAYED-TABLES EDCatline
&Scoped-define FIRST-DISPLAYED-TABLE EDCatline


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
Partner|y|y|asi.EDCatline.Partner
Color-ID||y|asi.EDCatline.Color-ID
description||y|asi.EDCatline.description[1]
rec_key||y|asi.EDCatline.rec_key
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Partner",
     Keys-Supplied = "Partner,Color-ID,description,rec_key"':U).
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
     EDCatline.Commodity-code AT ROW 1.71 COL 27 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     EDCatline.Pack-wght AT ROW 2.19 COL 90 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.Commodity-Grouping AT ROW 2.76 COL 27 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDCatline.Partner AT ROW 3.29 COL 90 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     EDCatline.Commodity-qual AT ROW 3.76 COL 27 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDCatline.Price AT ROW 4.33 COL 90 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.Config-code AT ROW 4.76 COL 27 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     EDCatline.Qty AT ROW 5.29 COL 90 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDCatline.Description[1] AT ROW 5.76 COL 27 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     EDCatline.Qty-Max AT ROW 6.33 COL 90 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.Description[2] AT ROW 6.76 COL 27 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     EDCatline.Qty-Min AT ROW 7.43 COL 90 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.Dim-uom AT ROW 7.76 COL 27 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDCatline.rec_key AT ROW 8.38 COL 90 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     EDCatline.Dimensions[1] AT ROW 8.76 COL 27 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.Color-ID AT ROW 8.86 COL 49 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDCatline.Seq AT ROW 9.38 COL 90 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDCatline.Dimensions[2] AT ROW 9.76 COL 27 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.Size-ID AT ROW 10.38 COL 90 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     EDCatline.Dimensions[3] AT ROW 10.76 COL 27 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.Uom-code AT ROW 11.38 COL 90 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDCatline.Inner-units AT ROW 11.76 COL 27 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDCatline.UPC AT ROW 12.38 COL 90 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDCatline.Last-line AT ROW 12.76 COL 27 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDCatline.Vendor-Item AT ROW 13.38 COL 90 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDCatline.Line AT ROW 13.76 COL 27 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     EDCatline.Volume-uom AT ROW 14.38 COL 90 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     EDCatline.Lines AT ROW 14.76 COL 27 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDCatline.Wght-uom AT ROW 15.38 COL 90 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDCatline.Pack-size AT ROW 15.76 COL 27 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDCatline.PID-Code AT ROW 16.48 COL 90 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDCatline.Pack-Volume AT ROW 16.76 COL 27 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDCatline
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

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
           &TABLE = EDCatline
           &WHERE = "WHERE EDCatline.Partner eq key-value"
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
  {src/adm/template/row-list.i "EDCatline"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDCatline"}

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
  {src/adm/template/sndkycas.i "Partner" "EDCatline" "Partner"}
  {src/adm/template/sndkycas.i "Color-ID" "EDCatline" "Color-ID"}
  {src/adm/template/sndkycas.i "description" "EDCatline" "description[1]"}
  {src/adm/template/sndkycas.i "rec_key" "EDCatline" "rec_key"}

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
  {src/adm/template/snd-list.i "EDCatline"}

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

