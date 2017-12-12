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
&Scoped-define EXTERNAL-TABLES EDMast
&Scoped-define FIRST-EXTERNAL-TABLE EDMast


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDMast.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDMast.Partner EDMast.Sf-code EDMast.Cust ~
EDMast.Ship-to-mask EDMast.Vendor EDMast.User-name EDMast.ASN-on-DS ~
EDMast.We-cust EDMast.Del-Days EDMast.We-vend-no EDMast.ID-Len ~
EDMast.ID-Out EDMast.Id-trim EDMast.Item-Length EDMast.Item-Prefix ~
EDMast.Item-suffix EDMast.Order-no-mask EDMast.Path-in EDMast.Path-out ~
EDMast.RE-code EDMast.rec_key EDMast.Seq 
&Scoped-define ENABLED-TABLES EDMast
&Scoped-define FIRST-ENABLED-TABLE EDMast
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS EDMast.Partner EDMast.Sf-code EDMast.Cust ~
EDMast.Ship-to-mask EDMast.Vendor EDMast.User-name EDMast.ASN-on-DS ~
EDMast.We-cust EDMast.Del-Days EDMast.We-vend-no EDMast.ID-Len ~
EDMast.ID-Out EDMast.Id-trim EDMast.Item-Length EDMast.Item-Prefix ~
EDMast.Item-suffix EDMast.Order-no-mask EDMast.Path-in EDMast.Path-out ~
EDMast.RE-code EDMast.rec_key EDMast.Seq 
&Scoped-define DISPLAYED-TABLES EDMast
&Scoped-define FIRST-DISPLAYED-TABLE EDMast


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
     EDMast.Partner AT ROW 1 COL 16 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     EDMast.Sf-code AT ROW 1 COL 81 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDMast.Cust AT ROW 2 COL 16 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.Ship-to-mask AT ROW 2 COL 81 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDMast.Vendor AT ROW 3 COL 16 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.User-name AT ROW 3 COL 81 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     EDMast.ASN-on-DS AT ROW 4 COL 16 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDMast.We-cust AT ROW 4 COL 81 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.Del-Days AT ROW 5 COL 22.8 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDMast.We-vend-no AT ROW 5 COL 81 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.ID-Len AT ROW 6 COL 16 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDMast.ID-Out AT ROW 7 COL 23.8 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDMast.Id-trim AT ROW 8 COL 16 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDMast.Item-Length AT ROW 9 COL 16 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDMast.Item-Prefix AT ROW 10 COL 16 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDMast.Item-suffix AT ROW 11 COL 16 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDMast.Order-no-mask AT ROW 12 COL 17.2 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDMast.Path-in AT ROW 13 COL 16 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDMast.Path-out AT ROW 14 COL 16 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDMast.RE-code AT ROW 15 COL 16 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDMast.rec_key AT ROW 16 COL 16 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     EDMast.Seq AT ROW 17 COL 16 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDMast
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
  {src/adm/template/row-list.i "EDMast"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDMast"}

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
  {src/adm/template/snd-list.i "EDMast"}

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

