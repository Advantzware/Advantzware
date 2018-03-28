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
&Scoped-define EXTERNAL-TABLES EDShipto
&Scoped-define FIRST-EXTERNAL-TABLE EDShipto


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDShipto.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDShipto.Cust EDShipto.Partner ~
EDShipto.Description EDShipto.Ship-to EDShipto.St-code EDShipto.By-code ~
EDShipto.Opened EDShipto.Ref-type EDShipto.Attention EDShipto.Name ~
EDShipto.Addr1 EDShipto.Addr2 EDShipto.City EDShipto.Cust-Region ~
EDShipto.State EDShipto.Zip EDShipto.Dest-Zone EDShipto.Country ~
EDShipto.Phone EDShipto.Fax EDShipto.Comments[1] EDShipto.Comments[2] ~
EDShipto.Comments[3] EDShipto.Comments[4] EDShipto.Comments[5] 
&Scoped-define ENABLED-TABLES EDShipto
&Scoped-define FIRST-ENABLED-TABLE EDShipto
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS EDShipto.Cust EDShipto.Partner ~
EDShipto.Description EDShipto.Ship-to EDShipto.St-code EDShipto.By-code ~
EDShipto.Opened EDShipto.Ref-type EDShipto.Attention EDShipto.Name ~
EDShipto.Addr1 EDShipto.Addr2 EDShipto.City EDShipto.Cust-Region ~
EDShipto.State EDShipto.Zip EDShipto.Dest-Zone EDShipto.Country ~
EDShipto.Phone EDShipto.Fax EDShipto.Comments[1] EDShipto.Comments[2] ~
EDShipto.Comments[3] EDShipto.Comments[4] EDShipto.Comments[5] 
&Scoped-define DISPLAYED-TABLES EDShipto
&Scoped-define FIRST-DISPLAYED-TABLE EDShipto


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
     EDShipto.Cust AT ROW 1 COL 43 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDShipto.Partner AT ROW 1.1 COL 16 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     EDShipto.Description AT ROW 2.1 COL 16 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDShipto.Ship-to AT ROW 3.1 COL 16 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDShipto.St-code AT ROW 3.1 COL 43 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDShipto.By-code AT ROW 3.14 COL 72 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDShipto.Opened AT ROW 4.1 COL 16 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDShipto.Ref-type AT ROW 4.1 COL 43.2 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDShipto.Attention AT ROW 5.1 COL 16.2 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDShipto.Name AT ROW 6.48 COL 16.2 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     EDShipto.Addr1 AT ROW 7.48 COL 16.2 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     EDShipto.Addr2 AT ROW 8.48 COL 16.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     EDShipto.City AT ROW 9.48 COL 16.2 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 29.6 BY 1
     EDShipto.Cust-Region AT ROW 9.52 COL 62.4 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     EDShipto.State AT ROW 10.48 COL 16 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDShipto.Zip AT ROW 10.48 COL 27.6 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDShipto.Dest-Zone AT ROW 10.52 COL 79 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     EDShipto.Country AT ROW 10.57 COL 52.6 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDShipto.Phone AT ROW 11.48 COL 16 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDShipto.Fax AT ROW 11.52 COL 45 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDShipto.Comments[1] AT ROW 13.05 COL 16.2 COLON-ALIGNED WIDGET-ID 12
          LABEL "Comments"
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDShipto.Comments[2] AT ROW 14.05 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDShipto.Comments[3] AT ROW 15.05 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDShipto.Comments[4] AT ROW 16.05 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDShipto.Comments[5] AT ROW 17.05 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDShipto
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

/* SETTINGS FOR FILL-IN EDShipto.Comments[1] IN FRAME F-Main
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
  {src/adm/template/row-list.i "EDShipto"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDShipto"}

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
  {src/adm/template/snd-list.i "EDShipto"}

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

