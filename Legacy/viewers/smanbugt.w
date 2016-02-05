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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES smanbugt
&Scoped-define FIRST-EXTERNAL-TABLE smanbugt


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR smanbugt.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS smanbugt.budget-yr smanbugt.budget-amt[1] ~
smanbugt.msf[1] smanbugt.ton[1] smanbugt.budget-amt[2] smanbugt.msf[2] ~
smanbugt.ton[2] smanbugt.budget-amt[3] smanbugt.msf[3] smanbugt.ton[3] ~
smanbugt.msf[4] smanbugt.budget-amt[4] smanbugt.ton[4] smanbugt.msf[5] ~
smanbugt.budget-amt[5] smanbugt.ton[5] smanbugt.msf[6] ~
smanbugt.budget-amt[6] smanbugt.ton[6] smanbugt.msf[7] ~
smanbugt.budget-amt[7] smanbugt.ton[7] smanbugt.msf[8] smanbugt.ton[8] ~
smanbugt.budget-amt[8] smanbugt.msf[9] smanbugt.ton[9] ~
smanbugt.budget-amt[9] smanbugt.msf[10] smanbugt.ton[10] ~
smanbugt.budget-amt[10] smanbugt.msf[11] smanbugt.ton[11] ~
smanbugt.budget-amt[11] smanbugt.msf[12] smanbugt.ton[12] ~
smanbugt.budget-amt[12] smanbugt.msf[13] smanbugt.ton[13] ~
smanbugt.budget-amt[13] 
&Scoped-define ENABLED-TABLES smanbugt
&Scoped-define FIRST-ENABLED-TABLE smanbugt
&Scoped-Define DISPLAYED-FIELDS smanbugt.budget-yr smanbugt.budget-amt[1] ~
smanbugt.msf[1] smanbugt.ton[1] smanbugt.budget-amt[2] smanbugt.msf[2] ~
smanbugt.ton[2] smanbugt.budget-amt[3] smanbugt.msf[3] smanbugt.ton[3] ~
smanbugt.msf[4] smanbugt.budget-amt[4] smanbugt.ton[4] smanbugt.msf[5] ~
smanbugt.budget-amt[5] smanbugt.ton[5] smanbugt.msf[6] ~
smanbugt.budget-amt[6] smanbugt.ton[6] smanbugt.msf[7] ~
smanbugt.budget-amt[7] smanbugt.ton[7] smanbugt.msf[8] smanbugt.ton[8] ~
smanbugt.budget-amt[8] smanbugt.msf[9] smanbugt.ton[9] ~
smanbugt.budget-amt[9] smanbugt.msf[10] smanbugt.ton[10] ~
smanbugt.budget-amt[10] smanbugt.msf[11] smanbugt.ton[11] ~
smanbugt.budget-amt[11] smanbugt.msf[12] smanbugt.ton[12] ~
smanbugt.budget-amt[12] smanbugt.msf[13] smanbugt.ton[13] ~
smanbugt.budget-amt[13] 
&Scoped-define DISPLAYED-TABLES smanbugt
&Scoped-define FIRST-DISPLAYED-TABLE smanbugt


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

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     smanbugt.budget-yr AT ROW 2.67 COL 14.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     smanbugt.budget-amt[1] AT ROW 2.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[1] AT ROW 2.67 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[1] AT ROW 2.67 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[2] AT ROW 3.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[2] AT ROW 3.67 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[2] AT ROW 3.67 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[3] AT ROW 4.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[3] AT ROW 4.67 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[3] AT ROW 4.67 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.msf[4] AT ROW 5.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[4] AT ROW 5.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.ton[4] AT ROW 5.67 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.msf[5] AT ROW 6.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[5] AT ROW 6.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.ton[5] AT ROW 6.67 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.msf[6] AT ROW 7.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[6] AT ROW 7.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.ton[6] AT ROW 7.67 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.msf[7] AT ROW 8.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[7] AT ROW 8.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.ton[7] AT ROW 8.67 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.msf[8] AT ROW 9.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[8] AT ROW 9.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[8] AT ROW 9.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[9] AT ROW 10.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[9] AT ROW 10.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[9] AT ROW 10.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[10] AT ROW 11.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[10] AT ROW 11.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     smanbugt.budget-amt[10] AT ROW 11.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[11] AT ROW 12.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[11] AT ROW 12.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[11] AT ROW 12.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[12] AT ROW 13.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[12] AT ROW 13.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[12] AT ROW 13.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     smanbugt.msf[13] AT ROW 14.52 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.ton[13] AT ROW 14.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     smanbugt.budget-amt[13] AT ROW 14.67 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.smanbugt
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
  {src/adm/template/row-list.i "smanbugt"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "smanbugt"}

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
  {src/adm/template/snd-list.i "smanbugt"}

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

