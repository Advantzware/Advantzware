&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
          asihlp           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i}

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
&Scoped-define EXTERNAL-TABLES hlp-head
&Scoped-define FIRST-EXTERNAL-TABLE hlp-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR hlp-head.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS hlp-head.FLD-NAME hlp-head.FRM-TITLE ~
hlp-head.FIL-NAME hlp-head.FRM-NAME hlp-head.help-txt 
&Scoped-define ENABLED-TABLES hlp-head
&Scoped-define FIRST-ENABLED-TABLE hlp-head
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS hlp-head.MSG-NUM hlp-head.FLD-NAME ~
hlp-head.FRM-TITLE hlp-head.FIL-NAME hlp-head.FRM-NAME hlp-head.help-txt 
&Scoped-define DISPLAYED-TABLES hlp-head
&Scoped-define FIRST-DISPLAYED-TABLE hlp-head


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS hlp-head.MSG-NUM 

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
     hlp-head.MSG-NUM AT ROW 1.24 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     hlp-head.FLD-NAME AT ROW 2.19 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     hlp-head.FRM-TITLE AT ROW 2.19 COL 78 COLON-ALIGNED
          LABEL "TITLE" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     hlp-head.FIL-NAME AT ROW 3.14 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     hlp-head.FRM-NAME AT ROW 3.14 COL 78 COLON-ALIGNED FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     hlp-head.help-txt AT ROW 4.57 COL 29 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 115 BY 13.1
     "Help Contents:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 4.33 COL 10
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asihlp.hlp-head
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
         HEIGHT             = 19.52
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer4.i}

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

/* SETTINGS FOR FILL-IN hlp-head.FRM-NAME IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN hlp-head.FRM-TITLE IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       hlp-head.help-txt:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR FILL-IN hlp-head.MSG-NUM IN FRAME F-Main
   NO-ENABLE 2                                                          */
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
  {src/adm/template/row-list.i "hlp-head"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "hlp-head"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  def buffer bf-hlp for hlp-head.
  def var li-next-num as int no-undo.

  find last bf-hlp use-index mess-num no-lock no-error.
  if avail bf-hlp then li-next-num = bf-hlp.msg-num + 1.
  else li-next-num = 1.

  hlp-head.msg-num = li-next-num.
  display hlp-head.msg-num with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
/* mod - sewa for Web Services task 08211210 */
    DEF VAR vconn AS CHAR  NO-UNDO.
    DEFINE VARIABLE vhWebService AS HANDLE NO-UNDO.
    DEFINE VARIABLE vhSalesSoap AS HANDLE NO-UNDO.
    DEFINE VARIABLE parameters1 AS LONGCHAR NO-UNDO.
/*mod - sewa*/

  /* Code placed here will execute PRIOR to standard behavior. */
  session:set-wait-state("general").
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  session:set-wait-state("").

/* mod - sewa for Web Services task 08211210 */

find first sys-ctrl  WHERE sys-ctrl.name    eq "UpdateService"
        no-lock no-error.
  IF AVAIL sys-ctrl THEN
      ASSIGN vconn = sys-ctrl.char-fld .
  ELSE
      vconn = "".

      CREATE SERVER vhWebService.
      vhWebService:CONNECT(vconn) NO-ERROR.

      IF NOT vhWebService:CONNECTED() THEN
      DO:
        STOP.
      END.

      RUN Service1Soap SET vhSalesSoap ON vhWebService .

      RUN HelpInsert IN vhSalesSoap(INPUT string(hlp-head.MSG-NUM),INPUT STRING(hlp-head.FLD-NAME),INPUT STRING(hlp-head.FRM-TITLE),INPUT string(hlp-head.FIL-NAME),INPUT STRING(hlp-head.FRM-NAME),INPUT STRING(hlp-head.help-txt),  OUTPUT parameters1).
/* mod- sewa */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* mod - sewa for Web Services task 08211210 */
    DEF VAR msg-num AS CHAR NO-UNDO.
    DEF VAR vconn AS CHAR  NO-UNDO.
    DEFINE VARIABLE vhWebService AS HANDLE NO-UNDO.
    DEFINE VARIABLE vhSalesSoap AS HANDLE NO-UNDO.
    DEFINE VARIABLE parameters1 AS LONGCHAR NO-UNDO.
    /*mod -sewa */
    ASSIGN msg-num = string(hlp-head.MSG-NUM) .   /*mod- sewa*/
  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/template/local/delete.i}

  &IF "{&FIRST-ENABLED-TABLE}" EQ "notes" &THEN
    RUN custom/notewtrg.p (ROWID({&FIRST-ENABLED-TABLE})).
  &ENDIF

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/deleteAfter.i}

  /* mod - sewa for Web Services task 08211210 */
   find first sys-ctrl  WHERE sys-ctrl.name    eq "UpdateService"
        no-lock no-error.
  IF AVAIL sys-ctrl THEN
      ASSIGN vconn = sys-ctrl.char-fld .
  ELSE
      vconn = "".

      CREATE SERVER vhWebService.
      vhWebService:CONNECT(vconn) NO-ERROR.

    IF NOT vhWebService:CONNECTED() THEN DO:
    STOP.
    END.
    IF msg-num <> "" THEN do:
        RUN Service1Soap SET vhSalesSoap ON vhWebService .
        RUN HelpDelete IN vhSalesSoap(INPUT string(msg-num),  OUTPUT parameters1).
        msg-num = "".

    END. /*msg-num <> ""*/
  /* mod- sewa*/


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
  {src/adm/template/snd-list.i "hlp-head"}

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

