&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/usrcmp-l.w

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

DEFINE VARIABLE op-user_id AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-company AS CHARACTER NO-UNDO.
{methods/defines/loc.i &NEW="NEW"}
DEFINE BUFFER b-usercomp FOR usercomp .
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
&Scoped-define EXTERNAL-TABLES usercomp
&Scoped-define FIRST-EXTERNAL-TABLE usercomp


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR usercomp.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS usercomp.loc usercomp.loc_default 
&Scoped-define ENABLED-TABLES usercomp
&Scoped-define FIRST-ENABLED-TABLE usercomp
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS usercomp.loc usercomp.loc_default 
&Scoped-define DISPLAYED-TABLES usercomp
&Scoped-define FIRST-DISPLAYED-TABLE usercomp
&Scoped-Define DISPLAYED-OBJECTS F1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define F1 F1 

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
DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     usercomp.loc AT ROW 1.24 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     usercomp.loc_default AT ROW 1.24 COL 27
          LABEL "Default"
          VIEW-AS TOGGLE-BOX
          SIZE 12.6 BY 1
     F1 AT ROW 1.24 COL 24 NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.usercomp
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
         HEIGHT             = 1.43
         WIDTH              = 43.4.
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

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX usercomp.loc_default IN FRAME F-Main
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





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME usercomp.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL usercomp.loc V-table-Win
ON HELP OF usercomp.loc IN FRAME F-Main /* Location */
DO:
   DEF VAR char-val AS cha NO-UNDO.

   {methods/run_link.i "RECORD-SOURCE" "Get-Values"
      "(OUTPUT op-user_id,OUTPUT op-company)"}
   RUN windows/l-loc.w (op-company,FOCUS:SCREEN-VALUE,OUTPUT char-val).
   IF char-val <> "" THEN ASSIGN SELF:SCREEN-VALUE = ENTRY(1,char-val).
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL usercomp.loc V-table-Win
ON LEAVE OF usercomp.loc IN FRAME F-Main /* Location */
DO:
  IF LASTKEY = -1 THEN RETURN.
  {&methods/lValidateError.i YES}
  {methods/run_link.i "RECORD-SOURCE" "Get-Values"
      "(OUTPUT op-user_id,OUTPUT op-company)"}
  IF NOT CAN-FIND(loc WHERE loc.company = op-company
                        AND loc.loc = {&SELF-NAME}:SCREEN-VALUE) THEN
  DO:
    MESSAGE "Location Code Does Not Exist" VIEW-AS ALERT-BOX.
    IF AVAILABLE {&FIRST-EXTERNAL-TABLE} THEN
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

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
  {src/adm/template/row-list.i "usercomp"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "usercomp"}

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
  {methods/viewers/create/usrcmp-l.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR vDefault AS CHAR NO-UNDO.
 DEF VAR default-val AS LOG NO-UNDO .
 DEFINE BUFFER bff-usercomp FOR usercomp .
 ASSIGN default-val = usercomp.loc_default .
  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-user_id,OUTPUT op-company)"}
  {&methods/lValidateError.i YES}
  IF NOT CAN-FIND(loc WHERE loc.company = op-company
                        AND loc.loc = usercomp.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME} )
  THEN DO:
    MESSAGE "Location Code Does Not Exist" VIEW-AS ALERT-BOX.
    APPLY "entry" TO usercomp.loc.
    RETURN NO-APPLY.
  END.
 IF default-val THEN
  FOR EACH b-usercomp WHERE b-usercomp.company = op-company
                         AND b-usercomp.user_id = op-user_id
                         AND ROWID(b-usercomp) <>  rowid(usercomp)
                         NO-LOCK:
     IF b-usercomp.loc_default = Yes THEN
       ASSIGN vDefault = "Yes" .
  END.
  IF default-val AND vDefault <> "Yes"  AND usercomp.loc_default:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"  THEN DO:
    MESSAGE "User must have 1 default Location " VIEW-AS ALERT-BOX.
    APPLY "entry" TO usercomp.loc.
    RETURN NO-APPLY.
  END.

  FIND FIRST bff-usercomp NO-LOCK
        WHERE bff-usercomp.company = op-company
          AND bff-usercomp.user_id = op-user_id 
          AND bff-usercomp.loc_default = Yes
          AND ROWID(bff-usercomp) <>  rowid(usercomp)
          AND usercomp.loc_default:SCREEN-VALUE EQ "Yes"
      NO-ERROR .
     IF AVAIL bff-usercomp THEN DO:
        MESSAGE "Are you sure you want to reset the default location to " 
           usercomp.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN RETURN.
        IF ll-ans THEN DO:
            FIND CURRENT bff-usercomp EXCLUSIVE-LOCK NO-ERROR .
            ASSIGN bff-usercomp.loc_default = NO .
            FIND CURRENT bff-usercomp NO-LOCK NO-ERROR .
        END.
     END.

  {&methods/lValidateError.i NO}
  IF usercomp.loc_default:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Yes" THEN DO:  /* task 07071403 */
      find first ce-ctrl where ce-ctrl.company = op-company and
                                  ce-ctrl.loc = usercomp.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                                                  no-lock no-error.
     if not available ce-ctrl then do:
                    create ce-ctrl.
                    assign
                    ce-ctrl.company = op-company
                    ce-ctrl.loc     = usercomp.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME} .
         end.
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

 /* Code placed here will execute AFTER standard behavior.    */
  DEF VAR char-hdl AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
  RUN dispatch IN WIDGET-HANDLE(char-hdl) ("open-query").

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
  {src/adm/template/snd-list.i "usercomp"}

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

