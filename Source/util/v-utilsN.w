&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/terms.w

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
DEF VAR lAddRecord AS LOG NO-UNDO.
DEF VAR iSecurityLevel AS INT NO-UNDO.
DEF VAR iBaseLevel AS INT NO-UNDO.

{custom/gcompany.i}

FIND FIRST users NO-LOCK WHERE 
    users.user_id EQ USERID(LDBNAME(1)) 
    NO-ERROR.
IF AVAILABLE users THEN ASSIGN 
    iSecurityLevel = users.securityLevel.
    
/* The below variables are used in run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.    

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
&Scoped-define EXTERNAL-TABLES utilities
&Scoped-define FIRST-EXTERNAL-TABLE utilities


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR utilities.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS utilities.programName utilities.module ~
utilities.hotkey utilities.description utilities.securityLevel ~
utilities.notes 
&Scoped-define ENABLED-TABLES utilities
&Scoped-define FIRST-ENABLED-TABLE utilities
&Scoped-Define ENABLED-OBJECTS btnRun RECT-1 
&Scoped-Define DISPLAYED-FIELDS utilities.programName utilities.module ~
utilities.hotkey utilities.description utilities.securityLevel ~
utilities.notes 
&Scoped-define DISPLAYED-TABLES utilities
&Scoped-define FIRST-DISPLAYED-TABLE utilities


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
DEFINE BUTTON btnRun 
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Run" 
     SIZE 7 BY 1.91 TOOLTIP "Run this Utility".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 9.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnRun AT ROW 7.67 COL 7
     utilities.programName AT ROW 1.24 COL 18 COLON-ALIGNED HELP
          "Enter Utility Name"
          LABEL "Program Name" FORMAT "x(32)"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     utilities.module AT ROW 1.24 COL 65 COLON-ALIGNED HELP
          "Enter Module Code"
          LABEL "Module" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     utilities.hotkey AT ROW 1.24 COL 86 COLON-ALIGNED
          LABEL "Hot Key" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     utilities.description AT ROW 2.43 COL 18 COLON-ALIGNED FORMAT "x(48)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     utilities.securityLevel AT ROW 2.43 COL 86 COLON-ALIGNED HELP
          ""
          LABEL "Sec. Lvl" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     utilities.notes AT ROW 3.62 COL 20 NO-LABEL WIDGET-ID 2
          VIEW-AS EDITOR MAX-CHARS 245
          SIZE 76 BY 5.95
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.utilities
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
         HEIGHT             = 9.14
         WIDTH              = 100.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i}*/

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

/* SETTINGS FOR FILL-IN utilities.description IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN utilities.hotkey IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN utilities.module IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN utilities.programName IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN utilities.securityLevel IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
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

&Scoped-define SELF-NAME btnRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRun V-table-Win
ON CHOOSE OF btnRun IN FRAME F-Main /* Run */
DO:
    IF NOT AVAILABLE utilities THEN 
        RETURN NO-APPLY.
    IF SEARCH('util/' + utilities.programName) NE ? THEN
        RUN VALUE('util/' + utilities.programName).
    ELSE IF SEARCH('util/' + utilities.programName + '.r') NE ? THEN
        RUN VALUE('util/' + utilities.programName + '.r').
    ELSE MESSAGE 
        'Program: util/' + utilities.programName + ' does not exist!' VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME utilities.hotkey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL utilities.hotkey V-table-Win
ON LEAVE OF utilities.hotkey IN FRAME F-Main /* Hot Key */
DO:
    IF LASTKEY NE -1 
    OR SELF:SCREEN-VALUE NE "" THEN ASSIGN 
        SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME utilities.module
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL utilities.module V-table-Win
ON LEAVE OF utilities.module IN FRAME F-Main /* Module */
DO:
    IF LASTKEY NE -1 
    OR SELF:SCREEN-VALUE NE "" THEN ASSIGN 
        SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME utilities.programName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL utilities.programName V-table-Win
ON LEAVE OF utilities.programName IN FRAME F-Main /* Program Name */
DO:
    IF INDEX(SELF:SCREEN-VALUE,'util/') NE 0 THEN ASSIGN 
        SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE,'util/','').
    IF SEARCH('util/' + SELF:SCREEN-VALUE) EQ ? 
    AND SEARCH('util/' + SELF:SCREEN-VALUE + ".r") EQ ? THEN DO:
        MESSAGE 
            "This program does not exist in the /util directory." SKIP 
            "Please correct this condition immediately."
            VIEW-AS ALERT-BOX WARNING.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME utilities.securityLevel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL utilities.securityLevel V-table-Win
ON ENTRY OF utilities.securityLevel IN FRAME F-Main /* Sec. Lvl */
DO:
    ASSIGN 
        iBaseLevel = INTEGER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL utilities.securityLevel V-table-Win
ON LEAVE OF utilities.securityLevel IN FRAME F-Main /* Sec. Lvl */
DO:
    IF INTEGER(SELF:SCREEN-VALUE) GT iSecurityLevel THEN DO:
        MESSAGE 
            "You may not increase the Security Level for a Utility" SKIP 
            "to be greater than your own Security Level.  Please" SKIP 
            "contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFO.
        ASSIGN 
            SELF:SCREEN-VALUE = STRING(iBaseLevel).
        RETURN NO-APPLY.
    END.
    IF INTEGER(SELF:SCREEN-VALUE) GT 1000 THEN DO:
        MESSAGE 
            "You may not increase the Security Level for a Utility to be greater than 1000."
            VIEW-AS ALERT-BOX INFO.
        ASSIGN 
            SELF:SCREEN-VALUE = STRING(utilities.securityLevel).
        RETURN NO-APPLY.
    END.
    
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
  {src/adm/template/row-list.i "utilities"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "utilities"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    IF iSecurityLevel LT 1000 THEN DO:
        MESSAGE 
            "You do not have sufficient permissions to add a utility." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFO.
        RETURN.
    END. 
        
    ASSIGN
        lAddRecord = TRUE.
           
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            utilities.securityLevel:SCREEN-VALUE in frame {&frame-name} = "900".
        APPLY 'entry' TO utilities.programName.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ).

    ASSIGN
        lAddRecord = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF iSecurityLevel LT 1000 THEN DO:
        MESSAGE 
            "You do not have sufficient permissions to add a utility." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFO.
        RETURN.
    END. 

    ASSIGN
        lAddRecord = TRUE.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

    DO WITH FRAME {&FRAME-NAME}:
        APPLY 'entry' TO utilities.programName.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR thisOne AS CHAR NO-UNDO.
    DEFINE BUFFER buff-cust FOR cust .

    IF iSecurityLevel LT 1000 THEN DO:
        MESSAGE 
            "You do not have sufficient permissions to add a utility." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFO.
        RETURN.
    END. 

    IF NOT adm-new-record THEN DO:
        {custom/askdel.i}
    END.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win
PROCEDURE local-disable-fields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"} 
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    IF NOT lAddRecord THEN ASSIGN 
        utilities.programName:SENSITIVE IN FRAME f-main = FALSE.
    IF iSecurityLevel LT utilities.securityLevel THEN ASSIGN 
        utilities.securityLevel:SENSITIVE = FALSE.
    IF iSecurityLevel LT 1000 THEN ASSIGN 
        utilities.module:SENSITIVE = FALSE 
        utilities.hotkey:SENSITIVE = FALSE 
        utilities.description:SENSITIVE = FALSE
        utilities.notes:SENSITIVE = FALSE.  
    ASSIGN 
        iBaseLevel = INTEGER(utilities.securityLevel:SCREEN-VALUE).
                      
    {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
        
    IF INTEGER(utilities.securityLevel:SCREEN-VALUE IN FRAME f-main) GT iSecurityLevel THEN DO:
        MESSAGE 
            "You may not increase the Security Level for a Utility" SKIP 
            "to be greater than your own Security Level.  Please" SKIP 
            "contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFO.
        ASSIGN 
            utilities.securityLevel:SCREEN-VALUE = STRING(iBaseLevel).
        RETURN NO-APPLY.
    END.
    IF INTEGER(utilities.securityLevel:SCREEN-VALUE) GT 1000 THEN DO:
        MESSAGE 
            "You may not increase the Security Level for a Utility to be greater than 1000."
            VIEW-AS ALERT-BOX INFO.
        ASSIGN 
            utilities.securityLevel:SCREEN-VALUE = STRING(iBaseLevel).
        RETURN NO-APPLY.
    END.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    
    ASSIGN
        lAddRecord = FALSE.

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
  {src/adm/template/snd-list.i "utilities"}

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

