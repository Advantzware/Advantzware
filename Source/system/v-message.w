&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: Util/v-message.w

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
DEFINE VARIABLE lAddRecord     AS LOG     NO-UNDO.
DEFINE VARIABLE iSecurityLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE iBaseLevel     AS INTEGER NO-UNDO.

{custom/gcompany.i}

FIND FIRST users NO-LOCK WHERE 
    users.user_id EQ USERID(LDBNAME(1)) 
    NO-ERROR.
IF AVAILABLE users THEN ASSIGN 
        iSecurityLevel = users.securityLevel.

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
&Scoped-define EXTERNAL-TABLES zMessage
&Scoped-define FIRST-EXTERNAL-TABLE zMessage


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR zMessage.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS zMessage.msgID zMessage.module ~
zMessage.hotKey zMessage.msgName zMessage.currSecLevel ~
zMessage.defaultTitle zMessage.defaultMsg zMessage.msgType ~
zMessage.userSuppress zMessage.currentTitle zMessage.logMessage ~
zMessage.currMessage zMessage.displayOptions zMessage.rtnValue ~
zMessage.options zMessage.contextParms 
&Scoped-define ENABLED-TABLES zMessage
&Scoped-define FIRST-ENABLED-TABLE zMessage
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Test
&Scoped-Define DISPLAYED-FIELDS zMessage.msgID zMessage.module ~
zMessage.hotKey zMessage.msgName zMessage.currSecLevel ~
zMessage.defaultTitle zMessage.defaultMsg zMessage.msgType ~
zMessage.userSuppress zMessage.currentTitle zMessage.logMessage ~
zMessage.currMessage zMessage.displayOptions zMessage.rtnValue ~
zMessage.options zMessage.contextParms 
&Scoped-define DISPLAYED-TABLES zMessage
&Scoped-define FIRST-DISPLAYED-TABLE zMessage


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
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  ROUNDED 
     SIZE 123 BY 14.29.

DEFINE BUTTON Btn_Test
    LABEL "&Test" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     zMessage.msgID AT ROW 1.24 COL 23.6 COLON-ALIGNED HELP
          "Enter Utility Name"
          LABEL "Message Id" FORMAT "x(32)"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          BGCOLOR 15
     zMessage.module AT ROW 1.24 COL 70.6 COLON-ALIGNED HELP
          "Enter Module Code"
          LABEL "Module" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15
     zMessage.hotKey AT ROW 1.24 COL 96 COLON-ALIGNED
          LABEL "Hot Key" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15
     zMessage.msgName AT ROW 2.43 COL 23.6 COLON-ALIGNED FORMAT "x(48)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
          BGCOLOR 15
     zMessage.currSecLevel AT ROW 2.43 COL 96 COLON-ALIGNED HELP
          ""
          LABEL "Sec. Lvl" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15
     zMessage.defaultTitle AT ROW 3.62 COL 23.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55.4 BY 1
          BGCOLOR 15
     zMessage.defaultMsg AT ROW 4.81 COL 23.8 COLON-ALIGNED
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 55.2 BY 2.24
          BGCOLOR 15
     zMessage.msgType AT ROW 4.91 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15
     zMessage.userSuppress AT ROW 6.14 COL 98
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
          BGCOLOR 15
     zMessage.currentTitle AT ROW 7.24 COL 23.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55.4 BY 1
          BGCOLOR 15
     zMessage.logMessage AT ROW 7.38 COL 98
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
          BGCOLOR 15
     zMessage.currMessage AT ROW 8.38 COL 23.6 COLON-ALIGNED
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 70 BY 3.1
          BGCOLOR 15
     zMessage.displayOptions AT ROW 8.62 COL 98
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
          BGCOLOR 15
     zMessage.rtnValue AT ROW 11.62 COL 23.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55 BY 1
          BGCOLOR 15
     zMessage.options AT ROW 12.71 COL 23.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55 BY 1
          BGCOLOR 15
     zMessage.contextParms AT ROW 13.95 COL 23.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 55 BY 1
          BGCOLOR 15
    Btn_Test AT ROW 13.95 COL 100
     RECT-1 AT ROW 1.05 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.zMessage
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
         HEIGHT             = 14.57
         WIDTH              = 124.4.
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

/* SETTINGS FOR FILL-IN zMessage.currSecLevel IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN zMessage.hotKey IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN zMessage.module IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN zMessage.msgID IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN zMessage.msgName IN FRAME F-Main
   EXP-FORMAT                                                           */
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

&Scoped-define SELF-NAME zMessage.currSecLevel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zMessage.currSecLevel V-table-Win
ON ENTRY OF zMessage.currSecLevel IN FRAME F-Main /* Sec. Lvl */
DO:
        ASSIGN 
            iBaseLevel = INTEGER(SELF:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zMessage.currSecLevel V-table-Win
ON LEAVE OF zMessage.currSecLevel IN FRAME F-Main /* Sec. Lvl */
DO:
        IF INTEGER(SELF:SCREEN-VALUE) GT iSecurityLevel THEN 
        DO:
            MESSAGE 
                "You may not increase the Security Level for a Utility" SKIP 
                "to be greater than your own Security Level.  Please" SKIP 
                "contact Advantzware Support for assistance."
                VIEW-AS ALERT-BOX INFORMATION.
            ASSIGN 
                SELF:SCREEN-VALUE = STRING(iBaseLevel).
            RETURN NO-APPLY.
        END.
        IF INTEGER(SELF:SCREEN-VALUE) GT 1000 THEN 
        DO:
            MESSAGE 
                "You may not increase the Security Level for a Utility to be greater than 1000."
                VIEW-AS ALERT-BOX INFORMATION.
            ASSIGN 
                SELF:SCREEN-VALUE = STRING(zMessage.currSecLevel).
            RETURN NO-APPLY.
        END.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME zMessage.hotKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zMessage.hotKey V-table-Win
ON LEAVE OF zMessage.hotKey IN FRAME F-Main /* Hot Key */
DO:
        IF LASTKEY NE -1 
            OR SELF:SCREEN-VALUE NE "" THEN ASSIGN 
                SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME zMessage.module
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zMessage.module V-table-Win
ON LEAVE OF zMessage.module IN FRAME F-Main /* Module */
DO:
        IF LASTKEY NE -1 
            OR SELF:SCREEN-VALUE NE "" THEN ASSIGN 
                SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Test V-table-Win
ON CHOOSE OF Btn_Test IN FRAME F-Main /* Test */
    DO:
    IF AVAIL zMessage AND zMessage.currMessage NE "" THEN
        MESSAGE zMessage.currMessage VIEW-AS ALERT-BOX INFO .
    ELSE IF AVAIL zMessage  THEN
        MESSAGE zMessage.defaultMsg VIEW-AS ALERT-BOX INFO .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME zMessage.msgID
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
  {src/adm/template/row-list.i "zMessage"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "zMessage"}

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
    DEFINE BUFFER bf-zMessage FOR zMessage.
    DEFINE VARIABLE li-next-num AS INTEGER NO-UNDO.

    IF iSecurityLevel LT 1000 THEN 
    DO:
        MESSAGE 
            "You do not have sufficient permissions to add a utility." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFORMATION.
        RETURN.
    END. 
        
    ASSIGN
        lAddRecord = TRUE.
           
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

   

    
    FIND LAST bf-zMessage NO-LOCK NO-ERROR.
    IF AVAILABLE bf-zMessage THEN li-next-num = INTEGER(bf-zMessage.msgID) + 1.
    ELSE li-next-num = 1.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            zMessage.msgID:SCREEN-VALUE IN FRAME {&frame-name} = STRING(li-next-num).
        zMessage.currSecLevel:SCREEN-VALUE IN FRAME {&frame-name} = "900".
        APPLY 'entry' TO zMessage.msgID.
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
    IF iSecurityLevel LT 1000 THEN 
    DO:
        MESSAGE 
            "You do not have sufficient permissions to add a utility." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFORMATION.
        RETURN.
    END. 

    ASSIGN
        lAddRecord = TRUE.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

    DO WITH FRAME {&FRAME-NAME}:
        APPLY 'entry' TO zMessage.msgID.
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
    DEFINE BUFFER bf-zMessage FOR zMessage.
    DEFINE VARIABLE li-next-num AS INTEGER NO-UNDO.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
  
    FIND LAST bf-zMessage NO-LOCK NO-ERROR.
    IF AVAILABLE bf-zMessage THEN li-next-num = INTEGER(bf-zMessage.msgID) + 1.
    ELSE li-next-num = 1.
  
    zMessage.msgID = STRING(li-next-num).
    DISPLAY zMessage.msgID WITH FRAME {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE thisOne AS CHARACTER NO-UNDO.
    DEFINE BUFFER buff-cust FOR cust .

    IF iSecurityLevel LT 1000 THEN 
    DO:
        MESSAGE 
            "You do not have sufficient permissions to add a utility." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFORMATION.
        RETURN.
    END. 

    IF NOT adm-new-record THEN 
    DO:
        {custom/askdel.i}
    END.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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
            zMessage.msgID:SENSITIVE IN FRAME f-main = FALSE.
    IF iSecurityLevel LT zMessage.currSecLevel THEN ASSIGN 
            zMessage.currSecLevel:SENSITIVE = FALSE.
    IF iSecurityLevel LT 1000 THEN ASSIGN 
            zMessage.module:SENSITIVE      = FALSE 
            zMessage.hotkey:SENSITIVE      = FALSE 
            zMessage.msgName:SENSITIVE     = FALSE .  
    ASSIGN 
        iBaseLevel = INTEGER(zMessage.currSecLevel:SCREEN-VALUE).  

  DEFINE VARIABLE hPgmSecurity AS HANDLE  NO-UNDO.
  DEFINE VARIABLE lResult      AS LOG     NO-UNDO.
  
  RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
        RUN epCanAccess IN hPgmSecurity ("windows/message.w", "", OUTPUT lResult).
        DELETE OBJECT hPgmSecurity.
        IF NOT lResult THEN DO:
            DO WITH FRAME {&FRAME-NAME}:
            DISABLE  zMessage.msgID zMessage.module 
                zMessage.hotKey zMessage.msgName zMessage.currSecLevel 
                zMessage.defaultTitle zMessage.defaultMsg zMessage.msgType 
                zMessage.logMessage zMessage.rtnValue
                zMessage.options zMessage.contextParms  .
            END.
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
        
    IF INTEGER(zMessage.currSecLevel:SCREEN-VALUE IN FRAME f-main) GT iSecurityLevel THEN 
    DO:
        MESSAGE 
            "You may not increase the Security Level for a Utility" SKIP 
            "to be greater than your own Security Level.  Please" SKIP 
            "contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFORMATION.
        ASSIGN 
            zMessage.currSecLevel:SCREEN-VALUE = STRING(iBaseLevel).
        RETURN NO-APPLY.
    END.
    IF INTEGER(zMessage.currSecLevel:SCREEN-VALUE) GT 1000 THEN 
    DO:
        MESSAGE 
            "You may not increase the Security Level for a Utility to be greater than 1000."
            VIEW-AS ALERT-BOX INFORMATION.
        ASSIGN 
            zMessage.currSecLevel:SCREEN-VALUE = STRING(iBaseLevel).
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
  {src/adm/template/snd-list.i "zMessage"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-buttons B-table-Win 
PROCEDURE valid-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-add AS LOG INIT YES NO-UNDO.
  DEF OUTPUT PARAM op-del AS LOG INIT YES NO-UNDO.

  DEFINE VARIABLE hPgmSecurity AS HANDLE  NO-UNDO.
  DEFINE VARIABLE lResult      AS LOG     NO-UNDO.

  RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
        RUN epCanAccess IN hPgmSecurity ("windows/message.w", "", OUTPUT lResult).
        DELETE OBJECT hPgmSecurity.
        IF NOT lResult THEN 
            ASSIGN op-add = NO
                   op-del = NO .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
