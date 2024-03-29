&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/userLog.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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

&SCOPED-DEFINE winReSize
//&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}
DEFINE VARIABLE cReturnChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cLogoutFolder   AS CHARACTER NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEFINE BUFFER bUserLog FOR userlog.
DEFINE VARIABLE ppid AS INTEGER NO-UNDO.

DEFINE STREAM sLogOut.
RUN sys/ref/nk1look.p (INPUT g_company, "UserControl", "C" /* Character*/, 
    INPUT NO /* check by cust */, 
    INPUT YES /* use cust not vendor */,
    INPUT "" /* cust */, 
    INPUT "" /* ship-to*/,
    OUTPUT cReturnChar, 
    OUTPUT lRecFound).
IF lRecFound THEN 
    cLogoutFolder = cReturnChar  .
IF SEARCH( cLogoutFolder) EQ ? THEN 
    OS-CREATE-DIR VALUE( cLogoutFolder).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES userLog

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table userLog.user_id ~
userLog.userName userLog.mode userLog.userStatus userLog.LoginDateTime ~
userLog.asiUsrNo userLog.asiPID userLog.audUsrNo userLog.audPID ~
userLog.IpAddress userLog.sessionID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH userLog WHERE ~{&KEY-PHRASE} ~
      AND userLog.logoutDateTime = ? NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH userLog WHERE ~{&KEY-PHRASE} ~
      AND userLog.logoutDateTime = ? NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table userLog
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table userLog


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find btDelSelected btRefresh 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find fiTotal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btDelSelected 
     LABEL "Delete" 
     SIZE 12 BY 1.14.

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON btRefresh 
     LABEL "Refresh" 
     SIZE 12 BY 1.14.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotal AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Current" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 29 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      userLog
    FIELDS(userLog.user_id
      userLog.userName
      userLog.mode
      userLog.userStatus
      userLog.LoginDateTime
      userLog.asiUsrNo
      userLog.asiPID
      userLog.audUsrNo
      userLog.audPID
      userLog.IpAddress
      userLog.sessionID) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      userLog.user_id FORMAT "X(12)":U
      userLog.userName FORMAT "x(20)":U WIDTH 35.6
      userLog.mode FORMAT "x(12)":U WIDTH 16.6
      userLog.userStatus FORMAT "x(12)":U WIDTH 16.6
      userLog.LoginDateTime FORMAT "99/99/9999 HH:MM:SS AM":U
      userLog.asiUsrNo FORMAT ">>>>>>>9":U WIDTH 13.8
      userLog.asiPID FORMAT ">>>>>>>9":U
      userLog.audUsrNo FORMAT ">>>>>>>9":U WIDTH 14.2
      userLog.audPID FORMAT ">>>>>>>9":U
      userLog.IpAddress FORMAT "x(15)":U
      userLog.sessionID FORMAT ">>>>>>>>>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 139 BY 16.43
         FONT 2 ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.67 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 17.67 COL 44 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.67 COL 79 HELP
          "CLEAR AUTO FIND Value"
     fiTotal AT ROW 17.67 COL 101 COLON-ALIGNED
     btDelSelected AT ROW 17.67 COL 113 WIDGET-ID 2
     btRefresh AT ROW 17.67 COL 126
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.67 COL 2
     RECT-4 AT ROW 17.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.52
         WIDTH              = 139.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* SETTINGS FOR FILL-IN fiTotal IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiTotal:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.userLog"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "userLog.logoutDateTime = ?"
     _FldNameList[1]   = ASI.userLog.user_id
     _FldNameList[2]   > ASI.userLog.userName
"userName" ? "x(20)" "character" ? ? ? ? ? ? no ? no no "35.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.userLog.mode
"mode" ? ? "character" ? ? ? ? ? ? no ? no no "16.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.userLog.userStatus
"userStatus" ? "x(12)" "character" ? ? ? ? ? ? no ? no no "16.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.userLog.LoginDateTime
"LoginDateTime" ? "99/99/9999 HH:MM:SS AM" "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.userLog.asiUsrNo
"asiUsrNo" ? ? "integer" ? ? ? ? ? ? no ? no no "13.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = ASI.userLog.asiPID
     _FldNameList[8]   > ASI.userLog.audUsrNo
"audUsrNo" ? ? "integer" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = ASI.userLog.audPID
     _FldNameList[10]   = ASI.userLog.IpAddress
     _FldNameList[11]   = ASI.userLog.sessionID
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
    ASSIGN 
        iCtr = 0.
    FOR EACH bUserLog NO-LOCK WHERE 
        bUserLog.userStatus EQ "Logged In":
        ASSIGN 
            iCtr = iCtr + 1.
    END.
    ASSIGN 
        fiTotal:SCREEN-VALUE IN FRAME {&frame-name} = STRING(iCtr,">>>9").
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelSelected B-table-Win
ON CHOOSE OF btDelSelected IN FRAME F-Main /* Delete */
DO:
  DEF BUFFER bf-userLog FOR userLog.
  DEFINE VARIABLE li AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lAns AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE hPgmSecurity AS HANDLE NO-UNDO.
  DEFINE VARIABLE lResult AS LOG NO-UNDO.
        RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
        RUN epCanAccess IN hPgmSecurity ("browsers/userlog.w", "", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.

  IF NOT lResult THEN DO:
    MESSAGE "Please contact your administrator to delete session records."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.

  END.
    
  DO WITH FRAME {&frame-name}:
  
    MESSAGE "Delete selected session records?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE lAns.
    IF lAns THEN DO:
        IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN
      DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
        {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
        IF AVAIL userLog THEN DO:
            IF userlog.processId EQ ppid THEN DO:
                MESSAGE 
                    "You cannot disconnect your current session."
                    VIEW-AS ALERT-BOX WARNING.
                NEXT.
            END.
            
          RUN system/userLogout.p (YES, userLog.sessionID).
             
        END. /* if avail userlog */
      END. /* do li ... */
      RUN dispatch ('open-query').
    END. /* if lAns */
  END. /* do with frame ... */

    ASSIGN iCtr = 0.
    FOR EACH bUserLog NO-LOCK WHERE 
        bUserLog.userStatus EQ "Logged In":
        ASSIGN 
            iCtr = iCtr + 1.
    END.
    ASSIGN 
        fiTotal:SCREEN-VALUE IN FRAME {&frame-name} = STRING(iCtr,">>>9").

END. /* end trigger block */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRefresh B-table-Win
ON CHOOSE OF btRefresh IN FRAME F-Main /* Refresh */
DO:
    RUN dispatch ('open-query').

    ASSIGN iCtr = 0.
    FOR EACH bUserLog NO-LOCK WHERE 
        bUserLog.userStatus EQ "Logged In":
        ASSIGN 
            iCtr = iCtr + 1.
    END.
    ASSIGN 
        fiTotal:SCREEN-VALUE IN FRAME {&frame-name} = STRING(iCtr,">>>9").

END. /* end trigger block */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF
    RUN GetCurrentProcessID (OUTPUT ppid).
{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    ASSIGN 
        iCtr = 0.
    FOR EACH bUserLog NO-LOCK WHERE 
        bUserLog.userStatus EQ "Logged In":
        ASSIGN 
            iCtr = iCtr + 1.
    END.
    ASSIGN 
        fiTotal = iCtr.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "value-changed" TO BROWSE {&browse-name}.
  APPLY "entry" TO BROWSE {&browse-name}.
  IF USERID(LDBNAME(1)) NE "ASI" THEN
    DISABLE btDelSelected.


    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "userLog"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE GetCurrentProcessId EXTERNAL "kernel32.dll":
    /*------------------------------------------------------------------------------
     Purpose: Gets the current process ID of this session on the user's machine
     Notes:   Required by logout when user chooses "Log out other sessions"
    ------------------------------------------------------------------------------*/
    DEFINE RETURN PARAMETER ppid AS LONG NO-UNDO.

END PROCEDURE.
