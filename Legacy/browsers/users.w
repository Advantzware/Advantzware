&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/users.w

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

&SCOPED-DEFINE setBrowseFocus
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE SortByPhrase (IF browse-order:SCREEN-VALUE IN FRAME F-Main = "1" THEN users.user_id ELSE users.user_name)
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
DEF BUFFER zUsers for users.
ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-define INTERNAL-TABLES users usr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table users.user_id users.user_name users.phone users.image_filename users.userType users.securityLevel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table   
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH users WHERE {&KEY-PHRASE} AND     users.user_id = userid(ldbname(1)) OR     zUsers.securityLevel GE 1000 OR     (zUsers.securityLevel GE 700 AND users.user_id NE "ASI") NO-LOCK, ~
               FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK         BY {&SortByPhrase}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH users WHERE {&KEY-PHRASE} AND     users.user_id = userid(ldbname(1)) OR     zUsers.securityLevel GE 1000 OR     (zUsers.securityLevel GE 700 AND users.user_id NE "ASI") NO-LOCK, ~
               FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK         BY {&SortByPhrase}.
&Scoped-define TABLES-IN-QUERY-Browser-Table users usr
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table users
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table usr


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 61 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      users, 
      usr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      users.user_id COLUMN-LABEL "User ID" FORMAT "X(8)":U
      users.user_name FORMAT "X(30)":U WIDTH 34.8
      users.phone FORMAT "x(12)":U WIDTH 18
      users.image_filename COLUMN-LABEL "Email" FORMAT "X(40)":U
            WIDTH 39.4
      users.userType FORMAT "x(20)":U WIDTH 18.2
      users.securityLevel COLUMN-LABEL "Sec Lvl" FORMAT ">999":U
            WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146 BY 22.86
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 24.1 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 24.1 COL 76 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 24.1 COL 133 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 24.1 COL 2
     RECT-4 AT ROW 23.86 COL 1
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
         HEIGHT             = 24.29
         WIDTH              = 146.
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH users WHERE {&KEY-PHRASE} AND
    users.user_id = userid(ldbname(1)) OR
    zUsers.securityLevel GE 1000 OR
    (zUsers.securityLevel GE 700 AND users.user_id NE "ASI") NO-LOCK,
        FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
        BY {&SortByPhrase}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, FIRST USED"
     _Where[1]         = "users.user_id = userid(ldbname(1)) or
zUsers.securityLevel GE 1000 OR
(zUsers.securityLevel GE 700 AND users.securityLevel LE zUsers.securityLevel)"
     _JoinCode[2]      = "usr.uid EQ users.user_id"
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

&Scoped-define SELF-NAME auto_find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL auto_find B-table-Win
ON VALUE-CHANGED OF auto_find IN FRAME F-Main /* Auto Find */
DO:
    CASE browse-order:SCREEN-VALUE:
        WHEN "1" THEN DO:
            IF zUsers.user_id = "ASI" THEN DO:
                OPEN QUERY browser-table FOR EACH users WHERE
                    (IF INDEX(SELF:SCREEN-VALUE,'*') EQ 0 THEN
                        users.user_id BEGINS SELF:SCREEN-VALUE
                    ELSE
                        (users.user_id MATCHES SELF:SCREEN-VALUE + "*" OR SELF:SCREEN-VALUE EQ "")) NO-LOCK,
                    FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
                    {&SORTBY-PHRASE}.
            END.
            ELSE IF zUsers.securityLevel GE 700 THEN DO:
                OPEN QUERY browser-table FOR EACH users WHERE
                    users.user_id NE "asi" AND
                    (IF INDEX(SELF:SCREEN-VALUE,'*') EQ 0 THEN
                        users.user_id BEGINS SELF:SCREEN-VALUE
                    ELSE
                        (users.user_id MATCHES SELF:SCREEN-VALUE + "*" OR SELF:SCREEN-VALUE EQ "")) NO-LOCK,
                    FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
                    {&SORTBY-PHRASE}.
            END.
            ELSE DO:
                OPEN QUERY browser-table FOR EACH users WHERE
                    users.user_id EQ zUsers.user_id NO-LOCK,
                    FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
                    {&SORTBY-PHRASE}.
            END.
        END.
        WHEN "2" THEN DO:
            IF zUsers.user_id = "ASI" THEN DO:
                OPEN QUERY browser-table FOR EACH users WHERE
                    (IF INDEX(SELF:SCREEN-VALUE,'*') EQ 0 THEN
                        users.user_name BEGINS SELF:SCREEN-VALUE
                    ELSE
                        (users.user_name MATCHES SELF:SCREEN-VALUE + "*" OR SELF:SCREEN-VALUE EQ "")) NO-LOCK,
                    FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
                    {&SORTBY-PHRASE}.
            END.
            ELSE IF zUsers.securityLevel GE 700 THEN DO:
                OPEN QUERY browser-table FOR EACH users WHERE
                    users.user_id NE "asi" AND
                    (IF INDEX(SELF:SCREEN-VALUE,'*') EQ 0 THEN
                        users.user_name BEGINS SELF:SCREEN-VALUE
                    ELSE
                        (users.user_name MATCHES SELF:SCREEN-VALUE + "*" OR SELF:SCREEN-VALUE EQ "")) NO-LOCK,
                    FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
                    {&SORTBY-PHRASE}.
            END.
            ELSE DO:
                OPEN QUERY browser-table FOR EACH users WHERE
                    users.user_name EQ zUsers.user_name NO-LOCK,
                    FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
                    {&SORTBY-PHRASE}.
            END.
        END.
    END CASE.
    ASSIGN
        auto_find.

    IF QUERY browser-table:NUM-RESULTS EQ 0 THEN DO:
        MESSAGE
            "No entries match that pattern."
            VIEW-AS ALERT-BOX.
        OPEN QUERY browser-table FOR EACH users WHERE 
            users.user_id = userid(ldbname(1)) OR
            zUsers.securityLevel GE 1000 OR
            (zUsers.securityLevel GE 700 AND users.user_id NE "ASI") NO-LOCK,
                FIRST usr WHERE usr.uid EQ users.user_id NO-LOCK
                BY {&SortByPhrase}.
        ASSIGN
            SELF:SCREEN-VALUE = ""
            auto_find = "".
    END.                
        
    APPLY 'value-changed' TO browser-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ANY-PRINTABLE OF Browser-Table IN FRAME F-Main
DO:
    IF LENGTH(KEYFUNCTION(lastkey)) = 1 THEN ASSIGN
        auto_find:SCREEN-VALUE IN FRAME {&FRAME-NAME} = auto_find:SCREEN-VALUE + KEYFUNCTION(lastkey).
    ELSE IF KEYFUNCTION(lastkey) = "BACKSPACE" THEN ASSIGN
        auto_find:SCREEN-VALUE = SUBSTRING(auto_find:SCREEN-VALUE,1,LENGTH(auto_find:SCREEN-VALUE) - 1).
    APPLY 'value-changed' TO auto_find.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

FIND zUsers NO-LOCK WHERE
    zUsers.user_id = USERID(LDBNAME(1))
    NO-ERROR.
IF NOT AVAIL zUsers THEN DO:
    MESSAGE
        "You are not logged in as a valid user."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.    
    
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR From-user AS CHAR NO-UNDO.
    DEF VAR To-user AS CHAR NO-UNDO.
    
    IF users.user_id NE "" THEN
    ASSIGN
        From-user = users.user_id
        To-user   = users.user_id
        . 
    
    RUN windows/user-exp.w (From-user,To-user).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGoBack B-table-Win 
PROCEDURE ipGoBack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    reposition browser-table backwards 1.
    apply 'value-changed' to browser-table in frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReposition B-table-Win 
PROCEDURE ipReposition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER iprCurrRowid AS ROWID.
    REPOSITION browser-table TO ROWID iprCurrRowid.
    apply 'value-changed' to browser-table in frame {&frame-name}.
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
  {src/adm/template/snd-list.i "users"}
  {src/adm/template/snd-list.i "usr"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setbrowsefocus B-table-Win 
PROCEDURE setbrowsefocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    apply 'entry' to browser-table in frame {&frame-name}.
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

