&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

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

&SCOPED-DEFINE dataGridInclude dataGrid\addon\browsers\maillist.i
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

def var lv-is-first as log init yes no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES contact

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table contact.last-name ~
contact.first-name contact.cust-no contact.contact-title contact.cust-name ~
contact.ship-id contact.maillist 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table contact.maillist 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table contact
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table contact
&Scoped-define QUERY-STRING-Browser-Table FOR EACH contact WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH contact WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table contact
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table contact


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table is-maillist browse-order ~
auto_find Btn_Clear_Find RECT-4 
&Scoped-Define DISPLAYED-OBJECTS is-maillist browse-order auto_find 

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
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 76 BY 1 NO-UNDO.

DEFINE VARIABLE is-maillist AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Yes", yes,
"No", no
     SIZE 14 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 124 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      contact
    FIELDS(contact.last-name
      contact.first-name
      contact.cust-no
      contact.contact-title
      contact.cust-name
      contact.ship-id
      contact.maillist) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      contact.last-name FORMAT "X(15)":U
      contact.first-name FORMAT "X(15)":U
      contact.cust-no COLUMN-LABEL "Customer" FORMAT "X(8)":U
      contact.contact-title FORMAT "X(20)":U
      contact.cust-name FORMAT "X(20)":U
      contact.ship-id COLUMN-LABEL "Shipto" FORMAT "X(8)":U
      contact.maillist COLUMN-LABEL "Mail?" FORMAT "yes/no":U
  ENABLE
      contact.maillist
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 18.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     is-maillist AT ROW 10.52 COL 55 NO-LABEL
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 19.33 COL 91 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 111 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 19.1 COL 1
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19.33 COL 2
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
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       is-maillist:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "contact"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > contact.last-name
"contact.last-name" ? "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > contact.first-name
"contact.first-name" ? "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > contact.cust-no
"contact.cust-no" "Customer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > contact.contact-title
"contact.contact-title" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > contact.cust-name
"contact.cust-name" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > contact.ship-id
"contact.ship-id" "Shipto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > contact.maillist
"contact.maillist" "Mail?" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
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
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    def var char-val as cha no-undo.
    case focus:name:
         when 'list-name' then do:
              run windows/l-maillst.w (focus:screen-value, output char-val).
              if char-val <> "" then assign focus:screen-value = entry(1,char-val).
         end.
    end case.
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


&Scoped-define SELF-NAME contact.maillist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.maillist Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF contact.maillist IN BROWSE Browser-Table /* Mail? */
DO:
  /*  if lv-is-first then lv-is-first = no.  
    else assign is-maillist:screen-value in frame {&frame-name} = 
                if (string(contact.maillist:screen-value in browse {&browse-name})) = "yes" then "no" else "yes"
           .
    assign is-maillist:hidden in frame {&frame-name} = no
           is-maillist:sensitive = yes.
           is-maillist:row  = self:row + 1 /*contact.maillist:row in browse {&browse-name}*/
           .

           is-maillist:column = self:column /*contact.maillist:column in browse {&browse-name}*/

           .
     apply "entry" to is-maillist.
     return no-apply.      
   */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.maillist Browser-Table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-CLICK OF contact.maillist IN BROWSE Browser-Table /* Mail? */
DO:
   if self:screen-value = "Yes" then self:screen-value = "No" .
   else self:screen-value = "Yes".
   return no-apply. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME is-maillist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL is-maillist B-table-Win
ON LEAVE OF is-maillist IN FRAME F-Main
DO:

    apply "value-changed" to is-maillist in frame {&frame-name}.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL is-maillist B-table-Win
ON MOUSE-SELECT-CLICK OF is-maillist IN FRAME F-Main
DO:
   apply "value-changed" to is-maillist.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL is-maillist B-table-Win
ON VALUE-CHANGED OF is-maillist IN FRAME F-Main
DO:
    assign is-maillist.
    contact.maillist:screen-value in browse {&browse-name} = string(is-maillist).
   /* if is-maillist then apply "entry" to contact.list-name.
    else*/

     apply "row-leave" to browse {&browse-name}.

    self:hidden in frame {&frame-name} = yes.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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
  {src/adm/template/snd-list.i "contact"}

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

