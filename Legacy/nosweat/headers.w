&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: headers.w

  Description: Set Audit Table Selections

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 11/03/98

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE field-string AS CHARACTER NO-UNDO.
DEFINE VARIABLE data-type AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS table-names db-names field-names ~
Btn_Add_Label Btn_Add_Field Btn_Add_Dash Btn_Add_All Btn_Add_WO_Dash ~
header-include Btn_Open Btn_Clear Btn_Save Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS include_name table-names db-names ~
field-names header-include 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add_All 
     LABEL "Add &All" 
     SIZE 16 BY 1.19.

DEFINE BUTTON Btn_Add_Dash 
     LABEL "Add &Dash" 
     SIZE 16 BY 1.19.

DEFINE BUTTON Btn_Add_Field 
     LABEL "Add &Field" 
     SIZE 16 BY 1.19.

DEFINE BUTTON Btn_Add_Label 
     LABEL "Add &Label" 
     SIZE 16 BY 1.19.

DEFINE BUTTON Btn_Add_WO_Dash 
     LABEL "Add &W/O Dash" 
     SIZE 16 BY 1.19.

DEFINE BUTTON Btn_Cancel DEFAULT 
     LABEL "&Cancel" 
     SIZE 16 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Clear 
     LABEL "Clea&r" 
     SIZE 16 BY 1.24.

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 16 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Open 
     LABEL "&Open" 
     SIZE 16 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Save AUTO-GO DEFAULT 
     LABEL "&Save" 
     SIZE 16 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE VARIABLE header-include AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 74 BY 6.91 NO-UNDO.

DEFINE VARIABLE include_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE db-names AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.91 NO-UNDO.

DEFINE VARIABLE field-names AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.91 NO-UNDO.

DEFINE VARIABLE table-names AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     include_name AT ROW 1 COL 74 COLON-ALIGNED NO-LABEL
     table-names AT ROW 2.14 COL 26 HELP
          "Select Table Name" NO-LABEL
     db-names AT ROW 2.19 COL 1 HELP
          "Select Database Name" NO-LABEL
     field-names AT ROW 2.19 COL 51 HELP
          "Select Field Name" NO-LABEL
     Btn_Add_Label AT ROW 2.19 COL 76 HELP
          "Add Label to Header Include"
     Btn_Add_Field AT ROW 3.62 COL 76 HELP
          "Add Field to Header Include"
     Btn_Add_Dash AT ROW 5.05 COL 76 HELP
          "Add Dash to Header Include"
     Btn_Add_All AT ROW 6.48 COL 76 HELP
          "Add Label, Field, and Dash to Header Include"
     Btn_Add_WO_Dash AT ROW 7.91 COL 76 HELP
          "Add Label and Field to Header Include"
     header-include AT ROW 9.33 COL 1 HELP
          "Use only single-quote characters with header include" NO-LABEL
     Btn_Open AT ROW 9.33 COL 76 HELP
          "Use this function to OPEN header include"
     Btn_Clear AT ROW 10.76 COL 76 HELP
          "Use this function to CLEAR header include"
     Btn_Save AT ROW 12.19 COL 76 HELP
          "Use this function to SAVE header include"
     Btn_OK AT ROW 13.62 COL 76 HELP
          "Use this function to ACCEPT save and close header include"
     Btn_Cancel AT ROW 15.05 COL 76 HELP
          "Use this function to CANCEL header include"
     "Database Names" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 1 COL 3
          FONT 6
     "Table Names" VIEW-AS TEXT
          SIZE 15.6 BY 1 AT ROW 1 COL 29
          FONT 6
     "Field Names" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1 COL 54
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92 BY 15.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Headers Update/Create"
         HEIGHT             = 15.52
         WIDTH              = 92
         MAX-HEIGHT         = 15.52
         MAX-WIDTH          = 92
         VIRTUAL-HEIGHT     = 15.52
         VIRTUAL-WIDTH      = 92
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN include_name IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Headers Update/Create */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Headers Update/Create */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_All
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_All C-Win
ON CHOOSE OF Btn_Add_All IN FRAME DEFAULT-FRAME /* Add All */
DO:
  APPLY "CHOOSE" TO Btn_Add_Label.
  APPLY "CHOOSE" TO Btn_Add_Field.
  APPLY "CHOOSE" TO Btn_Add_Dash.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Dash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Dash C-Win
ON CHOOSE OF Btn_Add_Dash IN FRAME DEFAULT-FRAME /* Add Dash */
DO:
  IF header-include:SCREEN-VALUE NE "" THEN
  ldummy = header-include:INSERT-STRING(" +").
  ldummy = header-include:INSERT-STRING(" ' - '").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Field C-Win
ON CHOOSE OF Btn_Add_Field IN FRAME DEFAULT-FRAME /* Add Field */
DO:
  data-type = "STRING".
  RUN Get_Procedure IN Persistent-Handle ("get_type.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc)
        (table-names:SCREEN-VALUE,field-names:SCREEN-VALUE,OUTPUT data-type).
  IF header-include:SCREEN-VALUE NE "" THEN
  ldummy = header-include:INSERT-STRING(" +").
  ASSIGN
    field-string = IF data-type = "STRING" THEN
        table-names:SCREEN-VALUE + "." + field-names:SCREEN-VALUE
        ELSE
        "STRING(" + table-names:SCREEN-VALUE + "." + field-names:SCREEN-VALUE + ")"
    ldummy = header-include:INSERT-STRING(" " + field-string).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Label C-Win
ON CHOOSE OF Btn_Add_Label IN FRAME DEFAULT-FRAME /* Add Label */
DO:
  IF header-include:SCREEN-VALUE NE "" THEN
  ldummy = header-include:INSERT-STRING(" + ").
  RUN Get_Procedure IN Persistent-Handle (INPUT "fld_lbls.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (table-names:SCREEN-VALUE,
                       field-names:SCREEN-VALUE,
                OUTPUT cdummy).
  ldummy = header-include:INSERT-STRING("'" + cdummy + ": '").
  {methods/nowait.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_WO_Dash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_WO_Dash C-Win
ON CHOOSE OF Btn_Add_WO_Dash IN FRAME DEFAULT-FRAME /* Add W/O Dash */
DO:
  APPLY "CHOOSE" TO Btn_Add_Label.
  APPLY "CHOOSE" TO Btn_Add_Field.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear C-Win
ON CHOOSE OF Btn_Clear IN FRAME DEFAULT-FRAME /* Clear */
DO:
  ASSIGN
    header-include:SCREEN-VALUE = ""
    include_name:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CHOOSE" TO Btn_Save.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Open C-Win
ON CHOOSE OF Btn_Open IN FRAME DEFAULT-FRAME /* Open */
DO:
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE include-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  init-dir = "methods\headers\".
  SYSTEM-DIALOG GET-FILE include-name
      TITLE      "Choose Include to OPEN ..."
      FILTERS    "Include Files (*.i)" "*.i",
                 "All Files (*.*)" "*.*"
      INITIAL-DIR init-dir
      MUST-EXIST
      USE-FILENAME
      UPDATE OKpressed.
  IF NOT OKpressed THEN
  RETURN NO-APPLY.
  ASSIGN
    ldummy = header-include:READ-FILE(include-name)
    include-name = SUBSTR(include-name,1,INDEX(include-name,".") - 1)
    include-name = SUBSTR(include-name,R-INDEX(include-name,"\") + 1)
    include_name:SCREEN-VALUE = LC(include-name) + ".i".
  DO i = 1 TO NUM-DBS:
    CREATE ALIAS dictdb FOR DATABASE VALUE(LDBNAME(i)).
    RUN Get_Procedure IN Persistent-Handle (INPUT "findfile.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (include-name,OUTPUT ldummy).
    {methods/nowait.i}
    IF ldummy THEN
    DO:
      db-names:SCREEN-VALUE = LDBNAME(i).
      RUN Get_Tables.
      table-names:SCREEN-VALUE = include-name.
      APPLY "VALUE-CHANGED" TO table-names.
      RETURN.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save C-Win
ON CHOOSE OF Btn_Save IN FRAME DEFAULT-FRAME /* Save */
DO:
  IF include_name:SCREEN-VALUE = "" THEN
  include_name:SCREEN-VALUE = table-names:SCREEN-VALUE + ".i".
  ldummy = header-include:SAVE-FILE("methods/headers/" + include_name:SCREEN-VALUE).
  Btn_Cancel:LABEL = "&Close".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME db-names
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL db-names C-Win
ON VALUE-CHANGED OF db-names IN FRAME DEFAULT-FRAME
DO:
  RUN Get_Tables.
  table-names:SCREEN-VALUE = ENTRY(1,table-names:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO table-names.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME table-names
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL table-names C-Win
ON VALUE-CHANGED OF table-names IN FRAME DEFAULT-FRAME
DO:
  RUN Get_Fields.
  field-names:SCREEN-VALUE = ENTRY(1,field-names:LIST-ITEMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN Get_DBs.
  db-names:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      db-names:ENTRY(1) IN FRAME {&FRAME-NAME}.
  APPLY "VALUE-CHANGED" TO db-names.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY include_name table-names db-names field-names header-include 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE table-names db-names field-names Btn_Add_Label Btn_Add_Field 
         Btn_Add_Dash Btn_Add_All Btn_Add_WO_Dash header-include Btn_Open 
         Btn_Clear Btn_Save Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_DBs C-Win 
PROCEDURE Get_DBs :
/* -----------------------------------------------------------
  Purpose: Populate db-names selection list with connected database names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE list-items AS CHARACTER NO-UNDO.
  
  RUN Get_Procedure IN Persistent-Handle (INPUT "db_list.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT list-items).
  db-names:LIST-ITEMS IN FRAME {&FRAME-NAME} = list-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Fields C-Win 
PROCEDURE Get_Fields :
/* -----------------------------------------------------------
  Purpose: Populate field-names selection list with field names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE list-items AS CHARACTER NO-UNDO.
  
  CREATE ALIAS dictdb FOR DATABASE VALUE(db-names:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (table-names:SCREEN-VALUE IN FRAME {&FRAME-NAME},OUTPUT list-items).
  field-names:LIST-ITEMS IN FRAME {&FRAME-NAME} = list-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Tables C-Win 
PROCEDURE Get_Tables :
/* -----------------------------------------------------------
  Purpose: Populate table-names selection list with table names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE list-items AS CHARACTER NO-UNDO.
  
  CREATE ALIAS dictdb FOR DATABASE VALUE(db-names:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN Get_Procedure IN Persistent-Handle (INPUT "filelist.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT list-items).
  table-names:LIST-ITEMS IN FRAME {&FRAME-NAME} = list-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i db-names}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


