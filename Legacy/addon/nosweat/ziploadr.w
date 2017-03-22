&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ziploadr.w

  Description: Zip Codes Loader

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 06/15/98

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

DEFINE VARIABLE save-zipcode AS CHARACTER NO-UNDO.
DEFINE VARIABLE save-pref_type AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE ttbl-zipcode NO-UNDO
  FIELD city AS CHARACTER FORMAT "x(30)"
  FIELD state AS CHARACTER FORMAT "xx"
  FIELD zipcode AS CHARACTER FORMAT "x(5)"
  FIELD area_code AS CHARACTER FORMAT "xxx"
  FIELD fips_code AS CHARACTER FORMAT "x(5)"
  FIELD county AS CHARACTER FORMAT "x(30)"
  FIELD pref_type AS CHARACTER FORMAT "x"
  FIELD time_zone AS CHARACTER FORMAT "x(5)"
  FIELD dst AS CHARACTER FORMAT "x"
    INDEX ttbl-zipcode IS PRIMARY UNIQUE
      zipcode pref_type DESCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS preferred Btn_Close approved Btn_OK ~
nonapproved 
&Scoped-Define DISPLAYED-OBJECTS preferred approved nonapproved 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 preferred approved Btn_OK nonapproved 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE approved AS LOGICAL INITIAL no 
     LABEL "Approved Alias" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE nonapproved AS LOGICAL INITIAL no 
     LABEL "Non-Approved Alias" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE preferred AS LOGICAL INITIAL yes 
     LABEL "Preferred" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     preferred AT ROW 2.19 COL 6 HELP
          "Select to Load Preferred Zip Codes"
     Btn_Close AT ROW 2.43 COL 41 HELP
          "CANCEL Zip Codes Loader"
     approved AT ROW 3.14 COL 6 HELP
          "Select to Load Approved Alias Zip Codes"
     Btn_OK AT ROW 3.86 COL 41 HELP
          "Load Selected Zip Code Types"
     nonapproved AT ROW 4.1 COL 6 HELP
          "Select to Load Non-Approved Alias Zip Codes"
     "Zip Code Types" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56 BY 4.29.


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
         TITLE              = "Zip Codes Loader"
         HEIGHT             = 4.29
         WIDTH              = 56
         MAX-HEIGHT         = 4.29
         MAX-WIDTH          = 56
         VIRTUAL-HEIGHT     = 4.29
         VIRTUAL-WIDTH      = 56
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   L-To-R, COLUMNS                                                      */
ASSIGN
       Btn_Close:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


/* SETTINGS FOR TOGGLE-BOX approved IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_OK IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX nonapproved IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX preferred IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME






/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Zip Codes Loader */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Zip Codes Loader */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  {methods/wait.i}
  CREATE ttbl-zipcode.
  INPUT FROM "zipcodes.txt" NO-ECHO.
  IMPORT ^.
  IMPORT ^.
  IMPORT ^.
  IMPORT ^.
  REPEAT WITH FRAME {&FRAME-NAME}:
    IMPORT DELIMITER "," ttbl-zipcode.
    IF (preferred:SCREEN-VALUE = "no" AND ttbl-zipcode.pref_type = "P") OR
       (approved:SCREEN-VALUE = "no" AND ttbl-zipcode.pref_type = "A") OR
       (nonapproved:SCREEN-VALUE = "no" AND ttbl-zipcode.pref_type = "N") THEN
    NEXT.
    IF ttbl-zipcode.zipcode NE save-zipcode AND
       ttbl-zipcode.pref_type NE save-pref_type THEN
    RUN Purge-ZipCodes.
    i = 1.
    DO WHILE CAN-FIND(zipcode
        WHERE zipcode.zipcode = ttbl-zipcode.zipcode
          AND zipcode.pref_type = ttbl-zipcode.pref_type
          AND zipcode.pref# = i):
      i = i + 1.
    END.
    CREATE zipcode.
    ASSIGN
      zipcode.zipcode = ttbl-zipcode.zipcode
      zipcode.pref_type = ttbl-zipcode.pref_type
      zipcode.pref# = i
      zipcode.city = ttbl-zipcode.city
      zipcode.state = ttbl-zipcode.state
      zipcode.area_code = INTEGER(ttbl-zipcode.area_code)
      zipcode.fips_code = INTEGER(ttbl-zipcode.fips_code)
      zipcode.county = ttbl-zipcode.county
      zipcode.time_zone = ttbl-zipcode.time_zone
      zipcode.dst = IF ttbl-zipcode.dst = "y" THEN yes ELSE no
      zipcode.country = "US".
  END.
  INPUT CLOSE.
  {methods/nowait.i}
  MESSAGE "Zip Codes Loader Complete!" VIEW-AS ALERT-BOX INFORMATION.
  APPLY "CLOSE" TO THIS-PROCEDURE.
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

ON ESC OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF SEARCH("zipcodes.txt") NE ? THEN
  DO:
    INPUT FROM "zipcodes.txt" NO-ECHO.
    IMPORT i.
    preferred:LABEL = preferred:LABEL + " (" + TRIM(STRING(i,">>>,>>9")) + ")".
    IMPORT i.
    approved:LABEL = approved:LABEL + " (" + TRIM(STRING(i,">>>,>>9")) + ")".
    IMPORT i.
    nonapproved:LABEL = nonapproved:LABEL + " (" + TRIM(STRING(i,">>>,>>9")) + ")".
    INPUT CLOSE.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    MESSAGE 'Zip Codes File ~"zipcodes.txt~" does not exist!' VIEW-AS ALERT-BOX.
    DISABLE {&LIST-1}.
  END.
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
  DISPLAY preferred approved nonapproved 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE preferred Btn_Close approved Btn_OK nonapproved 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Purge-ZipCodes C-Win 
PROCEDURE Purge-ZipCodes :
/*------------------------------------------------------------------------------
  Purpose:     Remove current zip codes before loading new codes.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    save-zipcode = ttbl-zipcode.zipcode
    save-pref_type = ttbl-zipcode.pref_type.
  FOR EACH zipcode
      WHERE zipcode.zipcode = save-zipcode
        AND zipcode.pref_type = save-pref_type
      EXCLUSIVE:
    DELETE zipcode.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus by calling program
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i Btn_Close}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


