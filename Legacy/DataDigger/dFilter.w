&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  Name: dFilter.w
  Desc: Let user select what files to show

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ DataDigger.i }

/* Parameters Definitions */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttTableFilter.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cbTableFieldShow cbTableFieldHide ~
cbTableNameShow cbTableNameHide tgShowNormalTables tgShowSchema tgShowVst ~
tgShowSql tgShowOther tgShowHidden tgShowFrozen btnReset Btn_OK Btn_Cancel ~
RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS cbTableFieldShow cbTableFieldHide ~
cbTableNameShow cbTableNameHide tgShowNormalTables tgShowSchema tgShowVst ~
tgShowSql tgShowOther tgShowHidden tgShowFrozen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnReset 
     LABEL "&Reset" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "reset all filters to the default value".

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE VARIABLE cbTableFieldHide AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN
     SIZE-PIXELS 475 BY 21 TOOLTIP "provide a comma separated list of (partial) field names" NO-UNDO.

DEFINE VARIABLE cbTableFieldShow AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN
     SIZE-PIXELS 475 BY 21 TOOLTIP "provide a comma separated list of (partial) field names" NO-UNDO.

DEFINE VARIABLE cbTableNameHide AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN
     SIZE-PIXELS 475 BY 21 TOOLTIP "provide a comma separated list of (partial) field names" NO-UNDO.

DEFINE VARIABLE cbTableNameShow AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN
     SIZE-PIXELS 475 BY 21 TOOLTIP "provide a comma separated list of (partial) field names" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 495 BY 125.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 495 BY 125.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 500 BY 118.

DEFINE VARIABLE tgShowFrozen AS LOGICAL INITIAL no 
     LABEL "Show &Frozen Application Tables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 245 BY 17 NO-UNDO.

DEFINE VARIABLE tgShowHidden AS LOGICAL INITIAL no 
     LABEL "Show &Hidden Application Tables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 245 BY 17 NO-UNDO.

DEFINE VARIABLE tgShowNormalTables AS LOGICAL INITIAL no 
     LABEL "Show &Normal Tables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 175 BY 17 TOOLTIP "show or hide normal application tables" NO-UNDO.

DEFINE VARIABLE tgShowOther AS LOGICAL INITIAL no 
     LABEL "Show Other &MetaTables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 245 BY 17 TOOLTIP "show or hide tables in other categories" NO-UNDO.

DEFINE VARIABLE tgShowSchema AS LOGICAL INITIAL no 
     LABEL "Show &Schema Tables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 175 BY 17 TOOLTIP "show or hide tables like _file / _field / _index etc" NO-UNDO.

DEFINE VARIABLE tgShowSql AS LOGICAL INITIAL no 
     LABEL "Show S&QL Tables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 175 BY 17 NO-UNDO.

DEFINE VARIABLE tgShowVst AS LOGICAL INITIAL no 
     LABEL "Show &VST Tables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 175 BY 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cbTableFieldShow AT Y 68 X 10 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     cbTableFieldHide AT Y 113 X 10 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cbTableNameShow AT Y 218 X 10 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     cbTableNameHide AT Y 263 X 10 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     tgShowNormalTables AT Y 335 X 20 WIDGET-ID 50
     tgShowSchema AT Y 358 X 20 WIDGET-ID 24
     tgShowVst AT Y 381 X 20 WIDGET-ID 28
     tgShowSql AT Y 406 X 20 WIDGET-ID 40
     tgShowOther AT Y 358 X 243 WIDGET-ID 44
     tgShowHidden AT Y 381 X 243 WIDGET-ID 30
     tgShowFrozen AT Y 406 X 243 WIDGET-ID 42
     btnReset AT Y 453 X 10 WIDGET-ID 46
     Btn_OK AT Y 450 X 345
     Btn_Cancel AT Y 450 X 425
     "But hide these" VIEW-AS TEXT
          SIZE-PIXELS 138 BY 18 AT Y 245 X 23 WIDGET-ID 20
     "But hide tables that contain any of these" VIEW-AS TEXT
          SIZE-PIXELS 350 BY 18 AT Y 95 X 23 WIDGET-ID 12
     "Show only tables that contain all of these fields" VIEW-AS TEXT
          SIZE-PIXELS 350 BY 18 AT Y 50 X 23 WIDGET-ID 6
     "Show these tables" VIEW-AS TEXT
          SIZE-PIXELS 163 BY 18 AT Y 200 X 23 WIDGET-ID 16
     "Filter by name" VIEW-AS TEXT
          SIZE-PIXELS 99 BY 18 AT Y 163 X 23 WIDGET-ID 36
     "Filter by fields in table" VIEW-AS TEXT
          SIZE-PIXELS 142 BY 18 AT Y 10 X 23 WIDGET-ID 38
     "Filter by Type" VIEW-AS TEXT
          SIZE-PIXELS 99 BY 18 AT Y 310 X 23 WIDGET-ID 48
     "Comma sep list, wildcards allowed" VIEW-AS TEXT
          SIZE-PIXELS 215 BY 18 AT Y 21 X 285 WIDGET-ID 52
          FGCOLOR 7 
     "Comma sep list, wildcards allowed" VIEW-AS TEXT
          SIZE-PIXELS 215 BY 18 AT Y 171 X 285 WIDGET-ID 54
          FGCOLOR 7 
     RECT-1 AT Y 170 X 10 WIDGET-ID 4
     RECT-2 AT Y 19 X 10 WIDGET-ID 22
     RECT-3 AT Y 320 X 10 WIDGET-ID 32
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 521 BY 514
         TITLE "Edit table filter"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Edit table filter */
DO:

  /* Save settings to ttTableFilter */
  ASSIGN
    ttTableFilter.lShowNormal     = tgShowNormalTables:CHECKED
    ttTableFilter.lShowSchema     = tgShowSchema:CHECKED
    ttTableFilter.lShowVst        = tgShowVst:CHECKED
    ttTableFilter.lShowSql        = tgShowSql:CHECKED
    ttTableFilter.lShowOther      = tgShowOther:CHECKED
    ttTableFilter.lShowHidden     = tgShowHidden:CHECKED
    ttTableFilter.lShowFrozen     = tgShowFrozen:CHECKED
    ttTableFilter.cTableNameShow  = cbTableNameShow :SCREEN-VALUE
    ttTableFilter.cTableNameHide  = cbTableNameHide :SCREEN-VALUE
    ttTableFilter.cTableFieldShow = cbTableFieldShow:SCREEN-VALUE
    ttTableFilter.cTableFieldHide = cbTableFieldHide:SCREEN-VALUE
    .

  /* Save the value in the combo boxes */
  RUN saveComboValue(cbTableNameShow:HANDLE , 'TableNameShow').
  RUN saveComboValue(cbTableNameHide:HANDLE , 'TableNameHide').
  RUN saveComboValue(cbTableFieldShow:HANDLE, 'TableFieldShow').
  RUN saveComboValue(cbTableFieldHide:HANDLE, 'TableFieldHide').

  /* Find out if user entered anything */
  ttTableFilter.lModified = (   NOT tgShowNormalTables:CHECKED     
                             OR tgShowSchema:CHECKED          
                             OR tgShowVst:CHECKED             
                             OR tgShowSql:CHECKED             
                             OR tgShowOther:CHECKED           
                             OR tgShowHidden:CHECKED          
                             OR tgShowFrozen:CHECKED          
                             OR cbTableNameShow :SCREEN-VALUE <> ?
                             OR cbTableNameHide :SCREEN-VALUE <> ? 
                             OR cbTableFieldShow:SCREEN-VALUE <> ?
                             OR cbTableFieldHide:SCREEN-VALUE <> ?).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Edit table filter */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:

  RUN initTableFilter(INPUT-OUTPUT TABLE ttTableFilter).
  RUN initializeObject.
  APPLY 'CHOOSE' TO Btn_OK IN FRAME Dialog-Frame.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* ASSIGN cbFieldFilter = pcFilter. */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  RUN initializeObject.

  WAIT-FOR GO OF FRAME {&FRAME-NAME} FOCUS cbTableFieldShow.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY cbTableFieldShow cbTableFieldHide cbTableNameShow cbTableNameHide 
          tgShowNormalTables tgShowSchema tgShowVst tgShowSql tgShowOther 
          tgShowHidden tgShowFrozen 
      WITH FRAME Dialog-Frame.
  ENABLE cbTableFieldShow cbTableFieldHide cbTableNameShow cbTableNameHide 
         tgShowNormalTables tgShowSchema tgShowVst tgShowSql tgShowOther 
         tgShowHidden tgShowFrozen btnReset Btn_OK Btn_Cancel RECT-1 RECT-2 
         RECT-3 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Dialog-Frame 
PROCEDURE initializeObject :
/* Prepare window: set fonts and load values from registry for combo's
  */
  DO WITH FRAME {&FRAME-NAME}:

    /* Prepare frame */
    FRAME {&FRAME-NAME}:FONT = getFont('Default').

    /* Should have exactly 1 record */
    FIND ttTableFilter NO-ERROR.
    IF NOT AVAILABLE ttTableFilter THEN RETURN.

    /* Load settings from registry */
    tgShowNormalTables:CHECKED = ttTableFilter.lShowNormal.
    tgShowSchema:CHECKED       = ttTableFilter.lShowSchema.
    tgShowVst:CHECKED          = ttTableFilter.lShowVst.
    tgShowSql:CHECKED          = ttTableFilter.lShowSql.
    tgShowOther:CHECKED        = ttTableFilter.lShowOther.
    tgShowHidden:CHECKED       = ttTableFilter.lShowHidden.
    tgShowFrozen:CHECKED       = ttTableFilter.lShowFrozen.
    APPLY 'VALUE-CHANGED' TO tgShowSchema.

    /* Fill the combo boxes from the registry */
    RUN populateCombo(cbTableNameShow:HANDLE , 'TableNameShow' ).
    RUN populateCombo(cbTableNameHide:HANDLE , 'TableNameHide' ).
    RUN populateCombo(cbTableFieldShow:HANDLE, 'TableFieldShow').
    RUN populateCombo(cbTableFieldHide:HANDLE, 'TableFieldHide').

    /* Set screen values */
    cbTableNameShow :SCREEN-VALUE = ttTableFilter.cTableNameShow .
    cbTableNameHide :SCREEN-VALUE = ttTableFilter.cTableNameHide .
    cbTableFieldShow:SCREEN-VALUE = ttTableFilter.cTableFieldShow.
    cbTableFieldHide:SCREEN-VALUE = ttTableFilter.cTableFieldHide.
  END.

END PROCEDURE. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateCombo Dialog-Frame 
PROCEDURE populateCombo :
/* Set items in a combo box
  */
  DEFINE INPUT PARAMETER phCombo   AS HANDLE      NO-UNDO.
  DEFINE INPUT PARAMETER pcSetting AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE iMaxFilterHistory AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cFilterList       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cComboList        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iPos              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cThisValue        AS CHARACTER   NO-UNDO.

  /* Place search history in the combo */
  phCombo:DELIMITER = CHR(1).

  /* How many to remember? */
  iMaxFilterHistory = INTEGER(getRegistry('DataDigger', 'MaxFilterHistory')).
  IF iMaxFilterHistory = ? THEN iMaxFilterHistory = 10.

  /* Populate combo */
  cFilterList = getRegistry('DataDigger:Tables',pcSetting).

  IF cFilterList = ? THEN cFilterList = ''.

  /* Add old entries to the list */
  #AddEntry:
  DO iPos = 1 TO NUM-ENTRIES(cFilterList,CHR(1)).
    cThisValue = ENTRY(iPos,cFilterList,CHR(1)).
    IF cThisValue = '' THEN NEXT #AddEntry.

    /* If it is already in the list, ignore */
    IF LOOKUP(cThisValue,cComboList,CHR(1)) > 0 THEN NEXT #AddEntry.

    /* Add to list */
    cComboList = cComboList + CHR(1) + cThisValue.

    /* Stop if there are too much in the list */
    IF NUM-ENTRIES(cComboList,CHR(1)) >= iMaxFilterHistory THEN LEAVE #AddEntry.
  END. /* #AddEntry */

  cComboList = TRIM(cComboList,CHR(1)).
  IF NUM-ENTRIES(cComboList,CHR(1)) > 0 THEN phCombo:LIST-ITEMS = cComboList.

END PROCEDURE. /* populateCombo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveComboValue Dialog-Frame 
PROCEDURE saveComboValue :
/* Save filter items from combo box
  */
  DEFINE INPUT  PARAMETER phCombo   AS HANDLE      NO-UNDO.
  DEFINE INPUT  PARAMETER pcSetting AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE cValue      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cList       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDelim      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNewList    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iEntry      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iMaxEntries AS INTEGER     NO-UNDO.

  /* Set in normal vars for easier handling */
  cList = phCombo:LIST-ITEMS.
  IF cList = ? THEN cList = ''.

  cValue = phCombo:SCREEN-VALUE.
  IF cValue = ? THEN cValue = ''.

  cDelim = phCombo:DELIMITER.

  /* Set current screen value in front of list */
  cNewList = cValue.

  /* Add all old entries to it */
  #AddEntry:
  DO iEntry = 1 TO NUM-ENTRIES(cList,cDelim):
    /* If same as new entry then leave it out */
    IF ENTRY(iEntry,cList,cDelim) = cValue THEN NEXT #AddEntry.

    /* add to list */
    cNewList = SUBSTITUTE('&1&2&3', cNewList, cDelim, ENTRY(iEntry,cList,cDelim)).
    cNewList = TRIM(cNewList,cDelim).

    /* if list is at max length, step out */
    IF NUM-ENTRIES(cNewList,cDelim) = iMaxEntries THEN LEAVE #AddEntry.
  END. /* #AddEntry */

  /* and finally, save it */
  setRegistry('DataDigger:Tables',pcSetting,cNewList).

END PROCEDURE. /* saveComboValue */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

