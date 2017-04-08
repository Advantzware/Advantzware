&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wEdit 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{ DataDigger.i }
/* { windows.i } */

/* Parameters Definitions ---                                           */
define {&invar}  pihBrowse         as handle     no-undo.
define {&invar}  picDatabase       as character  no-undo.
define {&invar}  picTableName      as character  no-undo.

&if defined(UIB_is_Running) eq 0  &then
define {&invar} table for ttField.
&endif

define {&outvar} polSuccess        as logical    no-undo initial ?.

/* Local Variable Definitions ---                                       */

/* This table holds the actual values of the selected records */
define temp-table ttData no-undo rcode-information
  field cFieldName    as character
  field cValue        as character format 'x(80)'
  index iPrim is primary cFieldName cValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frEdit
&Scoped-define BROWSE-NAME brFields

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttField

/* Definitions for BROWSE brFields                                      */
&Scoped-define FIELDS-IN-QUERY-brFields ttField.lShow ttField.cFullName ttField.cLabel ttField.cNewValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brFields ttField.lShow  ttField.cNewValue   
&Scoped-define ENABLED-TABLES-IN-QUERY-brFields ttField
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brFields ttField
&Scoped-define SELF-NAME brFields
&Scoped-define QUERY-STRING-brFields FOR EACH ttField
&Scoped-define OPEN-QUERY-brFields OPEN QUERY {&SELF-NAME} FOR EACH ttField.
&Scoped-define TABLES-IN-QUERY-brFields ttField
&Scoped-define FIRST-TABLE-IN-QUERY-brFields ttField


/* Definitions for FRAME frEdit                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brFields fiNumRecords btnOk btnClose 
&Scoped-Define DISPLAYED-OBJECTS fiNumRecords 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wEdit AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose AUTO-END-KEY 
     LABEL "&Close" 
     SIZE-PIXELS 74 BY 24.

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "&Ok" 
     SIZE-PIXELS 74 BY 24 TOOLTIP "start the dump".

DEFINE VARIABLE fiNumRecords AS CHARACTER FORMAT "X(256)":U 
     LABEL "Records Selected" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 50 BY 21 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brFields FOR 
      ttField SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brFields wEdit _FREEFORM
  QUERY brFields DISPLAY
      ttField.lShow      column-label '' view-as toggle-box 
  ttField.cFullName 
  ttField.cLabel
  ttField.cNewValue
  enable 
  ttField.lShow 
  ttField.cNewValue
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 133 BY 21
          &ELSE SIZE-PIXELS 665 BY 450 &ENDIF FIT-LAST-COLUMN TOOLTIP "fields to edit".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frEdit
     brFields AT Y 5 X 0 WIDGET-ID 200
     fiNumRecords AT Y 465 X 90 COLON-ALIGNED WIDGET-ID 10
     btnOk AT Y 465 X 505 WIDGET-ID 6
     btnClose AT Y 465 X 585 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         CANCEL-BUTTON btnClose WIDGET-ID 100.


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
  CREATE WINDOW wEdit ASSIGN
         HIDDEN             = YES
         TITLE              = "Edit records"
         HEIGHT-P           = 492
         WIDTH-P            = 665
         MAX-HEIGHT-P       = 2079
         MAX-WIDTH-P        = 1600
         VIRTUAL-HEIGHT-P   = 2079
         VIRTUAL-WIDTH-P    = 1600
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wEdit
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frEdit
   FRAME-NAME Size-to-Fit                                               */
/* BROWSE-TAB brFields 1 frEdit */
ASSIGN 
       FRAME frEdit:SCROLLABLE       = FALSE
       FRAME frEdit:RESIZABLE        = TRUE.

ASSIGN 
       brFields:COLUMN-RESIZABLE IN FRAME frEdit       = TRUE.

ASSIGN 
       fiNumRecords:READ-ONLY IN FRAME frEdit        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wEdit)
THEN wEdit:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brFields
/* Query rebuild information for BROWSE brFields
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttField.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brFields */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wEdit wEdit
ON END-ERROR OF wEdit /* Edit records */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wEdit wEdit
ON WINDOW-CLOSE OF wEdit /* Edit records */
or "LEAVE" of wEdit
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wEdit wEdit
ON WINDOW-RESIZED OF wEdit /* Edit records */
DO:
  run LockWindow (input wEdit:handle, input yes).

  do with frame frEdit:

    /* Make 'em small so we don't get errors on resizing the window */
    btnOk:x    = 1.
    btnOk:y    = 1.
    btnClose:x = 1.
    btnClose:y = 1.

    /* Set frame width */
    frame frEdit:width-pixels  = wEdit:width-pixels no-error.
    frame frEdit:height-pixels = wEdit:height-pixels no-error.

    /* Adjust the browse */
    brFields:width-pixels  = frame frEdit:width-pixels - 3.
    brFields:height-pixels = frame frEdit:height-pixels - 40.

    btnOk:x        = brFields:width-pixels  - 160.
    btnOk:y        = frame frEdit:height-pixels - 30.
    btnClose:x     = brFields:width-pixels  - 80.
    btnClose:y     = frame frEdit:height-pixels - 30.
    fiNumRecords:y = frame frEdit:height-pixels - 30.
    fiNumRecords:side-label-handle:y = fiNumRecords:y.


    /* Save settings */
    setRegistry("DataDigger:Edit", "Window:x", string(wEdit:x) ).                             
    setRegistry("DataDigger:Edit", "Window:y", string(wEdit:y) ).                             
    setRegistry("DataDigger:Edit", "Window:height", string(wEdit:height-pixels) ).                             
    setRegistry("DataDigger:Edit", "Window:width", string(wEdit:width-pixels) ).                             
  end.

  run showScrollBars(frame frEdit:handle, no, no).
  run LockWindow (input wEdit:handle, input no).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frEdit wEdit
ON GO OF FRAME frEdit
DO:
  run btnGoChoose(output polSuccess).
  if not polSuccess then return no-apply.
  else apply 'close' to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brFields
&Scoped-define SELF-NAME brFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields wEdit
ON RETURN OF brFields IN FRAME frEdit
DO:
  apply 'entry' to ttField.cNewValue in browse brFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields wEdit
ON ROW-DISPLAY OF brFields IN FRAME frEdit
DO:

  if ttField.cNewValue <> ttField.cOldValue then
    ttField.cNewValue:fgcolor in browse brFields = 12.
  else
    ttField.cNewValue:fgcolor in browse brFields = 9.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose wEdit
ON CHOOSE OF btnClose IN FRAME frEdit /* Close */
DO:
  define variable hToggle as handle no-undo.
  define buffer ttField for ttField. 
  define buffer ttData  for ttData. 

  /* If we are updating and we press ESC we don't want to close the 
   * window, but we want to escape out of the update mode
   */
  if focus:name = 'cNewValue' then
  do with frame {&frame-name}:

    find ttField where ttField.cFullName = brFields:get-browse-column(2):screen-value.

    /* See if there is only ONE ttData for this field.
     * The find will only succeed if there is exactly ONE record
     */
    find ttData where ttData.cFieldName = ttField.cFullName no-error.
    if not available ttData then ttField.lShow = no.

    brFields:get-browse-column(4):screen-value in frame {&frame-name} = ttField.cOldValue.
    brFields:get-browse-column(1):screen-value in frame {&frame-name} = string(ttField.lShow).

    hToggle = brFields:get-browse-column(1) in frame {&frame-name}.
    apply 'entry' to hToggle.

    return no-apply.
  end.

  apply 'close' to this-procedure. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:09 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk wEdit
ON CHOOSE OF btnOk IN FRAME frEdit /* Ok */
do:

  run btnGoChoose(output polSuccess).
  if not polSuccess then return no-apply.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:09 pm */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wEdit 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
do:
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:09 pm */
END.
  /* Save settings */
  setRegistry("DataDigger:Edit", "Window:x", string(wEdit:x) ).                             
  setRegistry("DataDigger:Edit", "Window:y", string(wEdit:y) ).                             
  setRegistry("DataDigger:Edit", "Window:height", string(wEdit:height-pixels) ).                             
  setRegistry("DataDigger:Edit", "Window:width", string(wEdit:width-pixels) ).    

  RUN disable_UI.
end.

on 'RETURN' of ttField.lShow in browse brFields
do:
  define variable hDataField as handle no-undo. 
  hDataField = brFields:get-browse-column(4) in frame {&frame-name}.
  apply 'ENTRY' to hDataField.
  return no-apply.
end.

on " " of ttField.lShow in browse brFields
do:
  do with frame {&frame-name}:
    ttField.lShow = not ttField.lShow.
    brFields:get-browse-column(1):checked  = ttField.lShow.
    apply 'value-changed' to ttField.lShow in browse brFields.
    brFields:select-next-row().
  end.
end.

on value-changed of ttField.lShow in browse brFields
do:
  /* If you toggle the field off, set the value to blank */
  do with frame {&FRAME-NAME}:
    if brFields:get-browse-column(1):screen-value = "no" then 
    do:
      ttField.cNewValue = ''.
      ttField.lShow     = no.
      brFields:get-browse-column(4):screen-value = "".
    end.
  end.
end. 

on 'PAGE-DOWN' of ttField.cNewValue in browse brFields
do:
  define buffer ttData for ttData.
  find first ttData 
    where ttData.cFieldName = ttField.cFieldName
      and ttData.cValue     > self:screen-value no-error.

  if available ttData then 
  do:
    self:screen-value = ttData.cValue.
    apply 'value-changed' to self.
  end. 

  return no-apply.
end.

on 'PAGE-UP' of ttField.cNewValue in browse brFields
do:
  define buffer ttData for ttData.
  find last ttData 
    where ttData.cFieldName = ttField.cFieldName
      and ttData.cValue     < self:screen-value no-error.

  if available ttData then 
  do:
    self:screen-value = ttData.cValue.
    apply 'value-changed' to self.
  end. 

  return no-apply.
end.

on value-changed of ttField.cNewValue in browse brFields
do:
  ttField.lShow = yes.  
  ttField.cNewValue = self:input-value.
  do with frame {&frame-name}:
    brFields:get-browse-column(1):screen-value = "YES".
  end.
end.

on 'entry' of ttField.cNewValue in browse brFields
do:
  do with frame {&frame-name}:
    brFields:tooltip = 'use CTRL-PGUP / CTRL-PGDN to~nbrowse through existing values'.
    if ttField.cDataType = 'character' then
      self:format = ttField.cFormat.  
  end.
end.

on 'leave' of ttField.cNewValue in browse brFields
do:
  do with frame {&frame-name}:
    brFields:tooltip = 'fields to edit'.
  end.
end.



/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  run initializeObject.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:09 pm */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnGoChoose wEdit 
PROCEDURE btnGoChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define output parameter polSuccess as logical no-undo. 

  define variable hBuffer as handle      no-undo.
  define variable iRow    as integer     no-undo.

  define buffer ttField for ttField. 

  hBuffer = pihBrowse:query:get-buffer-handle(1).

  commitLoop:
  do transaction:
    for each ttField where ttField.lShow = true
      on error undo commitLoop, leave commitLoop:

      do iRow = 1 to pihBrowse:num-selected-rows:
        pihBrowse:fetch-selected-row(iRow).

        hBuffer:disable-load-triggers(yes).
        hBuffer:disable-dump-triggers( ).

        hBuffer:find-current(exclusive-lock).
        hBuffer:buffer-field(ttField.cFieldName):buffer-value(ttField.iExtent) = ttField.cNewValue.
        hBuffer:buffer-release.
      end.
    end.

    polSuccess = true.
  end. /* transaction */

end procedure. /* btnGoChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wEdit  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wEdit)
  THEN DELETE WIDGET wEdit.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wEdit  _DEFAULT-ENABLE
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
  DISPLAY fiNumRecords 
      WITH FRAME frEdit IN WINDOW wEdit.
  ENABLE brFields fiNumRecords btnOk btnClose 
      WITH FRAME frEdit IN WINDOW wEdit.
  {&OPEN-BROWSERS-IN-QUERY-frEdit}
  VIEW wEdit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDataValues wEdit 
PROCEDURE getDataValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter phBrowse as handle      no-undo.
  define input  parameter pcColumn as character   no-undo.

  define variable hBuffer     as handle      no-undo.
  define variable iRow        as integer     no-undo.
  define variable cRowValue   as character   no-undo.

  define buffer ttData for ttData. 

  hBuffer = phBrowse:query:get-buffer-handle(1).

  addValue:
  do iRow = 1 to phBrowse:num-selected-rows:
    phBrowse:fetch-selected-row(iRow).

    /* Max length for combo is 263 see pkb P21143 */
    cRowValue = hBuffer:buffer-field(ttField.cFieldName):buffer-value(ttField.iExtent).
    cRowValue = substring(cRowValue,1,250).

    /* Already in the set or not? */
    find ttData 
      where ttData.cFieldName = pcColumn 
        and ttData.cValue     = cRowValue
            no-error.

    if not available ttData then
    do:
      create ttData.
      assign ttData.cFieldName = pcColumn 
             ttData.cValue     = cRowValue.
    end.
  end. 

END PROCEDURE. /* getDataValues */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wEdit 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define variable iFieldExtent    as integer   no-undo.
  define variable cExtentFormat   as character no-undo.
  define variable iMaxFieldLength as integer   no-undo.
  define variable hBuffer         as handle    no-undo.  
  define variable iValue          as integer   no-undo. 
  define variable cValueList      as character no-undo.

  define buffer bField for ttField. 

  /* Get rid of all hidden fields. Since the tt is a COPY of the tt
   * in the main window we can safely delete them. While we're at 
   * it, get rid of other trash as well
   */
  for each ttField 
    where ttField.lShow      = false   /* Hidden by user     */
       or ttField.lDataField = false   /* No real data field */
       or ttField.cFieldName = 'RECID'
       or ttField.cFieldName = 'ROWID':
    delete ttField.
  end.

  /* Find out max fieldname length */
  for each ttField: 
    ttField.cFilterValue = ''.    /* cFilterValue is now the list of currently used values */
    ttField.lShow        = false. /* lShow now means: "Change this field" */
    iMaxFieldLength      = maximum(iMaxFieldLength,length(ttField.cFullName)).
  end.

  /* Collect data for all fields 
   * And  if we only have 1 value for all selected records, let's show that 
   */
  for each ttField: 
    run getDataValues(pihBrowse,ttField.cFullName).

    find ttData where ttData.cFieldName = ttField.cFullName no-error.
    if available ttData then
      assign 
        ttField.cOldValue    = ttData.cValue /* so we can revert to the old value */
        ttField.cNewValue    = ttData.cValue
        ttField.lShow        = true. 
  end.

  /* Adjust column width to fit precisely */
  do with frame {&frame-name}:
    brFields:get-browse-column(2):width-chars = iMaxFieldLength + 2.

    /* Window position and size */
    /* Set title of the window */
    hBuffer = pihBrowse:query:get-buffer-handle(1).
    wEdit:title = substitute('&1 - &2.&3'
                            , wEdit:title 
                            , hBuffer:dbname
                            , hBuffer:table
                            ).

    /* Num records */
    fiNumRecords:screen-value = string(pihBrowse:num-selected-rows).

    /* Set minimum size of the window */
    wEdit:min-width-pixels  = 400.
    wEdit:min-height-pixels = 200.

    /* to avoid scrollbars on the frame */
    frame {&frame-name}:scrollable = false.

    iValue = integer(getRegistry('DataDigger:Edit', 'Window:x' )).
    if iValue = ? then iValue = integer(getRegistry('DataDigger', 'Window:x' )) + 50.
    assign wEdit:x = iValue no-error.

    /* Window has been parked at y=-1000 to get it out of sight */
    iValue = integer(getRegistry('DataDigger:Edit', 'Window:y' )).
    if iValue = ? then iValue = integer(getRegistry('DataDigger', 'Window:y' )) + 50.
    if iValue <> ? then assign wEdit:y = iValue no-error.

    iValue = integer(getRegistry('DataDigger:Edit', 'Window:height' )).
    if iValue = ? or iValue = 0 then iValue = integer(getRegistry('DataDigger', 'Window:height' )) - 100. 
    assign wEdit:height-pixels = iValue no-error.

    iValue = integer(getRegistry('DataDigger:Edit', 'Window:width' )).
    if iValue = ? or iValue = 0 then iValue = integer(getRegistry('DataDigger', 'Window:width' )) - 100.
    assign wEdit:width-pixels = iValue no-error.
  end. 

  /* Force a redraw */
  apply 'window-resized' to wEdit.

  /* reopen the browse */
  {&OPEN-QUERY-brFields} 

end procedure. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

