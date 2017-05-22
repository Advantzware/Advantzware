&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

{ datadigger.i }

/* Parameters Definitions ---                                           */
define input  parameter phParent as handle      no-undo.

/* Local Variable Definitions ---                                       */

define temp-table ttRawData no-undo rcode-information
  field iRecordNr   as integer   format '>>9'  
  field iFieldNr    as integer   format '>>9'  
  field lSkipField  as logical   label ''         
  field cFieldValue as character format 'x(80)' label 'Raw Data'
  .

/*
/* TT for the fields of a table */
define temp-table ttField no-undo
  field lShow        as logical                     label ''         format 'XXX/'
  field iOrder       as decimal                     label 'Order'    format '>>>>>9'
  field cFieldName   as character                   label 'Name'     format 'x(40)'
  field cDataType    as character                   label 'Type'     format 'x(16)'
  field cInitial     as character                   label 'Initial' 
  field cFormat      as character                   label 'Format'   format 'x(24)'
  field cFormatOrg   as character                   label 'Format' 
  field cLabel       as character                   label 'Label' 
  field iOrderOrg    as decimal                                                        
  field iExtent      as integer 
  field lPrimary     as logical                     label 'Pr' 
  field lVisible     as logical                     label 'Pr' 
  field cFilterValue as character 
  index iPrim is primary iOrder.
*/

/* define temp-table ttField no-undo rcode-information */
/*   field iFieldNr    as integer   format '>>9'       */
/*   field lSelected   as logical   label ''           */
/*   field cFieldName  as character format 'x(20)'     */
/*   .                                                 */
/*                                                     */
define variable giCurrentRecord as integer no-undo. 
define variable giNumRecords    as integer no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frDataSettings
&Scoped-define BROWSE-NAME brAvailableFields

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttField ttRawData

/* Definitions for BROWSE brAvailableFields                             */
&Scoped-define FIELDS-IN-QUERY-brAvailableFields cFullName cDataType iOrder cLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brAvailableFields   
&Scoped-define SELF-NAME brAvailableFields
&Scoped-define QUERY-STRING-brAvailableFields FOR EACH ttField where ttField.lShow = false by ttField.iOrder
&Scoped-define OPEN-QUERY-brAvailableFields OPEN QUERY {&SELF-NAME}   FOR EACH ttField where ttField.lShow = false by ttField.iOrder.
&Scoped-define TABLES-IN-QUERY-brAvailableFields ttField
&Scoped-define FIRST-TABLE-IN-QUERY-brAvailableFields ttField


/* Definitions for BROWSE brRawData                                     */
&Scoped-define FIELDS-IN-QUERY-brRawData cFieldValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brRawData   
&Scoped-define SELF-NAME brRawData
&Scoped-define QUERY-STRING-brRawData FOR EACH ttRawData where ttRawData.iRecordNr = giCurrentRecord
&Scoped-define OPEN-QUERY-brRawData OPEN QUERY {&SELF-NAME} FOR EACH ttRawData where ttRawData.iRecordNr = giCurrentRecord.
&Scoped-define TABLES-IN-QUERY-brRawData ttRawData
&Scoped-define FIRST-TABLE-IN-QUERY-brRawData ttRawData


/* Definitions for BROWSE brSelectedFields                              */
&Scoped-define FIELDS-IN-QUERY-brSelectedFields cFullName cDataType iOrder cLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSelectedFields   
&Scoped-define SELF-NAME brSelectedFields
&Scoped-define QUERY-STRING-brSelectedFields FOR EACH ttField where ttField.lShow = true by ttField.iOrder
&Scoped-define OPEN-QUERY-brSelectedFields OPEN QUERY {&SELF-NAME}   FOR EACH ttField where ttField.lShow = true by ttField.iOrder.
&Scoped-define TABLES-IN-QUERY-brSelectedFields ttField
&Scoped-define FIRST-TABLE-IN-QUERY-brSelectedFields ttField


/* Definitions for FRAME frDataSettings                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-frDataSettings ~
    ~{&OPEN-QUERY-brAvailableFields}~
    ~{&OPEN-QUERY-brRawData}~
    ~{&OPEN-QUERY-brSelectedFields}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnPrev btnNext fiCurrentRecord fiNumRecords ~
brRawData brSelectedFields brAvailableFields btSelect btDeselect ~
btMoveUpSel btMoveUpAvail btMoveDownSel btMoveDownAvail 
&Scoped-Define DISPLAYED-OBJECTS fiCurrentRecord fiNumRecords 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btDeselect 
     LABEL ">" 
     SIZE-PIXELS 40 BY 24 TOOLTIP "deselect field".

DEFINE BUTTON btMoveDownAvail 
     LABEL "v" 
     SIZE-PIXELS 22 BY 22 TOOLTIP "move field down".

DEFINE BUTTON btMoveDownSel 
     LABEL "v" 
     SIZE-PIXELS 22 BY 22 TOOLTIP "move field down".

DEFINE BUTTON btMoveUpAvail 
     LABEL "^" 
     SIZE-PIXELS 22 BY 22 TOOLTIP "move field up".

DEFINE BUTTON btMoveUpSel 
     LABEL "^" 
     SIZE-PIXELS 22 BY 22 TOOLTIP "move field up".

DEFINE BUTTON btnNext 
     LABEL ">" 
     SIZE-PIXELS 40 BY 24 TOOLTIP "show next record".

DEFINE BUTTON btnPrev 
     LABEL "<" 
     SIZE-PIXELS 40 BY 24 TOOLTIP "show previous record".

DEFINE BUTTON btSelect 
     LABEL "<" 
     SIZE-PIXELS 40 BY 24 TOOLTIP "select field".

DEFINE VARIABLE fiCurrentRecord AS INTEGER FORMAT ">,>>>,>>9":U INITIAL ? 
     LABEL "Showing record nr" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 45 BY 21 NO-UNDO.

DEFINE VARIABLE fiNumRecords AS INTEGER FORMAT ">,>>>,>>9":U INITIAL ? 
     LABEL "of" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 45 BY 21 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brAvailableFields FOR 
      ttField SCROLLING.

DEFINE QUERY brRawData FOR 
      ttRawData SCROLLING.

DEFINE QUERY brSelectedFields FOR 
      ttField SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brAvailableFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brAvailableFields C-Win _FREEFORM
  QUERY brAvailableFields DISPLAY
      cFullName format 'x(30)'
      cDataType
        iOrder
      cLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 50 BY 28
          &ELSE SIZE-PIXELS 250 BY 585 &ENDIF FIT-LAST-COLUMN.

DEFINE BROWSE brRawData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brRawData C-Win _FREEFORM
  QUERY brRawData DISPLAY
      cFieldValue
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 79 BY 28
          &ELSE SIZE-PIXELS 395 BY 585 &ENDIF
         FONT 0 ROW-HEIGHT-CHARS .6 FIT-LAST-COLUMN.

DEFINE BROWSE brSelectedFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSelectedFields C-Win _FREEFORM
  QUERY brSelectedFields DISPLAY
      cFullName format 'x(30)'
      cDataType
      iOrder
      cLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 50 BY 28
          &ELSE SIZE-PIXELS 250 BY 585 &ENDIF FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frDataSettings
     btnPrev AT Y 5 X 15 WIDGET-ID 2
     btnNext AT Y 5 X 60 WIDGET-ID 10
     fiCurrentRecord AT Y 5 X 195 COLON-ALIGNED WIDGET-ID 12
     fiNumRecords AT Y 5 X 260 COLON-ALIGNED WIDGET-ID 14
     brRawData AT Y 35 X 15 WIDGET-ID 200
     brSelectedFields AT Y 35 X 420 WIDGET-ID 300
     brAvailableFields AT Y 35 X 720 WIDGET-ID 400
     btSelect AT Y 90 X 675 WIDGET-ID 16
     btDeselect AT Y 115 X 675 WIDGET-ID 18
     btMoveUpSel AT Y 170 X 670 WIDGET-ID 20
     btMoveUpAvail AT Y 170 X 700 WIDGET-ID 24
     btMoveDownSel AT Y 192 X 670 WIDGET-ID 22
     btMoveDownAvail AT Y 192 X 700 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 0 Y 0
         SIZE-PIXELS 978 BY 633 WIDGET-ID 100.


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
         TITLE              = "Import"
         HEIGHT-P           = 633
         WIDTH-P            = 978
         MAX-HEIGHT-P       = 1181
         MAX-WIDTH-P        = 1920
         VIRTUAL-HEIGHT-P   = 1181
         VIRTUAL-WIDTH-P    = 1920
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frDataSettings
   FRAME-NAME                                                           */
/* BROWSE-TAB brRawData fiNumRecords frDataSettings */
/* BROWSE-TAB brSelectedFields brRawData frDataSettings */
/* BROWSE-TAB brAvailableFields brSelectedFields frDataSettings */
ASSIGN 
       brAvailableFields:COLUMN-RESIZABLE IN FRAME frDataSettings       = TRUE.

ASSIGN 
       brSelectedFields:COLUMN-RESIZABLE IN FRAME frDataSettings       = TRUE.

ASSIGN 
       fiCurrentRecord:READ-ONLY IN FRAME frDataSettings        = TRUE.

ASSIGN 
       fiNumRecords:READ-ONLY IN FRAME frDataSettings        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brAvailableFields
/* Query rebuild information for BROWSE brAvailableFields
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
  FOR EACH ttField where ttField.lShow = false by ttField.iOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brAvailableFields */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brRawData
/* Query rebuild information for BROWSE brRawData
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttRawData where ttRawData.iRecordNr = giCurrentRecord.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brRawData */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSelectedFields
/* Query rebuild information for BROWSE brSelectedFields
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
  FOR EACH ttField where ttField.lShow = true by ttField.iOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brSelectedFields */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brAvailableFields
&Scoped-define SELF-NAME brAvailableFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brAvailableFields C-Win
ON DEFAULT-ACTION OF brAvailableFields IN FRAME frDataSettings
DO:
  if available ttField then ttField.lShow = not ttField.lShow.
  {&OPEN-QUERY-brAvailableFields}
  {&OPEN-QUERY-brSelectedFields}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brSelectedFields
&Scoped-define SELF-NAME brSelectedFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSelectedFields C-Win
ON DEFAULT-ACTION OF brSelectedFields IN FRAME frDataSettings
DO:
  if available ttField then ttField.lShow = not ttField.lShow.
  {&OPEN-QUERY-brAvailableFields}
  {&OPEN-QUERY-brSelectedFields}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSelectedFields C-Win
ON VALUE-CHANGED OF brSelectedFields IN FRAME frDataSettings
, brAvailableFields
DO:
  if available ttField then
    self:tooltip = substitute('type: &1~nlabel: &2'
                             , caps(ttField.cDataType)
                             , ttField.cLabel
                             ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDeselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeselect C-Win
ON CHOOSE OF btDeselect IN FRAME frDataSettings /* > */
or "DEFAULT-ACTION"    of brSelectedFields
or "CTRL-CURSOR-RIGHT" of brSelectedFields
or "CTRL-CURSOR-RIGHT" of brAvailableFields
DO:
  run shootField(brSelectedFields:handle,brAvailableFields:handle).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMoveDownAvail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMoveDownAvail C-Win
ON CHOOSE OF btMoveDownAvail IN FRAME frDataSettings /* v */
or "CTRL-CURSOR-DOWN" of brAvailableFields
DO:
  run moveField(brAvailableFields:handle,yes).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMoveDownSel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMoveDownSel C-Win
ON CHOOSE OF btMoveDownSel IN FRAME frDataSettings /* v */
or "CTRL-CURSOR-DOWN" of brSelectedFields
DO:
  run moveField(brSelectedFields:handle,yes).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMoveUpAvail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMoveUpAvail C-Win
ON CHOOSE OF btMoveUpAvail IN FRAME frDataSettings /* ^ */
or "CTRL-CURSOR-UP" of brAvailableFields
DO:
  run moveField(brAvailableFields:handle,no).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMoveUpSel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMoveUpSel C-Win
ON CHOOSE OF btMoveUpSel IN FRAME frDataSettings /* ^ */
or "CTRL-CURSOR-UP" of brSelectedFields
DO:
  run moveField(brSelectedFields:handle,no).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext C-Win
ON CHOOSE OF btnNext IN FRAME frDataSettings /* > */
DO:

  if giCurrentRecord < giNumRecords then
  do:
    giCurrentRecord = giCurrentRecord + 1.
    fiCurrentRecord:screen-value in frame {&frame-name} = string(giCurrentRecord).
    {&OPEN-QUERY-brRawData}
  end.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev C-Win
ON CHOOSE OF btnPrev IN FRAME frDataSettings /* < */
DO:

  if giCurrentRecord > 1 then
  do:
    giCurrentRecord = giCurrentRecord - 1.
    fiCurrentRecord:screen-value in frame {&frame-name} = string(giCurrentRecord).
    {&OPEN-QUERY-brRawData}
  end.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelect C-Win
ON CHOOSE OF btSelect IN FRAME frDataSettings /* < */
or "DEFAULT-ACTION"   of brAvailableFields
or "CTRL-CURSOR-LEFT" of brAvailableFields
or "CTRL-CURSOR-LEFT" of brSelectedFields
DO:
  run shootField(brAvailableFields:handle,brSelectedFields:handle).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brAvailableFields
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

DELETE WIDGET  {&WINDOW-NAME}.
{&WINDOW-NAME} = CURRENT-WINDOW.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
END.

/* General procedures for all viewers in the MVC */
{ frLoad.i }

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE commitPage C-Win 
PROCEDURE commitPage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define output parameter plTabOk as logical     no-undo.

  plTabOk = yes.


END PROCEDURE. /* commitPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiCurrentRecord fiNumRecords 
      WITH FRAME frDataSettings IN WINDOW C-Win.
  ENABLE btnPrev btnNext fiCurrentRecord fiNumRecords brRawData 
         brSelectedFields brAvailableFields btSelect btDeselect btMoveUpSel 
         btMoveUpAvail btMoveDownSel btMoveDownAvail 
      WITH FRAME frDataSettings IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frDataSettings}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveField C-Win 
PROCEDURE moveField :
/*------------------------------------------------------------------------
  Name         : moveField
  Description  : Move a field up or down in the list
  ---------------------------------------------------------------------- 
  28-02-2011 pti Created
  ----------------------------------------------------------------------*/

  define input  parameter phBrowse as handle  no-undo.
  define input  parameter plDown   as logical no-undo.

  define buffer bFieldSource for ttField. 

  define variable rFieldSource  as rowid   no-undo.
  define variable iOrderSource  as integer no-undo. 
  define variable iPosition     as integer no-undo. 

  /* If < 2 records, then no action */
  if phBrowse:query:num-results < 2 then return. 

  /* What record do we need to move */
  phBrowse:fetch-selected-row(1).
  rFieldSource = rowid(ttField).
  iOrderSource = ttField.iOrder. /* original nr */
  iPosition    = phBrowse:focused-row + (if plDown then 1 else 0).

  if not plDown and ttField.iOrder > phBrowse:num-iterations then iPosition = iPosition - 1.

  /* What is the 'other' one */
  if plDown then phBrowse:query:get-next().
            else phBrowse:query:get-prev().
  if not available ttField then return. 

  /* Find source field back and swap orders */
  find bFieldSource where rowid(bFieldSource) = rFieldSource.
  bFieldSource.iOrder = ttField.iOrder.
  ttField.iOrder      = iOrderSource.

  /* Reopen the browses */
  case phBrowse:name:
    when 'brAvailableFields' then {&OPEN-QUERY-brAvailableFields}
    when 'brSelectedFields'  then {&OPEN-QUERY-brSelectedFields}
  end case. 

  /* And point to the right records */
  if phBrowse:query:num-results >= iPosition then
    phBrowse:set-repositioned-row(iPosition,"ALWAYS").

  /* And point to the right records */
  phBrowse:query:reposition-to-rowid(rFieldSource).

END PROCEDURE. /* moveField */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshFrame C-Win 
PROCEDURE refreshFrame :
/*------------------------------------------------------------------------
  Name         : refreshFrame
  Description  : Show the frame and populate tables
  ---------------------------------------------------------------------- 
  28-02-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable cFileName   as character no-undo. 
  define variable iRecord     as integer   no-undo. 
  define variable iField      as integer   no-undo. 
  define variable iNumFields  as integer   no-undo. 
  define variable cRawData    as character no-undo extent 2000.
  define variable iLastOrder  as integer   no-undo. 

  empty temp-table ttRawData. 

  /* Get filename */
  cFilename = getData('DataFile').
  giNumRecords = integer(getData('NumRecords')).
  fiNumRecords:screen-value in frame {&frame-name} = string(giNumRecords).

  brRawData:row-height-pixels = brSelectedFields:row-height-pixels.

  btnPrev:load-image(getImagePath('Prev.gif')).
  btnNext:load-image(getImagePath('Next.gif')).
  btSelect:load-image(getImagePath('Prev.gif')).
  btDeSelect:load-image(getImagePath('Next.gif')).
  btMoveUpSel:load-image(getImagePath('Up.gif')).
  btMoveUpAvail:load-image(getImagePath('Up.gif')).
  btMoveDownSel:load-image(getImagePath('Down.gif')).
  btMoveDownAvail:load-image(getImagePath('Down.gif')).

  input from value(cFilename).

  /* Read data from datafile */
  do iRecord = 1 to giNumRecords:
    import cRawData. 

    /* How many fields are used? */
    if iRecord = 1 then
    do:
      findField:
      do iField = 2000 to 0 by -1:
        if cRawData[iField] <> "" then 
        do: 
          iNumFields = iField.
          leave findField.
        end.
      end.
    end.

    do iField = 1 to iNumFields:
      create ttRawData. 
      assign ttRawData.iRecordNr   = iRecord
             ttRawData.iFieldNr    = iField
             ttRawData.lSkipField  = no
             ttRawData.cFieldValue = cRawData[iField]. 
    end.
  end.

  input close. 

  /* Show first record */
  giCurrentRecord = 1.
  fiCurrentRecord:screen-value in frame {&frame-name} = string(giCurrentRecord).

  {&OPEN-QUERY-brRawData}

  /* Collect fields from target table */
  run getFields( input getData('TargetDatabase')
               , input getData('TargetTable')
               , output table ttField
               ).
  if not can-find(first ttField) then return. 

  /* Clean it up a little */
  for each ttField 
    where ttField.cFieldName = 'RECID'
       or ttField.cFieldName = 'ROWID': 
    delete ttField.
  end.

  /* Definition field for extents */
  for each ttField 
    where ttField.iExtent > 0
      and ttField.lMetaField = true: 
    delete ttField.
  end.

  /* Reorder the fields and set 'selected' to true */
  iLastOrder = 0.
  repeat preselect each ttField by ttField.iOrder:
    find next ttField.
    iLastOrder = iLastOrder + 1.
    ttField.iOrder = iLastOrder.
    ttField.lShow  = true.
  end.

  /* Add 'empty field' placeholder */
  create ttField. 
  assign ttField.cFieldName = '<empty>'
         ttField.cFullName  = '<empty>'
         ttField.lShow      = no
         ttField.iOrder     = iLastOrder + 1
         .

  {&OPEN-QUERY-brAvailableFields}
  {&OPEN-QUERY-brSelectedFields}

end procedure. /* refreshFrame */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shootField C-Win 
PROCEDURE shootField :
/*------------------------------------------------------------------------
  Name         : shootField
  Description  : Move a field from brSelected to brAvailable or vv
  ---------------------------------------------------------------------- 
  28-02-2011 pti Created
  ----------------------------------------------------------------------*/

  define input  parameter phSource as handle no-undo.
  define input  parameter phTarget as handle no-undo.

  define variable rFieldSource  as rowid   no-undo.
  define variable rFieldTarget  as rowid   no-undo.
  define variable iLastOrder    as integer no-undo. 
  define variable iSourcePos    as integer no-undo.
  define variable iTargetPos    as integer no-undo. 

  define buffer bField for ttField. 

  /* Find highest nr used */
  find last ttField.
  iLastOrder = ttField.iOrder.

  /* If no records, then no action */
  if phSource:query:num-results = 0 then return. 

  /* Save position */
  iSourcePos = phSource:focused-row.
  iTargetPos = phTarget:focused-row.

  /* What record do we need to move */
  phSource:fetch-selected-row(1).
  ttField.lShow  = not ttField.lShow.
  ttField.iOrder = iLastOrder + 1.
  rFieldTarget   = rowid(ttField).

  /* If we moved a <empty> field to brSelected, 
   * create a new one 
   */
  if ttField.cFieldName = '<empty>' 
    and phSource:name = 'brAvailableFields' then
  do:
    find last bField.
    iLastOrder = bField.iOrder.

    create bField.
    assign bField.cFieldName = '<empty>'
           bField.cFullName  = '<empty>'
           bField.lShow      = no
           bField.iOrder     = iLastOrder + 1
           .
    rFieldSource = rowid(bField).
  end.

  else                                            
  do:
    /* What record should the source browse point to */
    phSource:query:get-next().
    if not available ttField then do:
      phSource:query:get-prev(). /* will point to the record to be moved */
      phSource:query:get-prev(). /* will point to the previous one */
    end.
    if available ttField then rFieldSource = rowid(ttField).
  end.

  /* Remove duplicate <empty> fields in brAvailableFields */
  for each bField 
    where bField.cFieldName = '<empty>' 
      and bField.lShow      = false
      and rowid(bField) <> rFieldSource
      and rowid(bField) <> rFieldTarget
      break by bField.lShow:
    delete bField.
  end.

  /* Reopen the browses */
  {&OPEN-QUERY-brAvailableFields}
  {&OPEN-QUERY-brSelectedFields}

  /* And point to the right records */
  if phSource:num-results >= iSourcePos then
    phSource:set-repositioned-row(iSourcePos,"ALWAYS").
  if phTarget:num-results >= iTargetPos then
    phTarget:set-repositioned-row(iTargetPos,"ALWAYS").

  if rFieldSource <> ? then phSource:query:reposition-to-rowid(rFieldSource).
  phTarget:query:reposition-to-rowid(rFieldTarget).

END PROCEDURE. /* shootField */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

