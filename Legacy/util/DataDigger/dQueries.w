&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{ datadigger.i }

/* Parameters Definitions ---                                           */

define input parameter  pcDatabase     as character no-undo.
define input parameter  pcTable        as character no-undo.
define input parameter  pcCurrentQuery as character no-undo. 
define output parameter piQueryNr      as integer no-undo initial ?. 

/* Local Variable Definitions ---                                       */

define variable giQueryOffset  as integer no-undo.
define variable ghEditor       as handle  extent 5 no-undo. 
define variable ghDelButton    as handle  extent 5 no-undo. 
define variable giQuery        as integer extent 5 no-undo. 
define variable ghActiveEditor as handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDelQuery-1 btDelQuery-2 btDelQuery-3 ~
btDelQuery-4 btDelQuery-5 RECT-5 EdQuery-1 btUp EdQuery-2 btDown EdQuery-3 ~
EdQuery-4 EdQuery-5 BtnOK BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS EdQuery-1 EdQuery-2 EdQuery-3 EdQuery-4 ~
EdQuery-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-EdQuery-1 
       MENU-ITEM m_Edit         LABEL "Edit"          
       MENU-ITEM m_Delete       LABEL "Delete"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btDelQuery-1  NO-FOCUS FLAT-BUTTON
     LABEL "Del" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-2  NO-FOCUS FLAT-BUTTON
     LABEL "Del" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-3  NO-FOCUS FLAT-BUTTON
     LABEL "Del" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-4  NO-FOCUS FLAT-BUTTON
     LABEL "Del" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-5  NO-FOCUS FLAT-BUTTON
     LABEL "Del" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDown 
     LABEL "Down" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "go down".

DEFINE BUTTON BtnCancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btUp 
     LABEL "Up" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "go up".

DEFINE VARIABLE EdQuery-1 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-2 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     BGCOLOR 8 FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-3 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-4 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     BGCOLOR 8 FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-5 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  
     SIZE-PIXELS 85 BY 378.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btDelQuery-1 AT Y 5 X 420 WIDGET-ID 56
     btDelQuery-2 AT Y 80 X 420 WIDGET-ID 58
     btDelQuery-3 AT Y 155 X 420 WIDGET-ID 60
     btDelQuery-4 AT Y 230 X 420 WIDGET-ID 62
     btDelQuery-5 AT Y 305 X 420 WIDGET-ID 64
     EdQuery-1 AT Y 5 X 5 NO-LABEL WIDGET-ID 2
     btUp AT Y 55 X 465 WIDGET-ID 52
     EdQuery-2 AT Y 80 X 5 NO-LABEL WIDGET-ID 4
     btDown AT Y 80 X 465 WIDGET-ID 54
     EdQuery-3 AT Y 155 X 5 NO-LABEL WIDGET-ID 6
     EdQuery-4 AT Y 230 X 5 NO-LABEL WIDGET-ID 12
     EdQuery-5 AT Y 305 X 5 NO-LABEL WIDGET-ID 10
     BtnOK AT Y 325 X 465 WIDGET-ID 48
     BtnCancel AT Y 355 X 465
     RECT-5 AT Y 5 X 460 WIDGET-ID 66
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 560 BY 412
         TITLE "Select query"
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       EdQuery-1:POPUP-MENU IN FRAME Dialog-Frame       = MENU POPUP-MENU-EdQuery-1:HANDLE
       EdQuery-1:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       EdQuery-2:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       EdQuery-3:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       EdQuery-4:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       EdQuery-5:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Select query */
DO:
  
  run saveQuery( ghActiveEditor:private-data
               , ghActiveEditor:screen-value ).

  run saveQueryTable( input table ttQuery
                    , input pcDatabase
                    , input pcTable
                    ).

  piQueryNr = integer(ghActiveEditor:private-data) no-error.
  if error-status:error 
    or not can-find(ttQuery 
              where ttQuery.cDatabase = pcDatabase
                and ttQuery.cTable    = pcTable
                and ttQuery.iQuery    = piQueryNr ) then piQueryNr = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON LEAVE OF FRAME Dialog-Frame /* Select query */
DO:
  self:bgcolor = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select query */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-1 Dialog-Frame
ON CHOOSE OF btDelQuery-1 IN FRAME Dialog-Frame /* Del */
or 'shift-del' of EdQuery-1
DO:

  run deleteQuery( ghEditor[1]:private-data ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-2 Dialog-Frame
ON CHOOSE OF btDelQuery-2 IN FRAME Dialog-Frame /* Del */
or 'shift-del' of EdQuery-2
DO:

  run deleteQuery(giQueryOffset + 1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-3 Dialog-Frame
ON CHOOSE OF btDelQuery-3 IN FRAME Dialog-Frame /* Del */
or 'shift-del' of EdQuery-3
DO:

  run deleteQuery(giQueryOffset + 2).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-4 Dialog-Frame
ON CHOOSE OF btDelQuery-4 IN FRAME Dialog-Frame /* Del */
or 'shift-del' of EdQuery-4
DO:

  run deleteQuery(giQueryOffset + 3).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-5 Dialog-Frame
ON CHOOSE OF btDelQuery-5 IN FRAME Dialog-Frame /* Del */
or 'shift-del' of EdQuery-5
DO:

  run deleteQuery(giQueryOffset + 4).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDown Dialog-Frame
ON CHOOSE OF btDown IN FRAME Dialog-Frame /* Down */
DO:
  apply 'cursor-down' to ghActiveEditor.
  apply 'entry' to ghActiveEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUp Dialog-Frame
ON CHOOSE OF btUp IN FRAME Dialog-Frame /* Up */
DO:
  apply 'cursor-up' to ghActiveEditor.
  apply 'entry' to ghActiveEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-1 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON CURSOR-UP OF EdQuery-1 IN FRAME Dialog-Frame
DO:
  giQueryOffset = giQueryOffset - 1.
  if giQueryOffset < 1 then giQueryOffset = 1.

  run showQueries.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON ENTRY OF EdQuery-1 IN FRAME Dialog-Frame
, edQuery-2, edQuery-3, edQuery-4, edQuery-5 DO:
  
  self:read-only = no.
  self:bgcolor = 14.

  ghActiveEditor = self:handle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON LEAVE OF EdQuery-1 IN FRAME Dialog-Frame
, edQuery-3, edQuery-5 
DO:
  if self:modified then 
    run saveQuery( self:private-data, self:screen-value ).

  self:read-only = yes.
  self:bgcolor = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF EdQuery-1 IN FRAME Dialog-Frame
, edQuery-2, edQuery-3, edQuery-4, edQuery-5 
or 'return' of edQuery-1, edQuery-2, edQuery-3, edQuery-4, edQuery-5 

DO:
  apply 'GO' to frame {&frame-name}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-2 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-2 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-2 Dialog-Frame
ON CURSOR-UP OF EdQuery-2 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-2 Dialog-Frame
ON LEAVE OF EdQuery-2 IN FRAME Dialog-Frame
, edQuery-4 
DO:
  if self:modified then 
    run saveQuery( self:private-data, self:screen-value ).

  self:read-only = yes.
  self:bgcolor = 8.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-3 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-3 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-3 Dialog-Frame
ON CURSOR-UP OF EdQuery-3 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-4 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-4 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-4 Dialog-Frame
ON CURSOR-UP OF EdQuery-4 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-5 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-5 IN FRAME Dialog-Frame
DO:

  /* Only proceed if there is more to show */
  .if not can-find(first ttQuery 
            where ttQuery.cDatabase = pcDatabase
              and ttQuery.cTable    = pcTable
              and ttQuery.iQueryNr  > giQueryOffset + 5) then 
    return.

  run saveQuery( ghActiveEditor:private-data
               , ghActiveEditor:screen-value ).

  giQueryOffset = giQueryOffset + 1.
  if giQueryOffset > 10 then giQueryOffset = 10.

  run showQueries.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-5 Dialog-Frame
ON CURSOR-UP OF EdQuery-5 IN FRAME Dialog-Frame
DO:
  apply 'entry' to edQuery-4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  run enable_UI.
  run initializeObject.

  wait-for go of frame {&frame-name} focus ghActiveEditor.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteQuery Dialog-Frame 
PROCEDURE deleteQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter piQueryNr as integer     no-undo.
  define variable iQuery as integer     no-undo.
  define buffer bQuery for ttQuery.

  find bQuery 
    where bQuery.cDatabase = pcDatabase
      and bQuery.cTable    = pcTable
      and bQuery.iQueryNr  = piQueryNr no-error.

  if available bQuery then 
  do:
    delete bQuery.

    run renumberQueries.
    run showQueries.
  end.


end procedure. /* deleteQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY EdQuery-1 EdQuery-2 EdQuery-3 EdQuery-4 EdQuery-5 
      WITH FRAME Dialog-Frame.
  ENABLE btDelQuery-1 btDelQuery-2 btDelQuery-3 btDelQuery-4 btDelQuery-5 
         RECT-5 EdQuery-1 btUp EdQuery-2 btDown EdQuery-3 EdQuery-4 EdQuery-5 
         BtnOK BtnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Dialog-Frame 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define buffer bQuery for ttQuery.

  do with frame {&frame-name}:
    
    /* Make sure the lib is running */
    run startDiggerLib.

    /* Get all queries */
    run getQueryTable( output table ttQuery ).
    
    /* Init handles */
    ghEditor[1] = edQuery-1:handle. 
    ghEditor[2] = edQuery-2:handle. 
    ghEditor[3] = edQuery-3:handle. 
    ghEditor[4] = edQuery-4:handle. 
    ghEditor[5] = edQuery-5:handle. 
  
    ghDelButton[1] = btDelQuery-1:handle.
    ghDelButton[2] = btDelQuery-2:handle.
    ghDelButton[3] = btDelQuery-3:handle.
    ghDelButton[4] = btDelQuery-4:handle.
    ghDelButton[5] = btDelQuery-5:handle.

    /* Init images */
    btUp:load-image(getImagePath('Up.gif')).
    btDown:load-image(getImagePath('Down.gif')).
    btDelQuery-1:load-image(getImagePath('Clear.gif') ).
    btDelQuery-2:load-image(getImagePath('Clear.gif')).
    btDelQuery-3:load-image(getImagePath('Clear.gif')).
    btDelQuery-4:load-image(getImagePath('Clear.gif')).
    btDelQuery-5:load-image(getImagePath('Clear.gif')).

    /* Transform query to internal format */
    pcCurrentQuery = replace(pcCurrentQuery,chr(1),'~n').
    pcCurrentQuery = replace(pcCurrentQuery, '~n', chr(160)).

    frame {&frame-name}:title = substitute('Select query for &1.&2'
                                          , pcDatabase
                                          , pcTable   
                                          ).
    run showQueries.
    giQueryOffset = 1.
    ghActiveEditor = ghEditor[1].

    /* Point to current query */
    find first bQuery 
      where bQuery.cDatabase = pcDatabase
        and bQuery.cTable    = pcTable 
        and bQuery.cQuery    = pcCurrentQuery
            no-error.

    if available bQuery then
    do:
      if bQuery.iQueryNr > 5 then 
      do:
        giQueryOffset = bQuery.iQueryNr - 4.
        ghActiveEditor = ghEditor[5].
        run showQueries.
      end.
      else 
      do:
        ghActiveEditor = ghEditor[bQuery.iQueryNr].
      end.
    end.

  end.

end procedure. /* initializeObject. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renumberQueries Dialog-Frame 
PROCEDURE renumberQueries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define variable iQuery as integer     no-undo.
  define buffer bQuery for ttQuery.

  /* Renumber ttQuery temp-table */
  iQuery = 0.

  repeat preselect each bQuery 
    where bQuery.cDatabase = pcDatabase
      and bQuery.cTable    = pcTable 
       by bQuery.iQueryNr:

    find next bQuery.
    assign 
      iQuery          = iQuery + 1
      bQuery.iQueryNr = iQuery.
  end.

end procedure. /* renumberQueries */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveQuery Dialog-Frame 
PROCEDURE saveQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter piQueryNr  as integer     no-undo.
  define input  parameter pcQueryTxt as character   no-undo.

  define buffer bQuery for ttQuery.

  /* New query? */
  if piQueryNr = 0 then
  do:
    find last bQuery 
      where bQuery.cDatabase = pcDatabase
        and bQuery.cTable    = pcTable no-error.

    if available bQuery then 
      piQueryNr = bQuery.iQueryNr.
    else 
      piQueryNr = 1.

    create bQuery. 
    assign bQuery.cDatabase = pcDatabase
           bQuery.cTable    = pcTable
           bQuery.iQueryNr  = piQueryNr
           bQuery.cQuery    = pcQueryTxt.
  end.
  else 
  do:
    find bQuery 
      where bQuery.cDatabase = pcDatabase
        and bQuery.cTable    = pcTable
        and bQuery.iQueryNr  = piQueryNr no-error.

    bQuery.cQuery = pcQueryTxt.
  end.

  run renumberQueries.
  run showQueries.

end procedure. /* deleteQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButtons Dialog-Frame 
PROCEDURE setButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define variable iQuery as integer no-undo.

  do iQuery = 1 to 5:
    ghDelButton[iQuery]:sensitive = (ghEditor[iQuery]:screen-value <> '').
  end.

end procedure. /* setButtons */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showQueries Dialog-Frame 
PROCEDURE showQueries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define variable iQuery as integer     no-undo.
  define variable iLoop  as integer     no-undo.
  define variable cQuery as character   no-undo.

  define buffer bQuery for ttQuery.
  
  for each bQuery 
    where bQuery.cDatabase = pcDatabase
      and bQuery.cTable    = pcTable
      and bQuery.iQueryNr  >= giQueryOffset:

    iQuery = iQuery + 1.

    cQuery = replace(bQuery.cQueryTxt,chr(1),'~n').
    ghEditor[iQuery]:screen-value = cQuery.
    ghEditor[iQuery]:private-data = string(bQuery.iQueryNr).

    if iQuery = 5 then leave.
  end.

  do iLoop = iQuery + 1 to 5:
    ghEditor[iLoop]:screen-value = ''.
    ghEditor[iLoop]:private-data = ''.
  end.

  run setButtons.

end procedure. /* showQueries */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startDiggerLib Dialog-Frame 
PROCEDURE startDiggerLib :
/*------------------------------------------------------------------------
  Name         : startDiggerLib
  Description  : Start DiggerLib if it has not already been started

  ----------------------------------------------------------------------
  21-10-2009 pti Created
  ----------------------------------------------------------------------*/
  
  define variable hDiggerLib   as handle      no-undo.
  define variable hProcedure   as handle      no-undo.
  define variable cProgDir     as character   no-undo.

  /* Call out to see if the lib has been started */
  publish 'DiggerLib' (output hProcedure). 

  if not valid-handle(hProcedure) then
  do:
    file-info:file-name = this-procedure:file-name.
    cProgDir = substring(file-info:full-pathname,1,r-index(file-info:full-pathname,'\')).
    if '{&uib_is_running}' <> '' then cProgDir = 'd:\data\progress\datadigger\'.

    run value(cProgDir + 'DataDiggerLib.p') persistent set hProcedure.
    session:add-super-procedure(hProcedure,search-target).
  end.

end procedure. /* startDiggerLib */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
