&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frDataSettings

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 fiFileContents cbSeparator ~
fiTimestamp fiTargetDatabase cbDateFormat fiYearOffset fiNumRecords ~
fiTargetTable cbNumericFormat fiFileSize 
&Scoped-Define DISPLAYED-OBJECTS fiDataFile fiFileContents cbSeparator ~
fiTimestamp fiTargetDatabase cbDateFormat fiYearOffset fiNumRecords ~
fiTargetTable cbNumericFormat fiFileSize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cbDateFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "D&ate Format" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     DROP-DOWN-LIST
     SIZE-PIXELS 140 BY 21 TOOLTIP "-D" NO-UNDO.

DEFINE VARIABLE cbNumericFormat AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "&Numeric Format" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE-PIXELS 140 BY 21 TOOLTIP "-E" NO-UNDO.

DEFINE VARIABLE cbSeparator AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Field Separator" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "",0,
                     "Space",1,
                     "Comma",2,
                     "Semicolon",3,
                     "Colon",4,
                     "Pipe",5
     DROP-DOWN-LIST
     SIZE-PIXELS 140 BY 21 TOOLTIP "the character that is used to separate the fields" NO-UNDO.

DEFINE VARIABLE fiDataFile AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 10
     SIMPLE
     SIZE-PIXELS 715 BY 23 TOOLTIP "the file to load" NO-UNDO.

DEFINE VARIABLE fiFileContents AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE-PIXELS 975 BY 470
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiFileSize AS INT64 FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Data Size" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 70 BY 21 TOOLTIP "size of the file to load" NO-UNDO.

DEFINE VARIABLE fiNumRecords AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Number of records" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 70 BY 21 TOOLTIP "expected number of records in the file" NO-UNDO.

DEFINE VARIABLE fiTargetDatabase AS CHARACTER FORMAT "X(256)":U 
     LABEL "Target Database" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 140 BY 21 TOOLTIP "target database" NO-UNDO.

DEFINE VARIABLE fiTargetTable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Target Table" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 140 BY 21 TOOLTIP "target table" NO-UNDO.

DEFINE VARIABLE fiTimestamp AS CHARACTER FORMAT "X(256)":U 
     LABEL "File Date/Time" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 140 BY 21 TOOLTIP "date / time dumped"
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiYearOffset AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "&Year Offset" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 35 BY 21 TOOLTIP "-YY" NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 195.6 BY 4.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frDataSettings
     fiDataFile AT Y 0 X 0 NO-LABEL WIDGET-ID 18
     fiFileContents AT Y 25 X 0 NO-LABEL WIDGET-ID 20
     cbSeparator AT Y 510 X 90 COLON-ALIGNED WIDGET-ID 92
     fiTimestamp AT Y 510 X 470 COLON-ALIGNED WIDGET-ID 104
     fiTargetDatabase AT Y 510 X 755 COLON-ALIGNED WIDGET-ID 96
     cbDateFormat AT Y 535 X 90 COLON-ALIGNED WIDGET-ID 50
     fiYearOffset AT Y 535 X 305 COLON-ALIGNED WIDGET-ID 60
     fiNumRecords AT Y 535 X 470 COLON-ALIGNED WIDGET-ID 94
     fiTargetTable AT Y 535 X 755 COLON-ALIGNED WIDGET-ID 98
     cbNumericFormat AT Y 560 X 90 COLON-ALIGNED WIDGET-ID 48
     fiFileSize AT Y 560 X 470 COLON-ALIGNED WIDGET-ID 102
     "bytes" VIEW-AS TEXT
          SIZE 6.4 BY .62 AT ROW 27.91 COL 112 WIDGET-ID 106
     RECT-3 AT ROW 24.81 COL 1 WIDGET-ID 116
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 0 Y 0
         SIZE-PIXELS 978 BY 599 WIDGET-ID 100.


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
         HEIGHT-P           = 599
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
/* SETTINGS FOR COMBO-BOX fiDataFile IN FRAME frDataSettings
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiFileContents:READ-ONLY IN FRAME frDataSettings        = TRUE.

ASSIGN 
       fiFileSize:READ-ONLY IN FRAME frDataSettings        = TRUE.

ASSIGN 
       fiNumRecords:READ-ONLY IN FRAME frDataSettings        = TRUE.

ASSIGN 
       fiTargetDatabase:READ-ONLY IN FRAME frDataSettings        = TRUE.

ASSIGN 
       fiTargetTable:READ-ONLY IN FRAME frDataSettings        = TRUE.

ASSIGN 
       fiTimestamp:READ-ONLY IN FRAME frDataSettings        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

DELETE WIDGET  {&WINDOW-NAME}.
{&WINDOW-NAME} = CURRENT-WINDOW.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i}
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

    {Advantzware/WinKit/embedfinalize-nonadm.i}
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

  do with frame {&frame-name}:
    setData('TargetDatabase', fiTargetDatabase:screen-value).
    setData('TargetTable', fiTargetTable:screen-value).   
  end.

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
  DISPLAY fiDataFile fiFileContents cbSeparator fiTimestamp fiTargetDatabase 
          cbDateFormat fiYearOffset fiNumRecords fiTargetTable cbNumericFormat 
          fiFileSize 
      WITH FRAME frDataSettings IN WINDOW C-Win.
  ENABLE RECT-3 fiFileContents cbSeparator fiTimestamp fiTargetDatabase 
         cbDateFormat fiYearOffset fiNumRecords fiTargetTable cbNumericFormat 
         fiFileSize 
      WITH FRAME frDataSettings IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frDataSettings}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshFrame C-Win 
PROCEDURE refreshFrame :
define variable cLine          as character   no-undo.
  define variable cYearOffset    as character   no-undo.
  define variable cDateFormat    as character   no-undo.
  define variable cNumSeparator  as character   no-undo.
  define variable cDecimalPoint  as character   no-undo.
  define variable cFilename      as character   no-undo.
  define variable cFileData      as longchar    no-undo.
  define variable iLine          as integer     no-undo.
  define variable iActualRecords as integer     no-undo. 
  define variable iNumRecords    as integer     no-undo. 
  define variable cField         as character   no-undo extent 1000.
  define variable cTimeStamp     as character   no-undo. 
  define variable cOldDateFormat as character   no-undo.
  define variable cActualTimeStamp as character no-undo. 
  define variable iFileSize       as int64      no-undo.
  define variable iActualFileSize as int64      no-undo.

  define variable cTargetDatabase as character no-undo.
  define variable cTargetTable    as character no-undo.

  /* Get filename */
  cFilename = getData('DataFile').
  do with frame frDataSettings:
    fiDataFile:screen-value = cFilename.
    fiFileContents:screen-value = "".
    fiFileContents:insert-file(cFilename).
  end.

  /* Add current setting to combo for numeric format */
  cbNumericFormat:delimiter = '|'.
  cbNumericFormat:list-item-pairs = substitute('American    (1,003.14)|American|European   (1.003,14)|European|Session (&1)|&1', session:numeric-format).
  cbNumericFormat = session:numeric-format. 

  /* Add current setting to combo for date format */
  cbDateFormat:list-item-pairs = substitute('DMY (18-3-2010),DMY,MDY (3-18-2010),MDY,YMD (2010-3-18),YMD,YDM (2010-18-3),YDM,MYD (3-2010-18),MYD,DYM (18-2010-3),DYM,Session (&1),&1', session:date-format).
  cbDateFormat = session:date-format. 

  /* year offset / target */
  fiYearOffset:screen-value     = string(session:year-offset).
  fiTargetDatabase:screen-value = getData('TargetDatabase').
  fiTargetTable:screen-value    = getData('TargetTable').   

  /* If it is an XML file, set to hard values for separator etc */
  if entry(num-entries(cFilename,'.'),cFilename,'.') = 'XML' then
  do:
    cbSeparator:screen-value     = ''.
    fiYearOffset:screen-value    = string(session:year-offset).
    cbDateFormat:screen-value    = 'YMD'.
    cbNumericFormat:screen-value = 'American'.
    return.
  end.

  /* Otherwise, if it's a .d file, read it from the file */
  if entry(num-entries(cFilename,'.'),cFilename,'.') = 'D' then
  do:
    /* Separator is space for .d files */
    cbSeparator:screen-value     = '1'.

    /* Read file and count nr of records 
     * A regular import will read until it finds either the end of the file 
     * or a single dot. The trailer (if present) starts with a single dot.
     */
    input from value(cFilename).
    repeat:
      import cField.
      iActualRecords = iActualRecords + 1.
    end.
    setData('NumRecords',string(iActualRecords)).

    /* The size up to the trailer is important, so save that */
    iActualFileSize = seek(input).
    input close. 

    /* Get the actual timestamp of the file */
    cOldDateFormat           = session:date-format.
    session:date-format      = 'ymd'.
    file-info:filename       = cFilename.
    cActualTimeStamp         = string(file-info:file-mod-date,'9999/99/99') + '-' + string(file-info:file-mod-time,'HH:MM:SS').
    fiTimestamp:screen-value = cActualTimeStamp.
    session:date-format      = cOldDateFormat.    

    fiNumRecords:screen-value     = string(iActualRecords).
    fiFileSize:screen-value       = string(iActualFileSize).

    /* Actually, we would like to read the trailer, but when trailer is a custom 
     * written trailer, we might see a last line without a return or periods as a last char.
     */
    copy-lob file cFilename to cFileData. 

    /* Remove the data part from cFileData */
    substring(cFileData,1,iActualFileSize) = "".

    /* Read the trailer:
      .
      PSC
      filename=customer
      records=0000000000012
      ldbname=sports
      timestamp=2010/03/17-10:44:08
      numformat=44,46
      dateformat=dmy-1960
      map=NO-MAP
      cpstream=1252
      .
     */
    seekSettings:
    do iLine = 1 to num-entries(cFileData,'~n'):

      cLine = entry(iLine,cFileData,'~n').

      /* Local strangeness: extra . as last character. 
       * But don't strip it if it is the only char on the line 
       */
      if cLine <> '.' then cLine = trim(cLine,'.').
      cLine = trim(cLine,chr(13)).

      if cLine begins 'ldbname='   then assign cTargetDatabase = entry(2,cLine,'=').
      if cLine begins 'filename='  then assign cTargetTable    = entry(2,cLine,'=').
      if cLine begins 'records='   then assign iNumRecords     = integer(entry(2,cLine,'=')).
      if cLine begins 'timestamp=' then assign cTimeStamp      = entry(2,cLine,'=').
      if cLine begins 'numformat=' then assign cNumSeparator   = entry(1, entry(2,cLine,'=') )
                                               cDecimalPoint   = entry(2, entry(2,cLine,'=') ).

      /* dateformat=dmy-1960 */
      if cLine begins 'dateformat=' 
        and num-entries( entry(2,cLine,'='), '-') = 2 then
        assign 
          cDateFormat = entry(1, entry(2,cLine,'=') ,'-')
          cYearOffset = entry(2, entry(2,cLine,'=') ,'-').

      /* Filesize is reported as-is on its own */
      if num-entries(cLine,'=') <> 2 
        and cLine <> "" 
        and iFileSize = 0 then 
      do:
        assign iFileSize = int64(cLine) no-error.
        if error-status:error then iFileSize = 0.
      end.

    end. /* seekSettings */


    /* Set date format + year offset */
    if lookup(cDateFormat, cbDateFormat:list-item-pairs) > 0 then 
      cbDateFormat:screen-value = cDateFormat.
    if cYearOffset <> "" then
      fiYearOffset:screen-value = cYearOffset.

    /* Translate back NumSeparator and DecimalPoint from ASC to readable format */
    if cNumSeparator = '44' and cDecimalPoint = '46' /* , . */ then cbNumericFormat:screen-value = 'European'.
    if cNumSeparator = '46' and cDecimalPoint = '44' /* . , */ then cbNumericFormat:screen-value = 'American'.


    /* Now do some checks to see if the info in the trailer is correct */
    /* - count nr of records 
     * records=0000000000012
     */
    if iNumRecords > 0
      and iActualRecords <> iNumRecords then
    do:
      fiNumRecords:bgcolor = 12. /* red */
      fiNumRecords:tooltip = substitute("Trailer indicated &1 records, but &2 were found"
                                       , iNumRecords
                                       , iActualRecords
                                       ).
    end.
    else 
      assign fiNumRecords:bgcolor = ?
             fiNumRecords:tooltip = "".

    /* - check timestamp 
     * timestamp=2010/03/17-10:44:08
     */
    if cTimeStamp <> "" then
    do:
      if cTimeStamp <> cActualTimeStamp then
      do:
        fiTimestamp:bgcolor = 12. /* red */
        fiTimestamp:tooltip = substitute("Timestamp indicated &1, but file was last changed on &2"
                                        , cTimeStamp
                                        , cActualTimeStamp
                                        ).
      end.
      else 
        assign fiTimestamp:bgcolor = ?
               fiTimestamp:tooltip = "".
    end.

    /* Actual file size */
    if iFileSize <> 0 and iFileSize <> iActualFileSize then
    do:
      fiFileSize:bgcolor = 12. /* red */
      fiFileSize:tooltip = substitute("Filesize indicated &1, but actual file size was &2"
                                       , iFileSize
                                       , iActualFileSize
                                       ).
    end.
    else assign fiFileSize:bgcolor = ?
                fiFileSize:tooltip = "".


    /* Target database */
    if fiTargetDatabase:screen-value = "" then fiTargetDatabase:screen-value = cTargetDatabase.
    if cTargetDatabase <> "" and cTargetDatabase <> fiTargetDatabase:screen-value then
    do:
      fiTargetDatabase:bgcolor = 12. /* red */
      fiTargetDatabase:tooltip = substitute("Target Database indicated &1, but selected database is &2"
                                           , cTargetDatabase
                                           , fiTargetDatabase:screen-value
                                           ).
    end.
    else assign fiTargetDatabase:bgcolor = ?
                fiTargetDatabase:tooltip = "".


    /* Target Table */
    if fiTargetTable:screen-value = "" then fiTargetTable:screen-value = cTargetTable.
    if cTargetTable <> "" and cTargetTable <> fiTargetTable:screen-value then 
    do:
      fiTargetTable:bgcolor = 12. /* red */
      fiTargetTable:tooltip = substitute("Target Table indicated &1, but selected Table is &2"
                                        , cTargetTable
                                        , getData('TargetTable')
                                        ).
    end.
    else assign fiTargetTable:bgcolor = ?
                fiTargetTable:tooltip = "".

  end. /* .d file */


end procedure. /* getDataSettings */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

