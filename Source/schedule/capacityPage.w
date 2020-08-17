&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: capacityPage.w

  Description: Capacity Schedule Web Page Generation

  Input Parameters: Type: "Est" or "Job" -- Rowid: Est or Job

  Output Parameters: <none>

  Author: Ron Stark

  Created: 12.20.2017
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowID   AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cocode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAccess   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lContinue AS LOGICAL   NO-UNDO.

{schedule/objects/includes/scheduleTempTables.i}

SESSION:SET-WAIT-STATE ("").

/* check for valid license */
RUN util/CheckModule.p ("ASI", "sbHTML", YES, OUTPUT lAccess).
IF NOT lAccess THEN RETURN.

/* get nk1 setting */
cocode = ipcCompany.
DO TRANSACTION:
    {sys/inc/capacityPage.i}  
END.
IF cCapacityPage EQ "No" THEN DO:
    MESSAGE 
        "Schedule Capacity Page Generatiion is not Enabled." SKIP 
        "See System Administrator for Assistance."
    VIEW-AS ALERT-BOX.
    RETURN.
END. /* if no */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME ttJob

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttJob ttMachine

/* Definitions for BROWSE ttJob                                         */
&Scoped-define FIELDS-IN-QUERY-ttJob ttJob.m-code ttJob.m-dscr ttJob.frm ttJob.blank-no ttJob.pass ttJob.mr-hr ttJob.run-hr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttJob ttJob.frm ttJob.blank-no ttJob.pass ttJob.mr-hr ttJob.run-hr   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttJob ttJob
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttJob ttJob
&Scoped-define SELF-NAME ttJob
&Scoped-define QUERY-STRING-ttJob FOR EACH ttJob
&Scoped-define OPEN-QUERY-ttJob OPEN QUERY {&SELF-NAME} FOR EACH ttJob.
&Scoped-define TABLES-IN-QUERY-ttJob ttJob
&Scoped-define FIRST-TABLE-IN-QUERY-ttJob ttJob

/* Definitions for BROWSE ttMachine                                     */
&Scoped-define FIELDS-IN-QUERY-ttMachine ttMachine.m-code ttMachine.m-dscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttMachine   
&Scoped-define SELF-NAME ttMachine
&Scoped-define QUERY-STRING-ttMachine FOR EACH ttMachine
&Scoped-define OPEN-QUERY-ttMachine OPEN QUERY {&SELF-NAME} FOR EACH ttMachine.
&Scoped-define TABLES-IN-QUERY-ttMachine ttMachine
&Scoped-define FIRST-TABLE-IN-QUERY-ttMachine ttMachine

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-ttJob}~
    ~{&OPEN-QUERY-ttMachine}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClear ttMachine ttJob btnExit btnOK ~
btnRemove btnReset btnSort 
&Scoped-Define DISPLAYED-OBJECTS baseOnText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClear 
     IMAGE-UP FILE "Graphics/32x32/error.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Clear" 
     SIZE 8 BY 1.91 TOOLTIP "Clear".

DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit".

DEFINE BUTTON btnOK 
     IMAGE-UP FILE "Graphics/32x32/html_tag.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "Generate Page".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/32x32/delete.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Remove" 
     SIZE 8 BY 1.91 TOOLTIP "Remove".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/nav_refresh.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnSort 
     IMAGE-UP FILE "Graphics/32x32/sort_up_down.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort" 
     SIZE 8 BY 1.91 TOOLTIP "Sort".

DEFINE VARIABLE baseOnText AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 81 BY 1
     BGCOLOR 14  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttJob FOR 
      ttJob SCROLLING.

DEFINE QUERY ttMachine FOR 
      ttMachine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttJob Dialog-Frame _FREEFORM
  QUERY ttJob DISPLAY
      ttJob.m-code LABEL "Machine" FORMAT "x(10)"
ttJob.m-dscr LABEL "Description" FORMAT "x(30)"
ttJob.frm
ttJob.blank-no LABEL "Blank"
ttJob.pass
ttJob.mr-hr LABEL "MR Hour"
ttJob.run-hr LABEL "Run Hour"
ENABLE
ttJob.frm
ttJob.blank-no
ttJob.pass
ttJob.mr-hr
ttJob.run-hr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 81 BY 32.14
         FGCOLOR 1  ROW-HEIGHT-CHARS .67.

DEFINE BROWSE ttMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttMachine Dialog-Frame _FREEFORM
  QUERY ttMachine DISPLAY
      ttMachine.m-code
ttMachine.m-dscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 33.1
         FGCOLOR 1 .

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnClear AT ROW 15.52 COL 129 WIDGET-ID 22
     baseOnText AT ROW 1 COL 47 NO-LABEL WIDGET-ID 10
     ttMachine AT ROW 1.24 COL 2 WIDGET-ID 300
     ttJob AT ROW 2.19 COL 47 WIDGET-ID 200
     btnExit AT ROW 32.43 COL 129 WIDGET-ID 6
     btnOK AT ROW 2.91 COL 129 WIDGET-ID 4
     btnRemove AT ROW 9.1 COL 129 WIDGET-ID 16
     btnReset AT ROW 12.19 COL 129 WIDGET-ID 18
     btnSort AT ROW 6 COL 129 WIDGET-ID 20
     SPACE(0.00) SKIP(26.43)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Capacity Schedule Page Generation" WIDGET-ID 100.

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
/* BROWSE-TAB ttMachine baseOnText Dialog-Frame */
/* BROWSE-TAB ttJob ttMachine Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN baseOnText IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       baseOnText:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttJob
/* Query rebuild information for BROWSE ttJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttJob.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttJob */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttMachine
/* Query rebuild information for BROWSE ttMachine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttMachine.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttMachine */
&ANALYZE-RESUME

/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Capacity Schedule Page Generation */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear Dialog-Frame
ON CHOOSE OF btnClear IN FRAME Dialog-Frame /* Clear */
DO:
    EMPTY TEMP-TABLE ttJob.
    {&OPEN-QUERY-ttJob}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    SESSION:SET-WAIT-STATE ("General").
    RUN pScheduleJob (iprRowID).
/*    RUN pHTMLPageHorizontal.*/
    RUN pHTMLPageVertical.
    SESSION:SET-WAIT-STATE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove Dialog-Frame
ON CHOOSE OF btnRemove IN FRAME Dialog-Frame /* Remove */
DO:
    APPLY "DEFAULT-ACTION":U TO BROWSE ttJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:
    RUN pBuildTTJob (ipcType, ipcCompany, iprRowID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSort Dialog-Frame
ON CHOOSE OF btnSort IN FRAME Dialog-Frame /* Sort */
DO:
    {&OPEN-QUERY-ttJob}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME ttJob
&Scoped-define SELF-NAME ttJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttJob Dialog-Frame
ON DEFAULT-ACTION OF ttJob IN FRAME Dialog-Frame
DO:
    IF AVAILABLE ttJob THEN DO:
        DELETE ttJob.
        {&OPEN-QUERY-ttJob}
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME ttMachine
&Scoped-define SELF-NAME ttMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttMachine Dialog-Frame
ON DEFAULT-ACTION OF ttMachine IN FRAME Dialog-Frame
DO:
    RUN pAddMachine.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME ttJob
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 

/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pLoadDowntime.
  RUN pBuildTTMachine.
  RUN pBuildTTJob (ipcType, ipcCompany, iprRowID).
  RUN pRemoveUnusedDowntime.
  RUN enable_UI.
  DISPLAY baseOnText WITH FRAME {&FRAME-NAME}.
  IF cErrorMsg NE "" THEN
  MESSAGE 
      cErrorMsg
  VIEW-AS ALERT-BOX WARNING.
  /* fatal error, bail */
  IF NOT lContinue THEN RETURN.
  IF cCapacityPage EQ "Yes" THEN DO: 
      APPLY "CHOOSE":U TO btnOK.
      RETURN.
  END. /* if yes */
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{schedule/objects/includes/scheduleProcs.i}

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
  DISPLAY baseOnText 
      WITH FRAME Dialog-Frame.
  ENABLE btnClear ttMachine ttJob btnExit btnOK btnRemove btnReset btnSort 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddMachine Dialog-Frame 
PROCEDURE pAddMachine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.
    
    CREATE ttJob.
    ASSIGN
        ttJob.m-code   = ttMachine.m-code
        ttJob.m-dscr   = ttMachine.m-dscr
        ttJob.d-seq    = ttMachine.d-seq
        ttJob.m-seq    = ttMachine.m-seq
        rRowID         = ROWID(ttJob)
        .
    {&OPEN-QUERY-ttJob}
    QUERY ttJob:REPOSITION-TO-ROWID(rRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLPageHorizontal Dialog-Frame 
PROCEDURE pHTMLPageHorizontal :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    
    DEFINE VARIABLE cDays       AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".    
    DEFINE VARIABLE lAltLine    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cBGColor    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDate AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dtDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE iJobs       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cType1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPercentage AS DECIMAL   NO-UNDO.
    
    FIND FIRST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate
         NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN RETURN.
    dtStartDate = ttblJob.startDate.
    FIND LAST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate.
    dtEndDate = ttblJob.endDate + 1.
    OUTPUT TO "c:\tmp\sbHTML.htm".
    PUT UNFORMATTED
        '<html>' SKIP
        '<head>' SKIP
        '<title>Schedule ' baseOnText '</title>' SKIP
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
        '</head>' SKIP
        '<a name="Top"></a>' SKIP
        '<form>' SKIP
        '<fieldset>' SKIP
        '  <legend><font face="{&fontFace}"><b>Schedule ' baseOnText '</b> (generated '
        STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
        '~&nbsp;</legend>' SKIP
        '  <img src="' SEARCH("Graphics/32x32/asiicon.png")
        '" align="middle">~&nbsp;<b><a href="http://www.advantzware.com" target="_blank">'
        '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
        '~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;<font face="{&fontFace}"><font color="#FF0000"><b>Projected Completion: </font>'
        ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am")
        '</font></b>' SKIP 
        '  <table align="right" cellspacing="2" cellpadding="8">' SKIP
        '    <tr>' SKIP 
        '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
        '      <td bgcolor="#00CCFF"><font face="{&fontFace}"><b>' baseOnText '</b></font></td>' SKIP  
        '      <td bgcolor="#C0BEBE"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
        '      <td bgcolor="#AAD5B9"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
        '      <td bgcolor="#F1FE98"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
        '    </tr>' SKIP  
        '  </table>' SKIP 
        '  <table border="1" cellspacing="0" cellpadding="5" width="100%">' SKIP
        '    <tr>' SKIP
        '      <td bgcolor="#F4F4F4" align="center" nowrap><font face="{&fontFace}"><b>'
        'Operation</b></font></td>' SKIP
        .
        DO dtDate = dtStartDate TO dtEndDate:
            PUT UNFORMATTED
                '      <td bgcolor="#F4F4F4" align="center" nowrap><font face="{&fontFace}">'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</font></td>' SKIP
                .
        END. /* do dtdate */
    PUT UNFORMATTED '    </tr>' SKIP.
    
    FOR EACH ttblJob
        WHERE ttblJob.newJob EQ YES
        BREAK BY ttblJob.startDateTime
              BY ttblJob.m-code
        :
        PUT UNFORMATTED
            '    <tr>' SKIP
            '      <td' cBGColor ' align="left" nowrap WIDTH="250"><font face="{&fontFace}">'
            '<img src="'
            (IF SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") NE ? THEN
                SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") ELSE
                SEARCH("Graphics/48x48/gearwheels.png"))
            '" width="48" height="48" align="left">~&nbsp~&nbsp~&nbsp~&nbsp<b>'
            ttblJob.m-code '</b> (f:<b>' ttblJob.frm
            '</b> b:<b>' ttblJob.blank-no
            '</b> p:<b>' ttblJob.pass ')</b><br>'
            ttblJob.startDate ' - ' STRING(ttblJob.startTime,"hh:mm:ss am") '<br>'
            ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am") '</font></td>' SKIP
            .
        DO dtDate = dtStartDate TO dtEndDate:
            EMPTY TEMP-TABLE ttTime.
            iJobs = 0.
            FOR EACH bTtblJob
                WHERE bTtblJob.m-code     EQ ttblJob.m-code
                  AND (bTtblJob.startDate EQ dtDate
                   OR (bTtblJob.startDate LT dtDate
                  AND  bTtbljob.endDate   GT dtDate)
                   OR  bTtblJob.endDate   EQ dtDate)
                BY bTtblJob.startDateTime
                :
                iStartTime = IF bTtblJob.startDate EQ dtDate THEN bTtblJob.startTime ELSE 0.
                fTimeSlice ("",dtDate,iStartTime,"Job","Start",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                iEndTime = IF bTtblJob.endDate EQ dtDate THEN bTtblJob.endTime ELSE 86400.
                fTimeSlice ("",dtDate,iEndTime,"Job","End",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                iJobs = iJobs + 1.
            END. /* each bttbljob */
            FOR EACH ttblDowntime
                WHERE  ttblDowntime.dayID     EQ WEEKDAY(dtDate)
                  AND (ttblDowntime.resource  EQ "<Calendar>"
                   OR  ttblDowntime.resource  EQ ttblJob.m-code)
                  AND (ttblDowntime.startDate EQ dtDate
                   OR  ttblDowntime.startDate EQ ?)
                :
                fTimeSlice ("",dtDate,ttblDowntime.startTime,"DT","Start",NO).
                fTimeSlice ("",dtDate,ttblDowntime.endTime,"DT","End",NO).
            END. /* each ttbldowntime */
            fTimeSlice ("",dtDate,0,"Avail","Start",NO).
            fTimeSlice ("",dtDate,86400,"Avail","End",NO).
            PUT UNFORMATTED
                '      <td' cBGColor ' align="center" nowrap><font face="{&fontFace}">' SKIP
                '        <table border="1" cellspacing="0" cellpadding="8" width="100%">' SKIP
                '          <tr>' SKIP  
                .
            FOR EACH ttTime
                BY ttTime.timeSlice
                :
                IF ttTime.timeSlice EQ 0 THEN DO:
                    ASSIGN
                        iTime  = 0
                        cType1 = ttTime.timeType1
                        cType2 = ttTime.timeType2                        
                        .
                    NEXT.
                END. /* timeslice eq 0 */
                IF ttTime.timeType1 EQ "DT"   AND 
                   ttTime.timeType1 NE cType1 AND
                   ttTime.timeType2 NE cType2 THEN
                cType1 = "Avail".
                ELSE IF ttTime.timeType1 EQ "DT"    AND 
                        ttTime.timeType2 EQ "Start" AND
                        cType2           EQ "End"   THEN
                cType1 = "Avail".
                ELSE IF ttTime.timeType2 NE "Start" OR cType2 NE "Start" THEN
                cType1 = ttTime.timeType1.
                dPercentage = ROUND((ttTime.timeSlice - iTime) / 86400 * 100,0).
                IF dPercentage GT 0 THEN
                PUT UNFORMATTED
                    '            <td bgcolor="#'
                    (IF ttTime.newJob AND ttTime.timeType2 EQ "End" THEN "00CCFF" ELSE
                     IF cType1 EQ "Avail" THEN "F1FE98" ELSE
                     IF cType1 EQ "Job"   THEN "AAD5B9" ELSE "C0BEBE")
                    '" align="center" width="' dPercentage '%" nowrap>~&nbsp;'
                    '</td>' SKIP
                    .
                ASSIGN
                    iTime  = ttTime.timeSlice
                    cType1 = ttTime.timeType1
                    cType2 = ttTime.timeType2
                    .
            END. /* each tttime */
            PUT UNFORMATTED
                '          </tr>' SKIP 
                '        </table>' SKIP 
                '      </font></td>' SKIP
                .
        END. /* do dtdate */
        PUT UNFORMATTED
            '    </tr>' SKIP
            .
        ASSIGN
            lAltLine = NOT lAltLine
            cBGColor = IF lAltLine THEN ' bgcolor="EEDFD2"' ELSE ''
            .
    END. /* each ttbljob */
    PUT UNFORMATTED
        '  </table>' SKIP
        '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
        '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
        '</fieldset>' SKIP
        '</form>' SKIP
        '</html>' SKIP
        .
    OUTPUT CLOSE.
    OS-COMMAND NO-WAIT START "c:\tmp\sbHTML.htm".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLPageVertical Dialog-Frame
PROCEDURE pHTMLPageVertical:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    &Scoped-define fontFace Comic Sans MS
    &Scoped-define fontFace Tahoma
    
    DEFINE VARIABLE cBGColor    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDays       AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
    DEFINE VARIABLE cHTMLPage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKey        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMachines   AS CHARACTER NO-UNDO EXTENT 200.
    DEFINE VARIABLE cPageTitle  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPercentage AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dtStartDate AS DATE      NO-UNDO.
    DEFINE VARIABLE iDays       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobs       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lAltLine    AS LOGICAL   NO-UNDO.
    
    FIND FIRST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate
         NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN RETURN.
    dtStartDate = TODAY.
    FIND LAST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate.
    ASSIGN
        dtEndDate = ttblJob.endDate + 1
        iDays     = dtEndDate - dtStartDate + 1
        cMachines = ""
        .
    EMPTY TEMP-TABLE ttTime.
    FOR EACH ttblJob
        WHERE ttblJob.newJob EQ YES
        BREAK BY ttblJob.startDateTime
              BY ttblJob.m-code
        :
        ASSIGN
            idx            = idx + 1
            cMachines[idx] = ttblJob.m-code + ","
                           + STRING(ttblJob.frm) + ","
                           + STRING(ttblJob.blank-no) + ","
                           + STRING(ttblJob.pass)
                           .
        DO dtDate = dtStartDate TO dtEndDate:
            FOR EACH ttblDowntime
                WHERE  ttblDowntime.dayID     EQ WEEKDAY(dtDate)
                  AND (ttblDowntime.resource  EQ "<Calendar>"
                   OR  ttblDowntime.resource  EQ ttblJob.m-code)
                  AND (ttblDowntime.startDate EQ dtDate
                   OR  ttblDowntime.startDate EQ ?)
                :
                fTimeSlice (cMachines[idx],dtDate,ttblDowntime.startTime,"DT","Start",NO).
                fTimeSlice (cMachines[idx],dtDate,ttblDowntime.endTime,  "DT","End",  NO).
            END. /* each ttbldowntime */
            iJobs = 0.
            FOR EACH bTtblJob
                WHERE bTtblJob.m-code     EQ ttblJob.m-code
                  AND (bTtblJob.startDate EQ dtDate
                   OR (bTtblJob.startDate LT dtDate
                  AND  bTtbljob.endDate   GT dtDate)
                   OR  bTtblJob.endDate   EQ dtDate)
                BY bTtblJob.startDateTime
                :
                ASSIGN
                    iStartTime = IF bTtblJob.startDate EQ dtDate THEN bTtblJob.startTime ELSE 0
                    iEndTime   = IF bTtblJob.endDate   EQ dtDate THEN bTtblJob.endTime   ELSE 86400
                    iJobs      = iJobs + 1
                    .
                fTimeSlice (cMachines[idx],dtDate,iStartTime,"Job","Start",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                fTimeSlice (cMachines[idx],dtDate,iEndTime,  "Job","End",  ROWID(bTtblJob) EQ ROWID(ttblJob)).                
            END. /* each bttbljob */
            fTimeSlice (cMachines[idx],dtDate,0,    "Avail","Start",NO).
            fTimeSlice (cMachines[idx],dtDate,86400,"Avail","End",  NO).
        END. /* do dtdate */
    END. /* each ttbljob */
    
    DO jdx = 1 TO 3:
        CASE jdx:
            WHEN 1 THEN
            cPageTitle = " by Time".
            WHEN 2 THEN
            cPageTitle = " by Percentage".
            WHEN 3 THEN
            cPageTitle = " by Time / Percentage".
        END CASE.
        cHTMLPage = "c:\tmp\sbHTMLProjected" + STRING(jdx) + ".htm".
        OUTPUT TO VALUE(cHTMLPage).
        PUT UNFORMATTED
            '<html>' SKIP
            '<head>' SKIP
            '<title>Schedule ' baseOnText '</title>' SKIP
            '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
            '</head>' SKIP
            '<a name="Top"></a>' SKIP
            '<form>' SKIP
            '<fieldset>' SKIP
            '  <legend><font face="{&fontFace}"><b>Schedule ' baseOnText cPageTitle '</b> (generated '
            STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
            '~&nbsp;</legend>' SKIP
            '  <img src="' SEARCH("Graphics/32x32/asiicon.png") '" align="middle">~&nbsp;'
            '<b><a href="http://www.advantzware.com" target="_blank">'
            '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
            '~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;<font face="{&fontFace}"><font color="#FF0000"><b>Projected Completion: </font>'
            ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am")
            '</font></b>' SKIP 
            '  <table align="center" cellspacing="2" cellpadding="8">' SKIP
            '    <tr>' SKIP 
            '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
            '      <td bgcolor="#85FEFE"><font face="{&fontFace}"><b>' baseOnText '</b></font></td>' SKIP  
            '      <td bgcolor="#FF8585"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
            '      <td bgcolor="#A1A5E2"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
            '      <td bgcolor="#97F3A0"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
            '    </tr>' SKIP  
            '  </table>' SKIP 
            '  <table border="1" cellspacing="0" cellpadding="0" width="100%" height="80%" style="border-color: white">' SKIP
            .
        RUN pOutputResources.
        DO dtDate = dtStartDate TO dtEndDate:
            PUT UNFORMATTED
                '    <tr style="height: ' INTEGER(90 / iDays) '%;">' SKIP
                '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF">'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</font></td>' SKIP
                .
            DO idx = 1 TO EXTENT(cMachines):
                IF cMachines[idx] EQ "" THEN LEAVE.
                PUT UNFORMATTED
                    '      <td style="padding: 5px">' SKIP
                    '        <table border="1" cellspacing="0" cellpadding="0" align="center" width="70%" height="140px">' SKIP
                    .
                FOR EACH ttTime
                    WHERE ttTime.timeKey  EQ cMachines[idx]
                      AND ttTime.timeDate EQ dtDate
                       BY ttTime.timeSlice DESCENDING
                       BY ttTime.timeType2 DESCENDING
                    :
                    IF ttTime.timeSlice EQ 86400 THEN DO:
                        ASSIGN
                            iTime  = 86400
                            cType1 = ttTime.timeType1
                            cType2 = ttTime.timeType2                        
                            .
                        NEXT.
                    END. /* timeslice eq 86400 */
    
                    IF ttTime.timeType1 EQ "DT"   AND 
                       ttTime.timeType1 NE cType1 AND
                       ttTime.timeType2 NE cType2 THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType1 EQ "DT"    AND
                            ttTime.timeType2 EQ "End"   AND
                            cType2           EQ "Start" THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType1 NE cType1 AND
                            ttTime.timeType2 NE cType2 THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType2 NE "End" OR cType2 NE "End" THEN
                    cType1 = ttTime.timeType1.
    
                    dPercentage = ROUND((iTime - ttTime.timeSlice) / 86400 * 100,2).
                    IF dPercentage GT 0 THEN DO:
                        PUT UNFORMATTED
                            '          <tr style="height: ' dPercentage '%;">' SKIP
                            '            <td bgcolor="#'
                            (IF ttTime.newJob AND ttTime.timeType2 EQ "Start" THEN "85FEFE" ELSE
                             IF cType1 EQ "Avail" THEN "97F3A0" ELSE
                             IF cType1 EQ "Job"   THEN "A1A5E2" ELSE "FF8585")
                            '" align="center" nowrap><font face="{&fontFace}">'
                            .
                        CASE jdx:
                            WHEN 1 THEN
                                IF dPercentage EQ 100 THEN
                                PUT UNFORMATTED "24:00:00".
                                ELSE
                                PUT UNFORMATTED STRING(iTime - ttTime.timeSlice,"hh:mm:ss").
                            WHEN 2 THEN
                                PUT UNFORMATTED dPercentage '%'.
                            WHEN 3 THEN DO:
                                IF dPercentage EQ 100 THEN
                                PUT UNFORMATTED "24:00:00".
                                ELSE
                                PUT UNFORMATTED STRING(iTime - ttTime.timeSlice,"hh:mm:ss").
                                PUT UNFORMATTED ' / ' dPercentage '%'.
                            END. /* 5 or 6 */
                        END CASE.
                        PUT UNFORMATTED 
                            '</font></td>' SKIP
                            '          </tr>' SKIP
                            .
                    END. /* if gt 0 */
                    ASSIGN
                        iTime  = ttTime.timeSlice
                        cType1 = ttTime.timeType1
                        cType2 = ttTime.timeType2
                        .
                END. /* each tttime */
                PUT UNFORMATTED
                    '        </table>' SKIP 
                    '      </td>' SKIP
                    .
            END. /* do idx */
            PUT UNFORMATTED
                '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF">'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</font></td>' SKIP
                '    </tr>' SKIP
                .
        END. /* do dtdate */
        RUN pOutputResources.
        PUT UNFORMATTED
            '  </table>' SKIP
            '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
            '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
            '  <table align="center" cellspacing="2" cellpadding="8">' SKIP
            '    <tr>' SKIP 
            '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
            '      <td bgcolor="#85FEFE"><font face="{&fontFace}"><b>' baseOnText '</b></font></td>' SKIP  
            '      <td bgcolor="#FF8585"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
            '      <td bgcolor="#A1A5E2"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
            '      <td bgcolor="#97F3A0"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
            '    </tr>' SKIP  
            '  </table>' SKIP 
            '</fieldset>' SKIP
            '</form>' SKIP
            '</html>' SKIP
            .
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT START VALUE(cHTMLPage).
        PAUSE 1 NO-MESSAGE.
    END. /* do jdx */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutputResources Dialog-Frame
PROCEDURE pOutputResources:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    PUT UNFORMATTED    
        '    <tr>' SKIP
        '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
        'Operation</b></font></td>' SKIP
        .
    FOR EACH ttblJob
        WHERE ttblJob.newJob EQ YES
        BREAK BY ttblJob.startDateTime
              BY ttblJob.m-code
        :
        PUT UNFORMATTED
            '      <td bgcolor="#576490" align="left" nowrap><font face="{&fontFace}" color="#FFFFFF">'
            '<img src="'
            (IF SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") NE ? THEN
                SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") ELSE
                SEARCH("Graphics/48x48/gearwheels.png"))
            '" width="48" height="48" align="left">~&nbsp<b>'
            ttblJob.m-code '</b> (Form:<b>' ttblJob.frm
            '</b> Blank:<b>' ttblJob.blank-no
            '</b> Pass:<b>' ttblJob.pass ')</b><br>'
            ttblJob.startDate ' - ' STRING(ttblJob.startTime,"hh:mm:ss am") '<br>'
            ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am") '</font></td>' SKIP
            .
    END. /* each ttbljob */
    PUT UNFORMATTED
        '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
        'Operation</b></font></td>' SKIP
        '    </tr>' SKIP
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */
