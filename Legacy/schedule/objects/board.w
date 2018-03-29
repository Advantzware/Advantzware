&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: board.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  
  Created: 5.28.2001

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

&SCOPED-DEFINE colorJobTable ttblJob

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{{&includes}/sharedVars.i NEW}
{{&includes}/filterVars.i NEW}

DEFINE VARIABLE accumTimeInterval AS INTEGER NO-UNDO.
DEFINE VARIABLE autoMonitorInterval AS INTEGER NO-UNDO.
DEFINE VARIABLE capacityViewHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE containerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE currentOrder AS INTEGER NO-UNDO.
DEFINE VARIABLE currentResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE downtimeHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE endMove AS LOGICAL NO-UNDO.
DEFINE VARIABLE hourMode AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE justOpened AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE openBoard AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE pending AS CHARACTER NO-UNDO.
DEFINE VARIABLE pendingHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE pendingJobsHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE resourceHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE runLoad AS LOGICAL NO-UNDO.
DEFINE VARIABLE statusHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE saveFullBoard AS LOGICAL NO-UNDO.
DEFINE VARIABLE saveGridColor AS INTEGER NO-UNDO.
DEFINE VARIABLE saveDowntimeTop AS LOGICAL NO-UNDO.
DEFINE VARIABLE schdChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE jobSequencerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE startTimeOK AS LOGICAL NO-UNDO.
DEFINE VARIABLE viewRefreshInterval AS DECIMAL NO-UNDO.
DEFINE VARIABLE iMsg AS INTEGER NO-UNDO.
DEFINE VARIABLE hMsgImage AS HANDLE NO-UNDO EXTENT 23.
DEFINE VARIABLE hMsgText AS HANDLE NO-UNDO EXTENT 23.

/* buildGridArray & buildPixel Array vars */
DEFINE VARIABLE dateTimePixel AS DECIMAL NO-UNDO EXTENT 2000.
DEFINE VARIABLE intColor AS INTEGER NO-UNDO EXTENT 25.
DEFINE VARIABLE intDate AS DATE NO-UNDO EXTENT 25.
DEFINE VARIABLE intEtime AS INTEGER NO-UNDO EXTENT 25.
DEFINE VARIABLE intStime AS INTEGER NO-UNDO EXTENT 25.
DEFINE VARIABLE intK AS INTEGER NO-UNDO EXTENT 25.
DEFINE VARIABLE intT AS INTEGER NO-UNDO EXTENT 25.
DEFINE VARIABLE maxPixel AS INTEGER NO-UNDO.
DEFINE VARIABLE rectWidth AS INTEGER NO-UNDO.

/* downtime vars */
DEFINE VARIABLE buildDowntime AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE downtimeIdx AS INTEGER NO-UNDO.
{{&includes}/ttblWidgetDefine.i "downtimeWidget"}

/* lock widget vars */
DEFINE VARIABLE lockIdx AS INTEGER NO-UNDO.
{{&includes}/ttblWidgetDefine.i "lockWidget"}

/* note widget vars - noteIcon */
DEFINE VARIABLE noteIdx AS INTEGER NO-UNDO.
{{&includes}/ttblWidgetDefine.i "noteWidget"}

/* resource vars */
DEFINE VARIABLE resourceFirst AS INTEGER NO-UNDO.
DEFINE VARIABLE resourceFirstButton AS HANDLE NO-UNDO EXTENT 50.
DEFINE VARIABLE resourceGap AS INTEGER NO-UNDO.
DEFINE VARIABLE resourceIdx AS INTEGER NO-UNDO.
DEFINE VARIABLE resourceLast AS INTEGER NO-UNDO.
DEFINE VARIABLE resourceLastButton AS HANDLE NO-UNDO EXTENT 50.
DEFINE VARIABLE resourceWidget AS WIDGET-HANDLE NO-UNDO EXTENT 50.
DEFINE VARIABLE resourceXCoord AS INTEGER NO-UNDO.
DEFINE VARIABLE resourceYCoord AS INTEGER NO-UNDO.
DEFINE VARIABLE resource# AS INTEGER NO-UNDO.

DEFINE VARIABLE gridLine AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE timeLine AS WIDGET-HANDLE NO-UNDO.

/* job widget vars */
DEFINE VARIABLE currentJob AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE jobBrowse AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobBrowseHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE moveResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE moveResourceHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE movePendingResHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE fgJobColor AS INTEGER NO-UNDO.
DEFINE VARIABLE jobIdx AS INTEGER NO-UNDO.
{{&includes}/ttblWidgetDefine.i "jobWidget" "jobBGColor" "jobFGColor" "isSelected"}

/* 3D widget vars */
DEFINE VARIABLE threeDIdx AS INTEGER NO-UNDO.
{{&includes}/ttblWidgetDefine.i "threeDWidget"}

{{&includes}/ttblJob.i NEW}

DEFINE TEMP-TABLE bTtblResource NO-UNDO LIKE ttblResource.
CREATE bTtblResource.

{{&includes}/{&Board}/boardDefs.i}
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
{{&includes}/lockWindowUpdate.i}
&ENDIF

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

{{&loads}/resourceListDef.i}

&SCOPED-DEFINE useTable ttblJob
{{&includes}/jobStatusFunc.i}

ON 'CTRL-D':U ANYWHERE
DO:
  RUN datePrompt.
END.

ON 'CTRL-L':U ANYWHERE
DO:
  RUN lagTime.
END.

ON 'CTRL-N':U ANYWHERE
DO:
  RUN moveToJob (1).
END.

ON 'CTRL-O':U ANYWHERE
DO:
  RUN statusCheckoff.
END.

ON 'CTRL-P':U ANYWHERE
DO:
  RUN moveToJob (-1).
END.

ON 'CTRL-R':U ANYWHERE
DO:
  RUN ctrlReset.
END.

ON 'CTRL-S':U ANYWHERE
DO:
  RUN ctrlSave.
END.

ON 'CTRL-T':U ANYWHERE
DO:
  RUN datePrompt.
END.
/*
ON 'PAGE-DOWN':U ANYWHERE
DO:
  RUN moveToJob (1).
END.

ON 'PAGE-UP':U ANYWHERE
DO:
  RUN moveToJob (-1).
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME boardFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS scenario btnSave btnReset btnJobSeqScan ~
btnPending btnPendingJobs btnDatePrompt btnShowDowntime btnDetail ~
btnFlashLight btnJobBrowse resourceGrid btnPrevDate btnNextDate boardDate ~
btnCalendar btnTimeLine intervals timeValue btnPrevInterval btnNextInterval ~
btnResourceList btnNext btnLast btnSetColorType resourceList 
&Scoped-Define DISPLAYED-OBJECTS scenario boardDate intervals timeValue ~
resourceList day-1 day-2 day-3 day-4 day-5 day-6 day-7 day-8 day-9 day-10 ~
day-11 day-12 day-13 day-14 day-15 day-16 day-17 day-18 day-19 day-20 ~
day-21 day-22 day-23 day-24 interval-1 interval-2 interval-3 interval-4 ~
interval-5 interval-6 interval-7 interval-8 interval-9 interval-10 ~
interval-11 interval-12 interval-13 interval-14 interval-15 interval-16 ~
interval-17 interval-18 interval-19 interval-20 interval-21 interval-22 ~
interval-23 interval-24 jobMovingDisplay 

/* Custom List Definitions                                              */
/* intervalButtons,configFields,gridColumns,navButtons,firstNav,lastNav */
&Scoped-define intervalButtons btnPrevInterval btnNextInterval 
&Scoped-define configFields boardDate 
&Scoped-define gridColumns boardGrid-6 boardGrid-24 boardGrid-17 ~
boardGrid-5 boardGrid-22 boardGrid-12 boardGrid-7 boardGrid-11 boardGrid-14 ~
boardGrid-18 boardGrid-20 boardGrid-21 boardGrid-9 boardGrid-2 boardGrid-23 ~
boardGrid-8 boardGrid-3 intervalRect boardGrid-10 settingsRect boardGrid-1 ~
boardGrid-16 boardGrid-19 boardGrid-4 boardGrid-13 boardGrid-15 ~
resourceGrid 
&Scoped-define navButtons btnResourceList btnNext btnLast 
&Scoped-define firstNav btnFirst btnPrevious 
&Scoped-define lastNav btnNext btnLast 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDateTimePixel s-object 
FUNCTION getDateTimePixel RETURNS INTEGER
  (ipDateTime AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD jobBGColor s-object 
FUNCTION jobBGColor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD jobFGColor s-object 
FUNCTION jobFGColor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime s-object 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pixelDate s-object 
FUNCTION pixelDate RETURNS DATE
  (ipDateTime AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pixelTime s-object 
FUNCTION pixelTime RETURNS INTEGER
  (ipDateTime AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD timeSpan s-object 
FUNCTION timeSpan RETURNS INTEGER PRIVATE
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnComplete 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "" 
     SIZE 5 BY 1.05 TOOLTIP "Complete Job".

DEFINE BUTTON btnDatePrompt 
     IMAGE-UP FILE "schedule/images/dateon.bmp":U
     LABEL "" 
     SIZE 5 BY 1.05 TOOLTIP "Turn Date Prompt Off".

DEFINE BUTTON btnDetail 
     IMAGE-UP FILE "schedule/images/detailwinon.bmp":U
     LABEL "Detail" 
     SIZE 5.2 BY 1.05 TOOLTIP "Turn Detail Window Display Off".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "schedule/images/first.bmp":U
     LABEL "First" 
     SIZE 4.4 BY 1.05 TOOLTIP "First Resource".

DEFINE BUTTON btnFlashLight 
     IMAGE-UP FILE "schedule/images/lightbulbon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Turn Highlight On".

DEFINE BUTTON btnJobBrowse 
     IMAGE-UP FILE "schedule/images/find.bmp":U
     LABEL "Job Browse" 
     SIZE 4.6 BY 1.05 TOOLTIP "Job Browse".

DEFINE BUTTON btnJobNotes 
     IMAGE-UP FILE "schedule/images/notetack.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Job Notes".

DEFINE BUTTON btnJobSeqScan 
     IMAGE-UP FILE "schedule/images/barcode_scanner.bmp":U
     LABEL "&Job Seq Scan" 
     SIZE 5.2 BY 1.05 TOOLTIP "Job Sequence Scan".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "schedule/images/last.bmp":U
     LABEL "Last" 
     SIZE 4.4 BY 1.05 TOOLTIP "Last Resource".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "schedule/images/next.bmp":U
     LABEL "Next" 
     SIZE 4.4 BY 1.05 TOOLTIP "Next Resource".

DEFINE BUTTON btnNextDate 
     IMAGE-UP FILE "schedule/images/next.bmp":U
     LABEL "&Next" 
     SIZE 4.4 BY 1.05 TOOLTIP "Next Date".

DEFINE BUTTON btnNextInterval 
     IMAGE-UP FILE "schedule/images/next.bmp":U
     LABEL "&Forward" 
     SIZE 4.6 BY 1.05 TOOLTIP "Move Forward by Interval".

DEFINE BUTTON btnPending 
     IMAGE-UP FILE "schedule/images/pending.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Pending by Job".

DEFINE BUTTON btnPendingJobs 
     IMAGE-UP FILE "schedule/images/pendingjobs.bmp":U
     LABEL "" 
     SIZE 5 BY 1.05 TOOLTIP "Pending by Resource".

DEFINE BUTTON btnPrevDate 
     IMAGE-UP FILE "schedule/images/prev.bmp":U
     LABEL "&Previous" 
     SIZE 4.4 BY 1.05 TOOLTIP "Previous Date".

DEFINE BUTTON btnPrevInterval 
     IMAGE-UP FILE "schedule/images/prev.bmp":U
     LABEL "&Back" 
     SIZE 4.6 BY 1.05 TOOLTIP "Move Back by Interval".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "schedule/images/prev.bmp":U
     LABEL "Previous" 
     SIZE 4.4 BY 1.05 TOOLTIP "Previous Resource".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "&Reset" 
     SIZE 5.2 BY 1.05 TOOLTIP "Reset Board from Last Save".

DEFINE BUTTON btnResourceList 
     LABEL "?" 
     SIZE 3.8 BY 1 TOOLTIP "Resource List"
     FONT 6.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "&Save" 
     SIZE 5.2 BY 1.05 TOOLTIP "Save Scenario".

DEFINE BUTTON btnSetColorType 
     LABEL "A" 
     SIZE 3.8 BY 1 TOOLTIP "Toggle Color Types (All/Time/Status)"
     FONT 6.

DEFINE BUTTON btnShowDowntime 
     IMAGE-UP FILE "schedule/images/downtimeon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Show Downtime".

DEFINE BUTTON btnTimeLine 
     IMAGE-UP FILE "schedule\images\clock1.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Position Board to Current Date & Time".

DEFINE BUTTON btnToolTip 
     IMAGE-UP FILE "schedule/images/helpabout.bmp":U
     LABEL "" 
     SIZE 5 BY 1.05 TOOLTIP "Show ToolTip".

DEFINE VARIABLE intervals AS CHARACTER FORMAT "X(256)":U INITIAL "1 Hour" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 22.6 BY 1 TOOLTIP "Select Grid Interval"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE scenario AS CHARACTER FORMAT "X(256)":U INITIAL "Actual" 
     LABEL "Scenari&o" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     LIST-ITEMS "<New>" 
     DROP-DOWN-LIST
     SIZE 12 BY 1 TOOLTIP "Select Board Scenario" NO-UNDO.

DEFINE VARIABLE timeValue AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 24
     LIST-ITEMS "0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100","1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200","2300" 
     DROP-DOWN-LIST
     SIZE 10.4 BY 1 TOOLTIP "Select Beginning Grid Time" NO-UNDO.

DEFINE VARIABLE boardDate AS DATE FORMAT "99.99.9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Set Beginning Grid Date"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE day-1 AS CHARACTER FORMAT "X(256)":U INITIAL "01" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-10 AS CHARACTER FORMAT "X(256)":U INITIAL "10" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-11 AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-12 AS CHARACTER FORMAT "X(256)":U INITIAL "12" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-13 AS CHARACTER FORMAT "X(256)":U INITIAL "13" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-14 AS CHARACTER FORMAT "X(256)":U INITIAL "14" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-15 AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-16 AS CHARACTER FORMAT "X(256)":U INITIAL "16" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-17 AS CHARACTER FORMAT "X(256)":U INITIAL "17" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-18 AS CHARACTER FORMAT "X(256)":U INITIAL "18" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-19 AS CHARACTER FORMAT "X(256)":U INITIAL "19" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-2 AS CHARACTER FORMAT "X(256)":U INITIAL "02" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-20 AS CHARACTER FORMAT "X(256)":U INITIAL "20" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-21 AS CHARACTER FORMAT "X(256)":U INITIAL "21" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-22 AS CHARACTER FORMAT "X(256)":U INITIAL "22" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-23 AS CHARACTER FORMAT "X(256)":U INITIAL "23" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-24 AS CHARACTER FORMAT "X(256)":U INITIAL "24" 
      VIEW-AS TEXT 
     SIZE 5 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-3 AS CHARACTER FORMAT "X(256)":U INITIAL "03" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-4 AS CHARACTER FORMAT "X(256)":U INITIAL "04" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-5 AS CHARACTER FORMAT "X(256)":U INITIAL "05" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-6 AS CHARACTER FORMAT "X(256)":U INITIAL "06" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-7 AS CHARACTER FORMAT "X(256)":U INITIAL "07" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-8 AS CHARACTER FORMAT "X(256)":U INITIAL "08" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE day-9 AS CHARACTER FORMAT "X(256)":U INITIAL "09" 
      VIEW-AS TEXT 
     SIZE 6 BY .52
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE interval-1 AS CHARACTER FORMAT "X(256)":U INITIAL "01" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-10 AS CHARACTER FORMAT "X(256)":U INITIAL "10" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-11 AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-12 AS CHARACTER FORMAT "X(256)":U INITIAL "12" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-13 AS CHARACTER FORMAT "X(256)":U INITIAL "13" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-14 AS CHARACTER FORMAT "X(256)":U INITIAL "14" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-15 AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-16 AS CHARACTER FORMAT "X(256)":U INITIAL "16" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-17 AS CHARACTER FORMAT "X(256)":U INITIAL "17" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-18 AS CHARACTER FORMAT "X(256)":U INITIAL "18" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-19 AS CHARACTER FORMAT "X(256)":U INITIAL "19" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-2 AS CHARACTER FORMAT "X(256)":U INITIAL "02" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-20 AS CHARACTER FORMAT "X(256)":U INITIAL "20" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-21 AS CHARACTER FORMAT "X(256)":U INITIAL "21" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-22 AS CHARACTER FORMAT "X(256)":U INITIAL "22" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-23 AS CHARACTER FORMAT "X(256)":U INITIAL "23" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-24 AS CHARACTER FORMAT "X(256)":U INITIAL "24" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-3 AS CHARACTER FORMAT "X(256)":U INITIAL "03" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-4 AS CHARACTER FORMAT "X(256)":U INITIAL "04" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-5 AS CHARACTER FORMAT "X(256)":U INITIAL "05" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-6 AS CHARACTER FORMAT "X(256)":U INITIAL "06" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-7 AS CHARACTER FORMAT "X(256)":U INITIAL "07" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-8 AS CHARACTER FORMAT "X(256)":U INITIAL "08" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE interval-9 AS CHARACTER FORMAT "X(256)":U INITIAL "09" 
      VIEW-AS TEXT 
     SIZE 3 BY .52
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE jobMovingDisplay AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 20 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE boardGrid-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-15
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-16
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-17
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-18
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-19
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-20
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-21
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-22
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-23
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-24
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE boardGrid-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.4 BY 24.38
     FGCOLOR 0 .

DEFINE RECTANGLE intervalRect
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 157 BY 1.14
     BGCOLOR 15 .

DEFINE RECTANGLE resourceGrid
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 27 BY 24.38
     BGCOLOR 8 .

DEFINE RECTANGLE settingsRect
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 157 BY 1.19
     BGCOLOR 7 .

DEFINE VARIABLE resourceList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "","''" 
     SIZE 27 BY 24.38
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE msgText-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-10 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-11 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-12 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-13 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-14 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-15 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-16 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-17 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-18 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-19 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-20 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-21 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-22 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-23 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-5 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-6 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-7 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-8 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE msgText-9 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY .86
     FGCOLOR 15  NO-UNDO.

DEFINE IMAGE msgImage-1
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-10
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-11
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-12
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-13
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-14
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-15
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-16
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-17
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-18
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-19
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-2
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-20
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-21
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-22
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-23
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-3
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-4
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-5
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-6
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-7
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-8
     SIZE 3.6 BY .86.

DEFINE IMAGE msgImage-9
     SIZE 3.6 BY .86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME boardFrame
     scenario AT ROW 1.05 COL 78 COLON-ALIGNED HELP
          "Select Working Scenario"
     btnSave AT ROW 1.05 COL 92 HELP
          "Click to Save Scenario"
     btnReset AT ROW 1.05 COL 97.2 HELP
          "Reset Scenario"
     btnJobSeqScan AT ROW 1.05 COL 102 HELP
          "Job Sequence Scan" WIDGET-ID 4
     btnToolTip AT ROW 1.05 COL 107.6 HELP
          "Click to Show ToolTip" WIDGET-ID 2
     btnPending AT ROW 1.05 COL 113 HELP
          "Click to Access Pending by Job"
     btnPendingJobs AT ROW 1.05 COL 118 HELP
          "Click to Access Pending by Resource"
     btnComplete AT ROW 1.05 COL 123 HELP
          "Click to Complete Job"
     btnJobNotes AT ROW 1.05 COL 128 HELP
          "Click to Complete Job"
     btnDatePrompt AT ROW 1.05 COL 133 HELP
          "Turn Date Prompt On/Off"
     btnShowDowntime AT ROW 1.05 COL 138 HELP
          "Click to Show/Hide Downtime"
     btnDetail AT ROW 1.05 COL 143 HELP
          "Tune Detail Window Display On/Off"
     btnFlashLight AT ROW 1.05 COL 147.8 HELP
          "Turn Highlight On/Off"
     btnJobBrowse AT ROW 1.05 COL 153 HELP
          "Click to Browse Jobs"
     btnPrevDate AT ROW 1.1 COL 1.2 HELP
          "Click for Previous Date"
     btnNextDate AT ROW 1.1 COL 5.6 HELP
          "Click for Next Date"
     boardDate AT ROW 1.1 COL 8 COLON-ALIGNED HELP
          "Select Date" NO-LABEL
     btnCalendar AT ROW 1.1 COL 26.4 HELP
          "Click for Next Date"
     btnTimeLine AT ROW 1.1 COL 31.4 HELP
          "Click to Reposition Time Line"
     intervals AT ROW 1.1 COL 34.8 COLON-ALIGNED HELP
          "Change Time Interval" NO-LABEL
     timeValue AT ROW 1.1 COL 57.8 COLON-ALIGNED HELP
          "Change Time" NO-LABEL
     btnPrevInterval AT ROW 1.1 COL 60 HELP
          "Shift Grid Interval Back"
     btnNextInterval AT ROW 1.1 COL 65 HELP
          "Shift Grid Interval Forward"
     btnFirst AT ROW 2.19 COL 1.2 HELP
          "Click to Goto First Resource"
     btnPrevious AT ROW 2.19 COL 5.6 HELP
          "Click to Goto Previous Resource"
     btnResourceList AT ROW 2.19 COL 10 HELP
          "Click to Goto Last Resource"
     btnNext AT ROW 2.19 COL 14 HELP
          "Click to Goto Next Resource"
     btnLast AT ROW 2.19 COL 18.4 HELP
          "Click to Goto Last Resource"
     btnSetColorType AT ROW 2.19 COL 23 HELP
          "Click to Toggle Color Types"
     resourceList AT ROW 3.38 COL 1 NO-LABEL
     day-1 AT ROW 2.19 COL 25 COLON-ALIGNED NO-LABEL
     day-2 AT ROW 2.19 COL 31.4 COLON-ALIGNED NO-LABEL
     day-3 AT ROW 2.19 COL 36.8 COLON-ALIGNED NO-LABEL
     day-4 AT ROW 2.19 COL 42.2 COLON-ALIGNED NO-LABEL
     day-5 AT ROW 2.19 COL 47.6 COLON-ALIGNED NO-LABEL
     day-6 AT ROW 2.19 COL 53 COLON-ALIGNED NO-LABEL
     day-7 AT ROW 2.19 COL 58.4 COLON-ALIGNED NO-LABEL
     day-8 AT ROW 2.19 COL 63.8 COLON-ALIGNED NO-LABEL
     day-9 AT ROW 2.19 COL 69.2 COLON-ALIGNED NO-LABEL
     day-10 AT ROW 2.19 COL 74.6 COLON-ALIGNED NO-LABEL
     day-11 AT ROW 2.19 COL 80 COLON-ALIGNED NO-LABEL
     day-12 AT ROW 2.19 COL 85.4 COLON-ALIGNED NO-LABEL
     day-13 AT ROW 2.19 COL 90.8 COLON-ALIGNED NO-LABEL
     day-14 AT ROW 2.19 COL 96.2 COLON-ALIGNED NO-LABEL
     day-15 AT ROW 2.19 COL 101.6 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME boardFrame
     day-16 AT ROW 2.19 COL 107 COLON-ALIGNED NO-LABEL
     day-17 AT ROW 2.19 COL 112.4 COLON-ALIGNED NO-LABEL
     day-18 AT ROW 2.19 COL 117.8 COLON-ALIGNED NO-LABEL
     day-19 AT ROW 2.19 COL 123.2 COLON-ALIGNED NO-LABEL
     day-20 AT ROW 2.19 COL 128.6 COLON-ALIGNED NO-LABEL
     day-21 AT ROW 2.19 COL 134 COLON-ALIGNED NO-LABEL
     day-22 AT ROW 2.19 COL 139.4 COLON-ALIGNED NO-LABEL
     day-23 AT ROW 2.19 COL 144.8 COLON-ALIGNED NO-LABEL
     day-24 AT ROW 2.19 COL 150.2 COLON-ALIGNED NO-LABEL
     interval-1 AT ROW 2.67 COL 25 COLON-ALIGNED NO-LABEL
     interval-2 AT ROW 2.67 COL 31.4 COLON-ALIGNED NO-LABEL
     interval-3 AT ROW 2.67 COL 36.8 COLON-ALIGNED NO-LABEL
     interval-4 AT ROW 2.67 COL 42.2 COLON-ALIGNED NO-LABEL
     interval-5 AT ROW 2.67 COL 47.6 COLON-ALIGNED NO-LABEL
     interval-6 AT ROW 2.67 COL 53 COLON-ALIGNED NO-LABEL
     interval-7 AT ROW 2.67 COL 58.4 COLON-ALIGNED NO-LABEL
     interval-8 AT ROW 2.67 COL 63.8 COLON-ALIGNED NO-LABEL
     interval-9 AT ROW 2.67 COL 69.2 COLON-ALIGNED NO-LABEL
     interval-10 AT ROW 2.67 COL 74.6 COLON-ALIGNED NO-LABEL
     interval-11 AT ROW 2.67 COL 80 COLON-ALIGNED NO-LABEL
     interval-12 AT ROW 2.67 COL 85.4 COLON-ALIGNED NO-LABEL
     interval-13 AT ROW 2.67 COL 90.8 COLON-ALIGNED NO-LABEL
     interval-14 AT ROW 2.67 COL 96.2 COLON-ALIGNED NO-LABEL
     interval-15 AT ROW 2.67 COL 101.6 COLON-ALIGNED NO-LABEL
     interval-16 AT ROW 2.67 COL 107 COLON-ALIGNED NO-LABEL
     interval-17 AT ROW 2.67 COL 112.4 COLON-ALIGNED NO-LABEL
     interval-18 AT ROW 2.67 COL 117.8 COLON-ALIGNED NO-LABEL
     interval-19 AT ROW 2.67 COL 123.2 COLON-ALIGNED NO-LABEL
     interval-20 AT ROW 2.67 COL 128.6 COLON-ALIGNED NO-LABEL
     interval-21 AT ROW 2.67 COL 134 COLON-ALIGNED NO-LABEL
     interval-22 AT ROW 2.67 COL 139.4 COLON-ALIGNED NO-LABEL
     interval-23 AT ROW 2.67 COL 144.8 COLON-ALIGNED NO-LABEL
     interval-24 AT ROW 2.67 COL 150.2 COLON-ALIGNED NO-LABEL
     jobMovingDisplay AT ROW 26.48 COL 3 NO-LABEL
     boardGrid-6 AT ROW 3.38 COL 55
     boardGrid-24 AT ROW 3.38 COL 152.2
     boardGrid-17 AT ROW 3.38 COL 114.4
     boardGrid-5 AT ROW 3.38 COL 49.6
     boardGrid-22 AT ROW 3.38 COL 141.4
     boardGrid-12 AT ROW 3.38 COL 87.4
     boardGrid-7 AT ROW 3.38 COL 60.4
     boardGrid-11 AT ROW 3.38 COL 82
     boardGrid-14 AT ROW 3.38 COL 98.2
     boardGrid-18 AT ROW 3.38 COL 119.8
     boardGrid-20 AT ROW 3.38 COL 130.6
     boardGrid-21 AT ROW 3.38 COL 136
     boardGrid-9 AT ROW 3.38 COL 71.2
     boardGrid-2 AT ROW 3.38 COL 33.4
     boardGrid-23 AT ROW 3.38 COL 146.8
     boardGrid-8 AT ROW 3.38 COL 65.8
     boardGrid-3 AT ROW 3.38 COL 38.8
     intervalRect AT ROW 2.14 COL 1
     boardGrid-10 AT ROW 3.38 COL 76.6
     settingsRect AT ROW 1 COL 1
     boardGrid-1 AT ROW 3.38 COL 28
     boardGrid-16 AT ROW 3.38 COL 109
     boardGrid-19 AT ROW 3.38 COL 125.2
     boardGrid-4 AT ROW 3.38 COL 44.2
     boardGrid-13 AT ROW 3.38 COL 92.8
     boardGrid-15 AT ROW 3.38 COL 103.6
     resourceGrid AT ROW 3.38 COL 1
     SPACE(130.00) SKIP(0.00)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

DEFINE FRAME msgFrame
     msgText-1 AT ROW 1.95 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     msgText-2 AT ROW 2.91 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     msgText-3 AT ROW 3.86 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     msgText-4 AT ROW 4.81 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     msgText-5 AT ROW 5.76 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     msgText-6 AT ROW 6.71 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     msgText-7 AT ROW 7.67 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     msgText-8 AT ROW 8.62 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     msgText-9 AT ROW 9.57 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     msgText-10 AT ROW 10.52 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     msgText-11 AT ROW 11.48 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     msgText-12 AT ROW 12.43 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     msgText-13 AT ROW 13.38 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     msgText-14 AT ROW 14.33 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     msgText-15 AT ROW 15.29 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     msgText-16 AT ROW 16.24 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     msgText-17 AT ROW 17.19 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     msgText-18 AT ROW 18.14 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     msgText-19 AT ROW 19.1 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     msgText-20 AT ROW 20.05 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     msgText-21 AT ROW 21 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     msgText-22 AT ROW 21.95 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     msgText-23 AT ROW 22.91 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     msgImage-1 AT ROW 1.95 COL 7 WIDGET-ID 2
     msgImage-2 AT ROW 2.91 COL 7 WIDGET-ID 14
     msgImage-3 AT ROW 3.86 COL 7 WIDGET-ID 18
     msgImage-4 AT ROW 4.81 COL 7 WIDGET-ID 22
     msgImage-5 AT ROW 5.76 COL 7 WIDGET-ID 26
     msgImage-6 AT ROW 6.71 COL 7 WIDGET-ID 30
     msgImage-7 AT ROW 7.67 COL 7 WIDGET-ID 34
     msgImage-8 AT ROW 8.62 COL 7 WIDGET-ID 38
     msgImage-9 AT ROW 9.57 COL 7 WIDGET-ID 42
     msgImage-10 AT ROW 10.52 COL 7 WIDGET-ID 46
     msgImage-11 AT ROW 11.48 COL 7 WIDGET-ID 50
     msgImage-12 AT ROW 12.43 COL 7 WIDGET-ID 54
     msgImage-13 AT ROW 13.38 COL 7 WIDGET-ID 58
     msgImage-14 AT ROW 14.33 COL 7 WIDGET-ID 62
     msgImage-15 AT ROW 15.29 COL 7 WIDGET-ID 66
     msgImage-16 AT ROW 16.24 COL 7 WIDGET-ID 70
     msgImage-17 AT ROW 17.19 COL 7 WIDGET-ID 74
     msgImage-18 AT ROW 18.14 COL 7 WIDGET-ID 78
     msgImage-19 AT ROW 19.1 COL 7 WIDGET-ID 82
     msgImage-20 AT ROW 20.05 COL 7 WIDGET-ID 86
     msgImage-21 AT ROW 21 COL 7 WIDGET-ID 90
     msgImage-22 AT ROW 21.95 COL 7 WIDGET-ID 94
     msgImage-23 AT ROW 22.91 COL 7 WIDGET-ID 98
    WITH 1 DOWN KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 28 ROW 3.38
         SIZE 130 BY 24.38
         BGCOLOR 0 FGCOLOR 14 
         TITLE BGCOLOR 1 "Scheduler Loading ... One Moment Please ...".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Add Fields to: Neither
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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 26.81
         WIDTH              = 157.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME msgFrame:FRAME = FRAME boardFrame:HANDLE.

/* SETTINGS FOR FRAME boardFrame
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME boardFrame:HIDDEN           = TRUE
       FRAME boardFrame:HEIGHT           = 26.76
       FRAME boardFrame:WIDTH            = 157.4.

/* SETTINGS FOR FILL-IN boardDate IN FRAME boardFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE boardGrid-1 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-1:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-10 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-10:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-11 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-11:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-12 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-12:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-13 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-13:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-14 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-14:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-15 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-15:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-16 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-16:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-17 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-17:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-18 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-18:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-19 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-19:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-2 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-2:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-20 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-20:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-21 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-21:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-22 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-22:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-23 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-23:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-24 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-24:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-3 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-3:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-4 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-4:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-5 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-5:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-6 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-6:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-7 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-7:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-8 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-8:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE boardGrid-9 IN FRAME boardFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       boardGrid-9:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR BUTTON btnComplete IN FRAME boardFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnComplete:PRIVATE-DATA IN FRAME boardFrame     = 
                "Complete Job".

ASSIGN 
       btnDatePrompt:PRIVATE-DATA IN FRAME boardFrame     = 
                "Turn Date Prompt".

ASSIGN 
       btnDetail:PRIVATE-DATA IN FRAME boardFrame     = 
                "Turn Detail Window Display".

/* SETTINGS FOR BUTTON btnFirst IN FRAME boardFrame
   NO-ENABLE 5                                                          */
ASSIGN 
       btnFlashLight:PRIVATE-DATA IN FRAME boardFrame     = 
                "Turn Highlight".

ASSIGN 
       btnJobBrowse:AUTO-RESIZE IN FRAME boardFrame      = TRUE
       btnJobBrowse:PRIVATE-DATA IN FRAME boardFrame     = 
                "jobBrowse".

/* SETTINGS FOR BUTTON btnJobNotes IN FRAME boardFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnJobNotes:PRIVATE-DATA IN FRAME boardFrame     = 
                "Complete Job".

/* SETTINGS FOR BUTTON btnLast IN FRAME boardFrame
   4 6                                                                  */
/* SETTINGS FOR BUTTON btnNext IN FRAME boardFrame
   4 6                                                                  */
/* SETTINGS FOR BUTTON btnNextInterval IN FRAME boardFrame
   1                                                                    */
ASSIGN 
       btnPending:PRIVATE-DATA IN FRAME boardFrame     = 
                "Pending by Job".

ASSIGN 
       btnPendingJobs:PRIVATE-DATA IN FRAME boardFrame     = 
                "Pending by Resource".

/* SETTINGS FOR BUTTON btnPrevInterval IN FRAME boardFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnPrevious IN FRAME boardFrame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR BUTTON btnResourceList IN FRAME boardFrame
   4                                                                    */
ASSIGN 
       btnShowDowntime:PRIVATE-DATA IN FRAME boardFrame     = 
                " Downtime".

/* SETTINGS FOR BUTTON btnToolTip IN FRAME boardFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnToolTip:PRIVATE-DATA IN FRAME boardFrame     = 
                "Complete Job".

/* SETTINGS FOR FILL-IN day-1 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-10 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-11 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-12 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-13 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-14 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-15 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-16 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-17 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-18 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-19 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-2 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-20 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-21 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-22 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-23 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-24 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-3 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-4 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-5 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-6 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-7 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-8 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN day-9 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-1 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-10 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-11 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-12 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-13 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-14 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-15 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-16 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-17 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-18 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-19 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-2 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-20 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-21 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-22 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-23 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-24 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-3 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-4 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-5 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-6 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-7 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-8 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN interval-9 IN FRAME boardFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE intervalRect IN FRAME boardFrame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN jobMovingDisplay IN FRAME boardFrame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       jobMovingDisplay:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE resourceGrid IN FRAME boardFrame
   3                                                                    */
ASSIGN 
       resourceList:HIDDEN IN FRAME boardFrame           = TRUE.

/* SETTINGS FOR RECTANGLE settingsRect IN FRAME boardFrame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FRAME msgFrame
                                                                        */
ASSIGN 
       FRAME msgFrame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN msgText-1 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-10 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-11 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-12 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-13 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-14 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-15 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-16 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-17 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-18 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-19 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-2 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-20 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-21 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-22 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-23 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-3 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-4 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-5 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-6 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-7 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-8 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN msgText-9 IN FRAME msgFrame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME boardFrame
/* Query rebuild information for FRAME boardFrame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME boardFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME msgFrame
/* Query rebuild information for FRAME msgFrame
     _Query            is NOT OPENED
*/  /* FRAME msgFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME boardDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDate s-object
ON ENTRY OF boardDate IN FRAME boardFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDate s-object
ON HELP OF boardDate IN FRAME boardFrame
DO:
  {{&includes}/calendar.i}
  APPLY 'RETURN' TO SELF.
  APPLY 'ENTRY' TO intervals.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDate s-object
ON LEAVE OF boardDate IN FRAME boardFrame
DO:
  SELF:SCREEN-VALUE = STRING({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDate s-object
ON RETURN OF boardDate IN FRAME boardFrame
DO:
  ASSIGN {&SELF-NAME}.
  APPLY 'VALUE-CHANGED' TO intervals.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar s-object
ON CHOOSE OF btnCalendar IN FRAME boardFrame
DO:
  APPLY 'HELP' TO boardDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComplete s-object
ON CHOOSE OF btnComplete IN FRAME boardFrame
DO:
  {{&includes}/{&Board}/btnComplete.i TO-ROWID(SELF:PRIVATE-DATA) THIS-PROCEDURE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDatePrompt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDatePrompt s-object
ON CHOOSE OF btnDatePrompt IN FRAME boardFrame
DO:
  {{&includes}/{&Board}/btnDatePrompt.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetail s-object
ON CHOOSE OF btnDetail IN FRAME boardFrame /* Detail */
DO:
  {{&includes}/{&Board}/btnDetail.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst s-object
ON CHOOSE OF btnFirst IN FRAME boardFrame /* First */
DO:
  IF resourceFirst EQ 1 THEN
  RETURN NO-APPLY.
  /* reposition back to first resource button */
  DISABLE {&firstNav} WITH FRAME {&FRAME-NAME}.
  ENABLE {&lastNav} WITH FRAME {&FRAME-NAME}.
  ASSIGN
    resourceFirst = 1
    resourceLast = resourceGap + 1.
  RUN buildResource.
  RUN buildBoard (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFlashLight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFlashLight s-object
ON CHOOSE OF btnFlashLight IN FRAME boardFrame
DO:
  {{&includes}/{&Board}/btnFlashLight.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobBrowse s-object
ON CHOOSE OF btnJobBrowse IN FRAME boardFrame /* Job Browse */
DO:
  {{&includes}/{&Board}/btnJobBrowse.i "'<Select ...>'"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobNotes s-object
ON CHOOSE OF btnJobNotes IN FRAME boardFrame
DO:
  {{&includes}/{&Board}/btnJobNotes.i TO-ROWID(SELF:PRIVATE-DATA)}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobSeqScan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobSeqScan s-object
ON CHOOSE OF btnJobSeqScan IN FRAME boardFrame /* Job Seq Scan */
DO:
  {{&includes}/{&Board}/btnJobSeqScan.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast s-object
ON CHOOSE OF btnLast IN FRAME boardFrame /* Last */
DO:
  IF resourceLast EQ resource# THEN
  RETURN NO-APPLY.
  /* reposition to last resource button on bottom */
  DISABLE {&lastNav} WITH FRAME {&FRAME-NAME}.
  ENABLE {&firstNav} WITH FRAME {&FRAME-NAME}.
  ASSIGN
    resourceFirst = resource# - resourceGap
    resourceLast = resource#.
  RUN buildResource.
  RUN buildBoard (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext s-object
ON CHOOSE OF btnNext IN FRAME boardFrame /* Next */
DO:
  IF resourceLast EQ resource# THEN
  RETURN NO-APPLY.
  ENABLE {&firstNav} WITH FRAME {&FRAME-NAME}.
  ASSIGN
    resourceFirst = resourceFirst + 1
    resourceLast = resourceLast + 1.
  IF resourceLast EQ resource# THEN
  DISABLE {&lastNav} WITH FRAME {&FRAME-NAME}.
  RUN buildResource.
  RUN buildBoard (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNextDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNextDate s-object
ON CHOOSE OF btnNextDate IN FRAME boardFrame /* Next */
DO:
  boardDate:SCREEN-VALUE = STRING(DATE(boardDate:SCREEN-VALUE) + 1).
  APPLY 'RETURN' TO boardDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNextInterval
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNextInterval s-object
ON CHOOSE OF btnNextInterval IN FRAME boardFrame /* Forward */
DO:
  RUN moveInterval ('moveForward').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPending s-object
ON CHOOSE OF btnPending IN FRAME boardFrame
DO:
  {{&includes}/{&Board}/btnPending.i THIS-PROCEDURE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPendingJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPendingJobs s-object
ON CHOOSE OF btnPendingJobs IN FRAME boardFrame
DO:
  {{&includes}/{&Board}/btnPendingJobs.i THIS-PROCEDURE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevDate s-object
ON CHOOSE OF btnPrevDate IN FRAME boardFrame /* Previous */
DO:
  boardDate:SCREEN-VALUE = STRING(DATE(boardDate:SCREEN-VALUE) - 1).
  APPLY 'RETURN' TO boardDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevInterval
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevInterval s-object
ON CHOOSE OF btnPrevInterval IN FRAME boardFrame /* Back */
DO:
  RUN moveInterval ('moveBack').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious s-object
ON CHOOSE OF btnPrevious IN FRAME boardFrame /* Previous */
DO:
  IF resourceFirst EQ 1 THEN
  RETURN NO-APPLY.
  ENABLE {&lastNav} WITH FRAME {&FRAME-NAME}.
  ASSIGN
    resourceFirst = resourceFirst - 1
    resourceLast = resourceLast - 1.
  IF resourceFirst EQ 1 THEN
  DISABLE {&firstNav} WITH FRAME {&FRAME-NAME}.
  RUN buildResource.
  RUN buildBoard (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset s-object
ON CHOOSE OF btnReset IN FRAME boardFrame /* Reset */
DO:
  {{&includes}/{&Board}/btnReset.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnResourceList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResourceList s-object
ON CHOOSE OF btnResourceList IN FRAME boardFrame /* ? */
DO:
  ASSIGN
    resourceList:HIDDEN = NOT resourceList:HIDDEN
    resourceList:SENSITIVE = NOT resourceList:SENSITIVE
    ldummy = resourceList:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave s-object
ON CHOOSE OF btnSave IN FRAME boardFrame /* Save */
DO:
  {{&includes}/{&Board}/btnSave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSetColorType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetColorType s-object
ON CHOOSE OF btnSetColorType IN FRAME boardFrame /* A */
DO:
  SELF:LABEL = IF SELF:LABEL EQ 'A' THEN 'T' ELSE
               IF SELF:LABEL EQ 'T' THEN 'S' ELSE 'A'.
  RUN setColorType (SELF:LABEL).
  RUN setJobColors.
  /*RUN {&objects}/sbHTML.p.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShowDowntime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowDowntime s-object
ON CHOOSE OF btnShowDowntime IN FRAME boardFrame
DO:
  {{&includes}/{&Board}/btnShowDowntime.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTimeLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTimeLine s-object
ON CHOOSE OF btnTimeLine IN FRAME boardFrame
DO:
  IF boardDate NE TODAY THEN DO:
    boardDate:SCREEN-VALUE = STRING(TODAY).
    APPLY 'RETURN' TO boardDate.
  END.
  ELSE
  RUN timeLine.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToolTip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToolTip s-object
ON CHOOSE OF btnToolTip IN FRAME boardFrame
DO:
    IF VALID-HANDLE(currentJob) THEN
    MESSAGE currentJob:TOOLTIP VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME intervals
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL intervals s-object
ON VALUE-CHANGED OF intervals IN FRAME boardFrame
DO:
  hourMode = INDEX(intervals:SCREEN-VALUE,'Hour') NE 0.
  HIDE timeValue {&intervalButtons}.
  IF hourMode THEN
  ENABLE {&intervalButtons} WITH FRAME {&FRAME-NAME}.
  ELSE
  ENABLE timeValue WITH FRAME {&FRAME-NAME}.
  RUN buildBoard (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME resourceList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resourceList s-object
ON DEFAULT-ACTION OF resourceList IN FRAME boardFrame
DO:
  APPLY 'CHOOSE' TO btnResourceList.
  ASSIGN
    resourceFirst = INTEGER(SELF:SCREEN-VALUE)
    resourceLast = resourceFirst + resourceGap.
  IF resourceLast GT resource# THEN
  APPLY 'CHOOSE' TO btnLast.
  ELSE
  DO:
    RUN buildResource.
    RUN buildBoard (YES).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scenario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scenario s-object
ON VALUE-CHANGED OF scenario IN FRAME boardFrame /* Scenario */
DO:
  {{&includes}/{&Board}/scenario.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME timeValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL timeValue s-object
ON VALUE-CHANGED OF timeValue IN FRAME boardFrame
DO:
  RUN buildBoard (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* make sure widget pools don't already exist */
DELETE WIDGET-POOL 'downtimePool' NO-ERROR.
DELETE WIDGET-POOL 'gridLine' NO-ERROR.
DELETE WIDGET-POOL 'jobPool' NO-ERROR.
DELETE WIDGET-POOL 'lockPool' NO-ERROR.
DELETE WIDGET-POOL 'notePool' NO-ERROR.
DELETE WIDGET-POOL 'resourcePool' NO-ERROR.
DELETE WIDGET-POOL 'threeDPool' NO-ERROR.

/* create widget pools needed */
CREATE WIDGET-POOL 'downtimePool' PERSISTENT.
CREATE WIDGET-POOL 'gridLine' PERSISTENT.
CREATE WIDGET-POOL 'jobPool' PERSISTENT.
CREATE WIDGET-POOL 'lockPool' PERSISTENT.
CREATE WIDGET-POOL 'notePool' PERSISTENT.
CREATE WIDGET-POOL 'resourcePool' PERSISTENT.
CREATE WIDGET-POOL 'threeDPool' PERSISTENT.

/* create line to show position on grid when clicking a button */
CREATE TEXT gridLine IN WIDGET-POOL 'gridLine'
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      FORMAT = 'X(256)'
      X = 1
      Y = 51
      WIDTH-PIXELS = 1
      HEIGHT-PIXELS = 2
      HIDDEN = NO.

CREATE TEXT timeLine IN WIDGET-POOL 'gridLine'
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      FORMAT = 'X(256)'
      X = 0
      Y = 50
      WIDTH-PIXELS = 2
      HEIGHT-PIXELS = 1
      BGCOLOR = 12
      HIDDEN = NO.

RUN initMsgFrame.

{{&includes}/{&Board}/boardProc.i}
{{&includes}/{&Board}/calcEnd.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asiDC s-object 
PROCEDURE asiDC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvProdAce AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvVorneDat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvHCDat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvHCExport AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvHCImport AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvTimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvJobLocked AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvJobCompleted AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvMRCompleted AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvRunCompleted AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvContinue AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvAutoMonitor AS LOGICAL NO-UNDO.

  SESSION:SET-WAIT-STATE('General').

  lvProdAce = findProgram('{&data}/',ID,'/ProdAce.dat').
  IF lvProdAce NE ? THEN DO:
      RUN autoMonitorFlag (OUTPUT lvAutoMonitor).
      RUN {&loads}/ASI/prodAce.w (lvProdAce,containerHandle,lvAutoMonitor,OUTPUT lvContinue).
      IF NOT lvContinue THEN RETURN.
  END. /* production ace */
  ELSE DO:
      lvVorneDat = findProgram('{&data}/',ID,'/Vorne.dat').
      IF lvVorneDat NE ? THEN DO:
          RUN {&loads}/ASI/vorne.w (lvVorneDat,OUTPUT lvContinue).
          IF NOT lvContinue THEN RETURN.
      END. /* vorne */
      ELSE DO:
          lvHCDat = findProgram('{&data}/',ID,'/HighwayConnect.dat').
          IF lvHCDat NE ? THEN DO:
            INPUT FROM VALUE(lvHCDat) NO-ECHO.
            IMPORT lvHCExport.
            IMPORT lvHCImport.
            INPUT CLOSE.
            FOR EACH ttblJob NO-LOCK
                WHERE ttblJob.jobCompleted EQ NO
                  AND ttblJob.liveUpdate EQ YES
                BREAK BY ttblJob.resource:
              IF FIRST-OF(ttblJob.resource) THEN DO:
                FIND FIRST ttblResource NO-LOCK
                     WHERE ttblResource.resource EQ ttblJob.resource NO-ERROR.
                IF AVAILABLE ttblResource THEN
                OUTPUT TO VALUE(lvHCImport + ttblResource.resourceDescription + '.txt').
                ELSE NEXT.
              END. /* first-of */
              EXPORT DELIMITER '~t' 'H' 'U'
                ttblJob.job + (IF ttblJob.userField19 EQ '' THEN ''
                               ELSE '.' + STRING(INTEGER(ttblJob.userField19)))
                ttblResource.resourceDescription
                ttblJob.userField08
                ttblJob.userField09
                ttblJob.userField01
                ttblJob.userField01
                STRING(YEAR(ttblJob.dueDate)) + STRING(MONTH(ttblJob.dueDate)) + STRING(DAY(ttblJob.dueDate))
                STRING(INTEGER(ttblJob.userField15))
                .
            END. /* each ttbljob */
            OUTPUT CLOSE.
          END. /* if lvhcdat ne ? */
      END. /* highway connect */
  END. /* else */

  {{&includes}/asiDC.i ttblJob}
  {{&includes}/asiDC.i pendingJob}
  RUN setJobDateTime (?,NO).
  OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/downtimes.Actual.dat')).
  FOR EACH ttblDowntime NO-LOCK BY ttblDowntime.dayID BY ttblDowntime.resource
      BY ttblDowntime.startDate BY ttblDowntime.startTime:
    EXPORT ttblDowntime EXCEPT ttblDowntime.startDateTime ttblDowntime.endDateTime.
  END.
  OUTPUT CLOSE.
  RUN loadDowntime.
  SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE autoMonitor s-object 
PROCEDURE autoMonitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define autoMonitorObjects ~
scenario btnSave /*btnRemove*/ btnReset btnJobSeqScan btnPending btnPendingJobs btnComplete ~
btnJobNotes btnDatePrompt

  DEFINE INPUT PARAMETER ipAutoMonitor AS LOGICAL NO-UNDO.
  
  IF ipAutoMonitor THEN
  DISABLE {&autoMonitorObjects} WITH FRAME {&FRAME-NAME}.
  ELSE
  ENABLE {&autoMonitorObjects} WITH FRAME {&FRAME-NAME}.
  autoMonitorInterval = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE autoMonitorFlag s-object 
PROCEDURE autoMonitorFlag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplAutoMonitor AS LOGICAL NO-UNDO.
  
  oplAutoMonitor = NOT btnSave:SENSITIVE IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE boardType s-object 
PROCEDURE boardType :
/*------------------------------------------------------------------------------
  Purpose:     tell any calling program this board's type (basic,view or pro)
  Parameters:  output board type
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opBoard AS CHARACTER NO-UNDO INITIAL '{&Board}'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildBoard s-object 
PROCEDURE buildBoard :
/*------------------------------------------------------------------------------
  Purpose:     destroy and rebuild the schedule board
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipBuildGridArray AS LOGICAL NO-UNDO.

  APPLY 'ENTRY' TO boardDate IN FRAME {&FRAME-NAME}.
  RUN setScreenStatus.
  DISABLE btnComplete btnJobNotes WITH FRAME {&FRAME-NAME}.
  RUN msgFrame ('Beginning Board Build').
  IF ipBuildGridArray THEN
  RUN buildGridArray.
  RUN buildJob.
  RUN buildDowntime.
  RUN msgFrame (?).
  HIDE FRAME msgFrame NO-PAUSE.
  RUN showBoard.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildGridArray s-object 
PROCEDURE buildGridArray :
/*------------------------------------------------------------------------------
  Purpose:     calculate the start date & time, end date & time for each grid
               interval
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE gridColor AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE t AS INTEGER NO-UNDO.
  DEFINE VARIABLE d AS DATE NO-UNDO.

  RUN msgFrame ('Building Grid Array').
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      j = INTEGER(SUBSTR(intervals:SCREEN-VALUE,1,2))
      d = DATE(boardDate:SCREEN-VALUE)
      gridColor = 74
      timeValue
      t = timeValue.
    DO i = 1 TO 24:
      k = i * j - j.
      IF NOT hourMode AND INTEGER(k / 60 - .5) GT 0 THEN
      k = k - 60 * INTEGER(k / 60 - .5).
      ELSE
      IF hourMode AND INTEGER(k / 24 - .5) GT 0 THEN
      k = k - 24 * INTEGER(k / 24 - .5).
      IF hourMode THEN
      DO:
        IF k EQ 0 THEN
        gridColor = 74.
        ASSIGN
          intStime[i] = k * 3600
          intEtime[i] = intStime[i] + j * 3600
          gridColor = gridColor + 1
          intColor[i] = gridColor
          d = d + (IF i NE 1 AND k EQ 0 THEN
                  (IF intEtime[i] GT 86400 THEN
                   TRUNCATE(intEtime[i] / 86400,0) ELSE 1) ELSE 0).
        IF intEtime[i] GT 86400 THEN
        intEtime[i] = intEtime[i] - TRUNCATE(intEtime[i] / 86400,0) * 86400 + 86400.
      END.
      ELSE
      ASSIGN
        t = t + (IF i NE 1 AND k EQ 0 THEN 100 ELSE 0)
        d = d + (IF t EQ 2400 THEN 1 ELSE 0)
        t = IF t EQ 2400 THEN 0 ELSE t
        intStime[i] = t / 100 * 3600 + k * 60
        intEtime[i] = intStime[i] + j * 60
        intT[i] = t
        intColor[i] = 74 + i.
      ASSIGN
        intDate[i] = d
        intK[i] = k.
    END.
    RUN setGrid (NO).
    RUN buildPixelArray.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildJob s-object 
PROCEDURE buildJob :
/*------------------------------------------------------------------------------
  Purpose:     build each job bar object, lock button and 3D look
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE jobFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE xValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE yValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE wValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE spixel AS INTEGER NO-UNDO.
  DEFINE VARIABLE epixel AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  &SCOPED-DEFINE ttblFile ttblJob
  &SCOPED-DEFINE startDate ttblJob.startDate
  &SCOPED-DEFINE endDate ttblJob.endDate
  
  RUN msgFrame ('Building Jobs').
  ASSIGN
    jobIdx = 0
    lockIdx = 0
    noteIdx = 0 /* noteIcon */
    threeDIdx = 0.
  DO i = 1 TO resourceIdx:
    currentWidget = resourceWidget[i].
    FOR EACH ttblJob
        WHERE ttblJob.resource EQ currentWidget:NAME
          AND ((ttblJob.startDate GE intDate[1]
          AND ttblJob.startDate LE intDate[24])
           OR (ttblJob.endDate GE intDate[1]
          AND ttblJob.endDate LE intDate[24])
           OR (ttblJob.startDate LE intDate[1]
          AND ttblJob.endDate GE intDate[24]))
        WITH FRAME {&FRAME-NAME}:
      IF completedHide AND ttblJob.jobCompleted THEN NEXT.
      {{&includes}/buildInclude.i}
      jobIdx = jobIdx + 1.
      RUN createJob (jobIdx,xValue,yValue,wValue,hpixels).
      {{&includes}/{&Board}/buildJob.i}
      IF threeD THEN
      RUN buildThreeD (xValue,yValue,wValue,hpixels).
    END. /* each ttblJob */
  END. /* do i */
  {{&includes}/{&Board}/buildJob2.i}
  RUN hideThreeD (threeDIdx).
  RUN hideJob (jobIdx).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildPixelArray s-object 
PROCEDURE buildPixelArray :
/*------------------------------------------------------------------------------
  Purpose:     calculates each pixels date & time value in YYYYMMDD.TTTTT format
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.

  RUN msgFrame ('Building Pixel Array').
  DO i = 1 TO 24:
    k = INT((intEtime[i] - intStime[i]) / rectWidth).
    DO j = 1 TO rectWidth:
      ASSIGN
        dateTimePixel[j + (i - 1) * rectWidth] =
                      numericDateTime(intDate[i],(j - 1) * k + intStime[i]).
        maxpixel = j + (i - 1) * rectWidth.
    END.
  END.
  DO k = maxPixel + 1 TO maxPixel + 10:
    dateTimePixel[k] = 99999999.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildResource s-object 
PROCEDURE buildResource :
/*------------------------------------------------------------------------------
  Purpose:     build each resource button
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN msgFrame ('Building Resources').
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  ASSIGN
    resourceXCoord = 1
    resourceYCoord = 55
    resourceIdx = 0.
  FOR EACH ttblResource NO-LOCK WHERE ttblResource.order GE resourceFirst
                                  AND ttblResource.order LE resourceLast:
    resourceIdx = resourceIdx + 1.
    {{&includes}/{&Board}/lightBulb.i}
    RUN createResourceButton (ttblResource.resource,
                              ttblResource.resourceDescription,
                              resourceIdx).
    RUN createResource (ttblResource.resource,
                        ttblResource.resourceDescription,
                        resourceIdx,
                        ttblResource.dmiID).
  END.
  RUN hideResource (resourceIdx).
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildScenario s-object 
PROCEDURE buildScenario :
/*------------------------------------------------------------------------------
  Purpose:     locate all existing scenario files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE searchDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fileName AS CHARACTER FORMAT "X(30)" NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT "X(4)" NO-UNDO.
  DEFINE VARIABLE listItems AS CHARACTER NO-UNDO.

  RUN msgFrame ('Building Scenarios').
  searchDir = clientDat + '{&scenarios}/' + ID.
  IF INDEX(PROPATH,'schedule') NE 0 AND
     INDEX(PROPATH,':') EQ 0 THEN searchDir = '../' + searchDir.
  INPUT FROM OS-DIR(searchDir) NO-ECHO.
  REPEAT:
    SET fileName ^ attrList.
    IF attrList NE "f" THEN
    NEXT.
    ASSIGN
      fileName = REPLACE(fileName,'.dat','')
      ldummy = scenario:ADD-LAST(fileName) IN FRAME {&FRAME-NAME}.
  END.
  INPUT CLOSE.
  IF scenario:NUM-ITEMS EQ 1 THEN
  ldummy = scenario:ADD-LAST('Actual').
  scenario = 'Actual'.
  DISPLAY scenario WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildThreeD s-object 
PROCEDURE buildThreeD :
/*------------------------------------------------------------------------------
  Purpose:     create the four sides of the 3D look
  Parameters:  X, Y, width and height
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipY AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipHeight AS INTEGER NO-UNDO.

  IF threeDTop THEN
  DO:
    threeDIdx = threeDIdx + 1.
    RUN createThreeD (threeDIdx,ipX,ipY,ipWidth,1,15). /* top */
  END.
  IF threeDLeft THEN
  DO:
    threeDIdx = threeDIdx + 1.
    RUN createThreeD (threeDIdx,ipX,ipY,1,ipHeight,15). /* left */
  END.
  IF threeDBottom THEN
  DO:
    threeDIdx = threeDIdx + 1.
    RUN createThreeD (threeDIdx,ipX,ipY + ipHeight - 2,ipWidth,2,0). /* bottom */
  END.
  IF threeDRight THEN
  DO:
    threeDIdx = threeDIdx + 1.
    RUN createThreeD (threeDIdx,ipX + ipWidth - 2,ipY,2,ipHeight,0). /* right */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE capacityView s-object 
PROCEDURE capacityView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT VALID-HANDLE(capacityViewHandle) THEN
  DO:
    RUN VALUE(findProgram('{&objects}','','/capacityView.w')) PERSISTENT SET capacityViewHandle.
    RUN local-initialize IN capacityViewHandle.
    RUN setPopup IN containerHandle (9,capacityViewHandle).
    RUN passHandle IN capacityViewHandle (THIS-PROCEDURE,'{&Board}').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeDowntimeScenario s-object 
PROCEDURE changeDowntimeScenario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
  
  pHandle = SESSION:FIRST-PROCEDURE.
  DO WHILE VALID-HANDLE(pHandle):
    IF pHandle:FILE-NAME EQ '{&prompts}/scheduleID.p' THEN
    DO:
      RUN setValues IN pHandle (ID,scenario).
      RUN initCalendar IN downtimeHandle.
      RUN getDowntime.
      RUN buildBoardDowntime.
      LEAVE.
    END.
    pHandle = pHandle:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE colorGrid s-object 
PROCEDURE colorGrid :
/*------------------------------------------------------------------------------
  Purpose:     run colorGrid.w from container
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hdle AS HANDLE NO-UNDO.

  btnSetColorType:LABEL IN FRAME {&FRAME-NAME} = 'A'.
  RUN setColorType (btnSetColorType:LABEL).
  RUN {&prompts}/colorGrid.w PERSISTENT SET hdle (THIS-PROCEDURE).
  RUN setPopup IN containerHandle (1,hdle).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE containerHandle s-object 
PROCEDURE containerHandle :
/*------------------------------------------------------------------------------
  Purpose:     hold the container handle value
  Parameters:  container handle value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipDowntimeHandle AS HANDLE NO-UNDO.

  ASSIGN
    containerHandle = ipHandle
    downtimeHandle = ipDowntimeHandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createJob s-object 
PROCEDURE createJob :
/*------------------------------------------------------------------------------
  Purpose:     create job bar and triggers
  Parameters:  object number assigned, X, Y, width and height
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipY AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipHeight AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE jobDescriptn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE jobToolTip AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  ASSIGN
    ipX = ipX + (IF threeD THEN 1 ELSE 0)
    ipY = ipY + (IF threeD THEN 1 ELSE 0)
    ipWidth = IF ipWidth GT 3 THEN ipWidth - (IF threeD THEN 3 ELSE 0) ELSE 1
    ipHeight = ipHeight - (IF threeD THEN 3 ELSE 0).
  
  {{&includes}/ttblWidgetFind.i "jobWidget" ipIdx}
  {{&includes}/ttblWidgetAssign.i "jobWidget" pWidget}
  ELSE
  DO:
    CREATE EDITOR pWidget IN WIDGET-POOL 'jobPool'
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        FONT = 6
        READ-ONLY = YES
        BOX = threeD EQ ?
        WORD-WRAP = NO
        SENSITIVE = YES
        {{&includes}/{&Board}/createJob.i}
    {{&includes}/ttblWidgetCreate.i
      "jobWidget" ipIdx pWidget "jobBGColor" jobBGColor() "jobFGColor" jobFGColor()}
  END.
  RUN jobText (ttblJob.jobDescription,OUTPUT jobDescriptn).
  RUN jobText (ttblJob.jobToolTip,OUTPUT jobToolTip).
  ASSIGN
    ttblJob.jobBGColor = jobBGColor()
    ttblJob.jobFGColor = jobFGColor()
    ttblJob.statusLabel = jobStatus()
    ttblJob.widgetIdx = ipIdx
    jobWidget.jobBGColor = ttblJob.jobBGColor
    jobWidget.jobFGColor = ttblJob.jobFGColor
    pWidget:HIDDEN = YES
    pWidget:X = ipX
    pWidget:Y = ipY
    pWidget:WIDTH-PIXELS = ipWidth
    pWidget:HEIGHT-PIXELS = ipHeight
    pWidget:NAME = ttblJob.job
    pWidget:BGCOLOR = ttblJob.jobBGColor
    pWidget:FGCOLOR = ttblJob.jobFGColor
    pWidget:SCREEN-VALUE = (IF /* '{&Board}' EQ 'Pro' AND */
                            intervals:LOOKUP(intervals:SCREEN-VALUE)
                            LE lockButtons THEN '    ' ELSE '')
    /* noteIcon */ 
    pWidget:SCREEN-VALUE = pWidget:SCREEN-VALUE +
                           (IF intervals:LOOKUP(intervals:SCREEN-VALUE)
                            LE noteButtons THEN '    ' ELSE '') + jobDescriptn
    pWidget:PRIVATE-DATA = STRING(ROWID(ttblJob))
    .
  pWidget:TOOLTIP = jobToolTip + '~n~nStatus: ' + jobStatus().
  IF '{&Board}' EQ 'Basic' THEN
  pWidget:SCREEN-VALUE = jobDescriptn.
  /* {{&includes}/tooltip.i} */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createResource s-object 
PROCEDURE createResource :
/*------------------------------------------------------------------------------
  Purpose:     create resource button and triggers
  Parameters:  resource name and object number assigned
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipResourceDescription AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipDmiID AS INTEGER NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  IF VALID-HANDLE(resourceWidget[ipIdx]) THEN
  pWidget = resourceWidget[ipIdx].
  ELSE
  CREATE BUTTON pWidget IN WIDGET-POOL 'resourcePool'
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        FONT = 6
        SENSITIVE = YES
  TRIGGERS:
    ON ENTRY
       PERSISTENT RUN gridLine IN THIS-PROCEDURE (pWidget:HANDLE).
    {{&includes}/{&Board}/createResource.i}
  END TRIGGERS.
  ASSIGN
    pWidget:HIDDEN = NO
    pWidget:X = resourceXCoord + 48
    pWidget:Y = resourceYCoord
    pWidget:WIDTH-PIXELS = resourceGrid:WIDTH-PIXELS - 54
    pWidget:HEIGHT-PIXELS = hpixels
    pWidget:LABEL = REPLACE(ipResource,'&','&&')
    pWidget:NAME = ipResource
    pWidget:PRIVATE-DATA = ipResource
    pWidget:TOOLTIP = (IF ipResourceDescription EQ '' THEN ?
                       ELSE ipResourceDescription) +
                      (IF ipDmiID NE 0 THEN ' (DMI ID: ' + STRING(ipDmiID,'999') + ')'
                       ELSE '')
    ldummy = pWidget:MOVE-TO-TOP()
    resourceWidget[ipIdx] = pWidget:HANDLE
    resourceYCoord = resourceYCoord + hpixels + gap.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createResourceButton s-object 
PROCEDURE createResourceButton :
/*------------------------------------------------------------------------------
  Purpose:     create resource 1st & last buttons and triggers
  Parameters:  resource name and object number assigned
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipResourceDescription AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  IF VALID-HANDLE(resourceFirstButton[ipIdx]) THEN
  pWidget = resourceFirstButton[ipIdx].
  ELSE
  CREATE BUTTON pWidget IN WIDGET-POOL 'resourcePool'
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        TOOLTIP = 'Reposition to First Job' + IF ipResourceDescription EQ '' THEN ''
                                  ELSE ' for ' + ipResourceDescription
        SENSITIVE = YES
  TRIGGERS:
    ON ENTRY
       PERSISTENT RUN resourceFirstLast IN THIS-PROCEDURE (pWidget:HANDLE,'First').
    {{&includes}/{&Board}/createResource.i}
  END TRIGGERS.
  ASSIGN
    pWidget:HIDDEN = NO
    pWidget:X = resourceXCoord
    pWidget:Y = resourceYCoord
    pWidget:WIDTH-PIXELS = 22
    pWidget:HEIGHT-PIXELS = hpixels
    pWidget:NAME = ipResource
    pWidget:PRIVATE-DATA = ipResource
    ldummy = pWidget:LOAD-IMAGE('{&images}/first.bmp')
    ldummy = pWidget:MOVE-TO-TOP()
    resourceFirstButton[ipIdx] = pWidget:HANDLE.
  IF VALID-HANDLE(resourceLastButton[ipIdx]) THEN
  pWidget = resourceLastButton[ipIdx].
  ELSE
  CREATE BUTTON pWidget IN WIDGET-POOL 'resourcePool'
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        TOOLTIP = 'Reposition to Last Job' + IF ipResourceDescription EQ '' THEN ''
                                 ELSE ' for ' + ipResourceDescription
        SENSITIVE = YES
  TRIGGERS:
    ON ENTRY
       PERSISTENT RUN resourceFirstLast IN THIS-PROCEDURE (pWidget:HANDLE,'Last').
    {{&includes}/{&Board}/createResource.i}
  END TRIGGERS.
  ASSIGN
    pWidget:HIDDEN = NO
    pWidget:X = resourceXCoord + 22
    pWidget:Y = resourceYCoord
    pWidget:WIDTH-PIXELS = 22
    pWidget:HEIGHT-PIXELS = hpixels
    pWidget:NAME = ipResource
    pWidget:PRIVATE-DATA = ipResource
    ldummy = pWidget:LOAD-IMAGE('{&images}/last.bmp')
    ldummy = pWidget:MOVE-TO-TOP()
    resourceLastButton[ipIdx] = pWidget:HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createThreeD s-object 
PROCEDURE createThreeD :
/*------------------------------------------------------------------------------
  Purpose:     create rectangles used to show job bar in 3D
  Parameters:  object number assigned, X, Y, width, height & background color
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipY AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipHeight AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipBGColor AS INTEGER NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  {{&includes}/ttblWidgetFind.i "threeDWidget" ipIdx}
  {{&includes}/ttblWidgetAssign.i "threeDWidget" pWidget}
  ELSE
  DO:
    CREATE RECTANGLE pWidget IN WIDGET-POOL 'threeDPool'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          EDGE-PIXELS = 0
          FILLED = TRUE.
    {{&includes}/ttblWidgetCreate.i "threeDWidget" ipIdx pWidget}
  END.
  ASSIGN
    pWidget:X = resourceGrid:WIDTH-PIXELS + 1
    pWidget:WIDTH-PIXELS = ipWidth
    pWidget:HEIGHT-PIXELS = ipHeight
    pWidget:X = ipX
    pWidget:Y = ipY
    pWidget:BGCOLOR = ipBGColor
    pWidget:HIDDEN = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlReset s-object 
PROCEDURE ctrlReset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY 'CHOOSE' TO btnReset IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlSave s-object 
PROCEDURE ctrlSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY 'CHOOSE' TO btnSave IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataCollection s-object 
PROCEDURE dataCollection :
/*------------------------------------------------------------------------------
  Purpose:     run dataCollection program so it has access to shared vars in
               avail from schedule.w
  Parameters:  ttblJob rowids
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowIDs AS CHARACTER NO-UNDO.

  DEFINE VARIABLE hdle AS HANDLE NO-UNDO.
  DEFINE VARIABLE running AS LOGICAL NO-UNDO.

  RUN checkPopup IN containerHandle (7,OUTPUT running).
  IF running THEN RETURN.

  RUN VALUE(findProgram('{&viewers}/',ID,'/dataCollection.w')) PERSISTENT SET hdle (ipRowIDs).
  RUN setPopup IN containerHandle (7,hdle).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE datePrompt s-object 
PROCEDURE datePrompt :
/*------------------------------------------------------------------------------
  Purpose:     access date prompt popup from current selected job
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &IF '{&Board}' EQ 'Pro' &THEN
  DEFINE VARIABLE lvBoardDatePrompt AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvResizeJob AS LOGICAL NO-UNDO.

  IF VALID-HANDLE(currentJob) AND currentJob:SELECTED THEN
  DO:
    ASSIGN
      lvBoardDatePrompt = boardDatePrompt
      resizeJob = endDateMove
      boardDatePrompt = YES.
    RUN jobEndMove (currentJob).
    ASSIGN
      boardDatePrompt = lvBoardDatePrompt
      resizeJob = NO.
  END.
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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
  HIDE FRAME boardFrame.
  HIDE FRAME msgFrame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynColumns s-object 
PROCEDURE dynColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE runAgain AS LOGICAL NO-UNDO.

  runAgain = YES.
  DO WHILE runAgain:
    RUN {&viewers}/dynColumns.w (FRAME {&FRAME-NAME}:HEIGHT-PIXELS,
                                 FRAME {&FRAME-NAME}:WIDTH-PIXELS,
                                 OUTPUT runAgain).
  END. /* do while */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getConfiguration s-object 
PROCEDURE getConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     load configuration values and set screen values accordingly
  Parameters:  input yes if initial get, otherwise don't set everything
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER setConfigValues AS LOGICAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  RUN msgFrame ('Get Configurations').
  DO WITH FRAME {&FRAME-NAME}:
    {{&includes}/{&Board}/getConfiguration.i}
    INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
    IMPORT version.
    INPUT CLOSE.
    RUN VALUE('get' + version).
    IF version NE '{&version}' THEN /* handle version changes */
    RUN put{&version}.
    &IF '{&Board}' NE 'Pro' &THEN
    IF autoSize THEN DO:
      MESSAGE 'Set Auto-Size Off?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE autoSize.
      ASSIGN
        autoSize = NOT autoSize
        fullBoard = YES.
    END.
    &ENDIF
    ASSIGN /* set opposite of initial setting, apply will toggle it back */
      capacityLoad = loadCapacity
      boardDatePrompt = NOT boardDatePrompt
      detailWindow = NOT detailWindow
      flashLight = NOT flashLight
      saveInterval = saveInterval * 60
      showDowntime = NOT showDowntime
      settingsRect:BGCOLOR = resourceBGColor
      resourceGrid:BGCOLOR = resourceBGColor.
    APPLY 'CHOOSE' TO btnDatePrompt.
    APPLY 'CHOOSE' TO btnDetail.
    APPLY 'CHOOSE' TO btnFlashLight.
    APPLY 'CHOOSE' TO btnShowDowntime.
    IF setConfigValues THEN
    ASSIGN /* done initially, don't want to reset values everytime */
      gridLine:BGCOLOR = gridLineColor
      gridLine:X = resourceGrid:X + 1
      gridLine:Y = resourceGrid:Y
      intervals:LIST-ITEMS = '{{&includes}/intervals.i}'
      intervals:INNER-LINES = intervals:NUM-ITEMS
      intervals:SCREEN-VALUE = intervals:ENTRY(intervalInit)
      boardDate:SCREEN-VALUE = STRING(TODAY)
      timeValue:SCREEN-VALUE = STRING(timeInit,'9999')
      saveFullBoard = fullBoard
      saveGridColor = gridBGColor
      saveDowntimeTop = downtimeTop
      {&configFields}.
    DISPLAY {&configFields}.
    EMPTY TEMP-TABLE jobColors.
    DO i = 1 TO EXTENT(colorPriority) + 2:
      CREATE jobColors.
      ASSIGN
        jobColors.priority = IF i GT 2 THEN colorPriority[i - 2] ELSE 0
        jobColors.idx = i
        jobColors.bgColorValue = IF i LE EXTENT(jobBGColor) THEN jobBGColor[i]
          ELSE IF i LE EXTENT(colorPriority) THEN customBGColor[i - EXTENT(jobBGColor)]
          ELSE IF i EQ EXTENT(colorPriority) + 1 THEN jobConflictBGColor
          ELSE downtimeConflictBGColor
        jobColors.fgColorValue = IF i LE EXTENT(jobFGColor) THEN jobFGColor[i]
          ELSE IF i LE EXTENT(colorPriority) THEN customFGColor[i - EXTENT(jobFGColor)]
          ELSE IF i EQ EXTENT(colorPriority) + 1 THEN jobConflictFGColor
          ELSE downtimeConflictFGColor
        jobColors.colorLabel = IF i LE EXTENT(jobLabel) THEN jobLabel[i]
          ELSE IF i LE EXTENT(colorPriority) THEN customLabel[i - EXTENT(jobLabel)]
          ELSE IF i EQ EXTENT(colorPriority) + 1 THEN 'Job Conflict'
          ELSE 'Downtime Conflict'
        jobColors.jobValue = IF i GT EXTENT(jobBGColor) AND
                  i LE EXTENT(colorPriority) THEN customValue[i - EXTENT(jobBGColor)]
          ELSE IF i EQ EXTENT(colorPriority) + 1 THEN 'Job.Conflict'
          ELSE 'Downtime.Conflict'
        jobColors.timeColor = i LE EXTENT(jobBGColor).
    END. /* do i */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLoginID s-object 
PROCEDURE getLoginID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opLoginID AS CHARACTER NO-UNDO.

  opLoginID = loginID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getResource s-object 
PROCEDURE getResource :
/*------------------------------------------------------------------------------
  Purpose:     read in resources used from file or extracted from ttblJob table
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE resourceName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  
  RUN msgFrame ('Load Resources').
  &SCOPED-DEFINE resourceListDef
  {{&loads}/resourceList.i}
  &UNDEFINE resourceListDef
  EMPTY TEMP-TABLE ttblResource.
  ASSIGN
    resourceList:HIDDEN IN FRAME {&FRAME-NAME} = YES
    resourceList:LIST-ITEM-PAIRS = ?
    resource# = 0.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/resources.dat')) NO-ECHO.
  REPEAT:
    IMPORT bTtblResource.
    IF CAN-FIND(FIRST ttblResource WHERE ttblResource.resource EQ bTtblResource.resource) THEN
    NEXT.
    {{&loads}\resourceUse.i bTtblResource.resource}
    CREATE ttblResource.
    BUFFER-COPY bTtblResource TO ttblResource.
    FIND priorityList NO-LOCK
         WHERE priorityList.resource EQ ttblResource.resource NO-ERROR.
    ASSIGN /* set a resources priority value */
      resource# = resource# + 1
      ttblResource.priority = IF AVAILABLE priorityList THEN priorityList.priority
                              ELSE ttblResource.sortOrder + 900.
    IF alphaSort THEN /* remove sort order set during load */
    ttblResource.sortOrder = 0.
  END.
  INPUT CLOSE.
  /* remove unused resources */
  IF NOT allResources AND resourceUse NE 'Without' THEN
  FOR EACH ttblResource EXCLUSIVE-LOCK:
    IF CAN-FIND(FIRST ttblJob WHERE ttblJob.resource EQ ttblResource.resource) OR
       CAN-FIND(FIRST pendingJob WHERE pendingJob.resource EQ ttblResource.resource) THEN
    NEXT.
    DELETE ttblResource.
    resource# = resource# - 1.
  END.
  FOR EACH ttblResource EXCLUSIVE-LOCK
      BY ttblResource.sortOrder BY ttblResource.resource
      WITH FRAME {&FRAME-NAME}:
    ASSIGN /* set order value to be used by selection list lookup */
      i = i + 1
      ttblResource.order = i
      ldummy = resourceList:ADD-LAST(ttblResource.resource,STRING(i)).
  END.
  IF autoSize AND
     resource# * hpixels + resource# * gap - gap GT
     resourceGrid:HEIGHT-PIXELS IN FRAME {&FRAME-NAME} - 5 THEN
  checkSpace: /* override size settings in config to show on one page */
  DO i = hpixels TO 14 BY -1:
    DO j = gap TO 0 BY -1:
      IF resource# * i + resource# * j - j LE
         resourceGrid:HEIGHT-PIXELS IN FRAME {&FRAME-NAME} - 5 THEN DO:
        ASSIGN
          hpixels = i - (IF downtimeSize GT 0 THEN 1 ELSE 0)
          gap = j.
        LEAVE checkSpace.
      END.
    END. /* do j */
  END. /* do i */
  ASSIGN /* calculate first/last/# on page, used to nav resources */
    resourceFirst = 1
    resourceLast = resource#.
  DO i = 1 TO resource#:
    IF i * hpixels + i * gap - gap LE
       resourceGrid:HEIGHT-PIXELS IN FRAME {&FRAME-NAME} - 5 THEN
    NEXT.
    resourceLast = i - 1.
    LEAVE.
  END.
  resourceGap = resourceLast - resourceFirst.
  /* set resource navigation buttons on/off */
  IF resourceLast EQ resource# THEN
  DISABLE {&firstNav} {&navButtons} WITH FRAME {&FRAME-NAME}.
  ELSE
  ENABLE {&navButtons} WITH FRAME {&FRAME-NAME}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getScenario s-object 
PROCEDURE getScenario :
/*------------------------------------------------------------------------------
  Purpose:     read all jobs from it's scenario file
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE version AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN msgFrame ('Load Scenario "' + scenario + '"').
  {{&includes}/getScenario.i}
  RUN msgFrame ('Load Pending Jobs').
  {{&includes}/{&Board}/getScenario.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gridLine s-object 
PROCEDURE gridLine :
/*------------------------------------------------------------------------------
  Purpose:     reposition the grid line based on selected object
  Parameters:  object widget handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      gridLine:HEIGHT-PIXELS = 2
      gridLine:BGCOLOR = gridLineColor
      gridLine:X = 1
      gridLine:Y = ipWidget:Y + ipWidget:HEIGHT-PIXELS / 2.
    {{&includes}/{&Board}/gridLine.i}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideJob s-object 
PROCEDURE hideJob :
/*------------------------------------------------------------------------------
  Purpose:     instead of deleting widgets, simply hide unused objects
  Parameters:  last used object count
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  {{&includes}/ttblWidgetHide.i "jobWidget" ipIdx}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideResource s-object 
PROCEDURE hideResource :
/*------------------------------------------------------------------------------
  Purpose:     instead of deleting widgets, simply hide unused objects
  Parameters:  last used object count
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = ipIdx + 1 TO EXTENT(resourceWidget):
    IF VALID-HANDLE(resourceWidget[i]) AND NOT resourceWidget[i]:HIDDEN THEN
    ASSIGN
      {{&includes}/{&Board}/hideResource.i}
      resourceWidget[i]:HIDDEN = YES
      resourceFirstButton[i]:HIDDEN = YES
      resourceLastButton[i]:HIDDEN = YES.
    ELSE
    LEAVE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideThreeD s-object 
PROCEDURE hideThreeD :
/*------------------------------------------------------------------------------
  Purpose:     instead of deleting widgets, simply hide unused objects
  Parameters:  last used object count
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  {{&includes}/ttblWidgetHide.i "threeDWidget" ipIdx}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ID s-object 
PROCEDURE ID :
/*------------------------------------------------------------------------------
  Purpose:     get selected ID stored in persistent procedure
  Parameters:  ID
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/id.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initMsgFrame s-object 
PROCEDURE initMsgFrame :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    DO WITH FRAME msgFrame:
        ASSIGN
            iMsg          = 0
            hMsgImage[1]  = msgImage-1:HANDLE
            hMsgImage[2]  = msgImage-2:HANDLE
            hMsgImage[3]  = msgImage-3:HANDLE
            hMsgImage[4]  = msgImage-4:HANDLE
            hMsgImage[5]  = msgImage-5:HANDLE
            hMsgImage[6]  = msgImage-6:HANDLE
            hMsgImage[7]  = msgImage-7:HANDLE
            hMsgImage[8]  = msgImage-8:HANDLE
            hMsgImage[9]  = msgImage-9:HANDLE
            hMsgImage[10] = msgImage-10:HANDLE
            hMsgImage[11] = msgImage-11:HANDLE
            hMsgImage[12] = msgImage-12:HANDLE
            hMsgImage[13] = msgImage-13:HANDLE
            hMsgImage[14] = msgImage-14:HANDLE
            hMsgImage[15] = msgImage-15:HANDLE
            hMsgImage[16] = msgImage-16:HANDLE
            hMsgImage[17] = msgImage-17:HANDLE
            hMsgImage[18] = msgImage-18:HANDLE
            hMsgImage[19] = msgImage-19:HANDLE
            hMsgImage[20] = msgImage-20:HANDLE
            hMsgImage[21] = msgImage-21:HANDLE
            hMsgImage[22] = msgImage-22:HANDLE
            hMsgImage[23] = msgImage-23:HANDLE
            hMsgText[1]   = msgText-1:HANDLE
            hMsgText[2]   = msgText-2:HANDLE
            hMsgText[3]   = msgText-3:HANDLE
            hMsgText[4]   = msgText-4:HANDLE
            hMsgText[5]   = msgText-5:HANDLE
            hMsgText[6]   = msgText-6:HANDLE
            hMsgText[7]   = msgText-7:HANDLE
            hMsgText[8]   = msgText-8:HANDLE
            hMsgText[9]   = msgText-9:HANDLE
            hMsgText[10]  = msgText-10:HANDLE
            hMsgText[11]  = msgText-11:HANDLE
            hMsgText[12]  = msgText-12:HANDLE
            hMsgText[13]  = msgText-13:HANDLE
            hMsgText[14]  = msgText-14:HANDLE
            hMsgText[15]  = msgText-15:HANDLE
            hMsgText[16]  = msgText-16:HANDLE
            hMsgText[17]  = msgText-17:HANDLE
            hMsgText[18]  = msgText-18:HANDLE
            hMsgText[19]  = msgText-19:HANDLE
            hMsgText[20]  = msgText-20:HANDLE
            hMsgText[21]  = msgText-21:HANDLE
            hMsgText[22]  = msgText-22:HANDLE
            hMsgText[23]  = msgText-23:HANDLE
            .
    END. /* frame msgframe */
    
    DO iIndex = 1 TO EXTENT(hMsgImage):
        hMsgImage[iIndex]:LOAD-IMAGE(?).
        hMsgText[iIndex]:SCREEN-VALUE = "".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobMoving s-object 
PROCEDURE jobMoving :
{{&includes}/{&Board}/jobMoving.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobSequencer s-object 
PROCEDURE jobSequencer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT VALID-HANDLE(jobSequencerHandle) THEN
  DO:
    RUN VALUE(findProgram('{&objects}','','/jobSequencer.w')) PERSISTENT SET jobSequencerHandle.
    RUN local-initialize IN jobSequencerHandle.
    RUN setPopup IN containerHandle (8,jobSequencerHandle).
    RUN passHandle IN jobSequencerHandle (THIS-PROCEDURE,'{&Board}').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobText s-object 
PROCEDURE jobText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&loads}/jobText.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagTime s-object 
PROCEDURE lagTime :
/*------------------------------------------------------------------------------
  Purpose:     access lag time popup from current selected job
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &IF '{&Board}' EQ 'Pro' &THEN
  IF VALID-HANDLE(currentJob) AND currentJob:SELECTED THEN
  RUN {&prompts}/lagTime.w (TO-ROWID(currentJob:PRIVATE-DATA)).
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadBoard s-object 
PROCEDURE loadBoard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMsg AS LOGICAL NO-UNDO.

  DEFINE VARIABLE loadProgram AS CHARACTER NO-UNDO.
  
  RUN setScreenStatus.
  IF ipMsg THEN
  RUN msgFrame ('Load Jobs into Scheduler {&Board}').
  IF runLoad THEN DO:
    {{&includes}/loadProgram.i}
  END.
  runLoad = YES.
  DO WITH FRAME {&FRAME-NAME}:
    RUN getScenario.
    RUN getResource.
    VIEW {&gridColumns}.
    RUN buildResource.
    {{&includes}/{&Board}/setSize.i}
    APPLY 'VALUE-CHANGED' TO intervals.
    APPLY 'ENTRY' TO intervals.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadColors s-object 
PROCEDURE loadColors :
/*------------------------------------------------------------------------------
  Purpose:     load custom colors used for background of grid
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE colorLabel AS CHARACTER NO-UNDO.
  DEFINE VARIABLE redColor AS INTEGER NO-UNDO.
  DEFINE VARIABLE greenColor AS INTEGER NO-UNDO.
  DEFINE VARIABLE blueColor AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN msgFrame ('Loading Background Colors').
  INPUT FROM VALUE(SEARCH('{&data}/gridColor.dat')) NO-ECHO.
  REPEAT:
    IMPORT colorLabel.
    IF colorLabel EQ 'COLOR' + STRING(gridBGColor) THEN
    DO:
      DO i = 1 TO EXTENT(intColor) - 1:
        IMPORT redColor greenColor blueColor.
        COLOR-TABLE:SET-RGB-VALUE(74 + i,RGB-VALUE(redColor,greenColor,blueColor)).
      END.
      LEAVE.
    END.
  END.
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadConfiguration s-object 
PROCEDURE loadConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     reload configuration values and rebuild entire board
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN getConfiguration (NO).
  jobMovingDisplay:BGCOLOR IN FRAME {&FRAME-NAME} = flashLightColor.
  IF saveDowntimeTop NE downtimeTop THEN
  DO: /* remove downtime widgets, need to re-create them as different type */
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
    DELETE WIDGET-POOL 'downtimePool' NO-ERROR.
    CREATE WIDGET-POOL 'downtimePool' PERSISTENT.
    ASSIGN
      downtimeIdx = 0
      saveDowntimeTop = downtimeTop.
    EMPTY TEMP-TABLE downtimeWidget.
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
    ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF
  END.
  IF saveFullBoard NE fullBoard THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      resourceGrid:HEIGHT-PIXELS = IF NOT fullBoard THEN 415
                                   ELSE FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 50
      resourceList:HEIGHT-PIXELS = resourceGrid:HEIGHT-PIXELS
      FRAME msgFrame:HEIGHT-PIXELS = resourceGrid:HEIGHT-PIXELS
      saveFullBoard = fullBoard
      saveGridColor = -1.
  END.
  RUN getResource.
  RUN setScreenStatus.
  IF saveGridColor NE gridBGColor THEN
  DO: /* have to repaint background grid, color has changed */
    RUN loadColors.
    RUN setGrid (YES).
    saveGridColor = gridBGColor.
  END.
  RUN buildResource.
  APPLY 'VALUE-CHANGED' TO intervals IN FRAME {&FRAME-NAME}.
  APPLY 'ENTRY' TO intervals.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveBack s-object 
PROCEDURE moveBack :
/*------------------------------------------------------------------------------
  Purpose:     move grid back one interval
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 25 TO 2 BY -1:
    ASSIGN
      intDate[i] = intDate[i - 1]
      intK[i] = intK[i - 1]
      intT[i] = intT[i - 1]
      intStime[i] = intStime[i - 1]
      intEtime[i] = intEtime[i - 1]
      intColor[i] = intColor[i - 1].
  END.
  ASSIGN
    intDate[1] = intDate[2] - (IF intStime[2] EQ 0 THEN 1 ELSE 0)
    intK[1] = intK[25]
    intT[1] = intT[25]
    intStime[1] = intStime[25]
    intEtime[1] = intEtime[25]
    intColor[1] = intColor[25].

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveBoardObjects s-object 
PROCEDURE moveBoardObjects :
/*------------------------------------------------------------------------------
  Purpose:     move board buttons, etc. to newly sized frame
  Parameters:  change in width value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  ASSIGN
    pWidget = FRAME {&FRAME-NAME}:HANDLE
    pWidget = pWidget:FIRST-CHILD
    pWidget = pWidget:FIRST-CHILD.
  DO WHILE VALID-HANDLE(pWidget):
    IF pWidget:TYPE EQ 'BUTTON' AND pWidget:PRIVATE-DATA NE ? THEN
    pWidget:X = pWidget:X + ipX.
    pWidget = pWidget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveForward s-object 
PROCEDURE moveForward :
/*------------------------------------------------------------------------------
  Purpose:     move grid forward one interval
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN
    intDate[25] = intDate[24] + (IF intStime[1] EQ 0 THEN 1 ELSE 0)
    intK[25] = intK[1]
    intT[25] = intT[1]
    intStime[25] = intStime[1]
    intEtime[25] = intEtime[1]
    intColor[25] = intColor[1].
  DO i = 1 TO 24:
    ASSIGN
      intDate[i] = intDate[i + 1]
      intK[i] = intK[i + 1]
      intT[i] = intT[i + 1]
      intStime[i] = intStime[i + 1]
      intEtime[i] = intEtime[i + 1]
      intColor[i] = intColor[i + 1].
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveInterval s-object 
PROCEDURE moveInterval :
/*------------------------------------------------------------------------------
  Purpose:     execute procedures to move grid one interval back/forward
  Parameters:  move procedure to execute based on button chosen
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMoveProc AS CHARACTER NO-UNDO.

  RUN setScreenStatus.
  RUN VALUE(ipMoveProc).
  RUN setGrid (NO).
  RUN buildPixelArray.
  RUN buildBoard (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE movePendingRes s-object 
PROCEDURE movePendingRes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT VALID-HANDLE(movePendingResHandle) THEN
  DO:
    moveResource = findProgram('{&objects}','','/movePendingRes.w').
    RUN VALUE(moveResource) PERSISTENT SET movePendingResHandle.
    RUN adm-initialize IN movePendingResHandle.
  END.
  RUN setPopup IN containerHandle (13,movePendingResHandle).
  RUN passHandle IN movePendingResHandle (THIS-PROCEDURE,'{&Board}','<Select ...>').
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveResource s-object 
PROCEDURE moveResource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT VALID-HANDLE(moveResourceHandle) THEN DO:
    moveResource = findProgram('{&objects}','','/moveResource.w').
    RUN VALUE(moveResource) PERSISTENT SET moveResourceHandle.
    RUN adm-initialize IN moveResourceHandle.
  END.
  RUN setPopup IN containerHandle (12,moveResourceHandle).
  RUN passHandle IN moveResourceHandle (THIS-PROCEDURE,'{&Board}','<Select ...>').
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveToJob s-object 
PROCEDURE moveToJob :
/*------------------------------------------------------------------------------
  Purpose:     move to next job from current selected job
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS INTEGER NO-UNDO.

  &IF '{&Board}' EQ 'Basic' &THEN
  DEFINE BUFFER buffJob FOR ttblJob.
  &ENDIF
  
  IF VALID-HANDLE(currentJob) AND currentJob:SELECTED THEN DO:
    FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(currentJob:PRIVATE-DATA).
    FIND FIRST buffJob NO-LOCK
         WHERE buffJob.job EQ ttblJob.job
           AND buffJob.resourceSequence EQ ttblJob.resourceSequence + ipMove NO-ERROR.
    IF NOT AVAILABLE buffJob THEN
    IF ipMove EQ 1 THEN
    FIND FIRST buffJob NO-LOCK USE-INDEX resourceSequence
         WHERE buffJob.job EQ ttblJob.job NO-ERROR.
    ELSE
    FIND LAST buffJob NO-LOCK USE-INDEX resourceSequence
         WHERE buffJob.job EQ ttblJob.job NO-ERROR.
    IF NOT AVAILABLE buffJob THEN RETURN.
    RUN positionBoard (ROWID(buffJob),buffJob.startDate,NO).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msgFrame s-object 
PROCEDURE msgFrame :
/*------------------------------------------------------------------------------
  Purpose:     display status message as board builds
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMsg AS CHARACTER NO-UNDO.

 IF ipMsg EQ ? THEN DO:
      RUN initMsgFrame. 
      SESSION:SET-WAIT-STATE('').
  END.
  
  IF NOT (showStatus OR openBoard) THEN RETURN.
  
  IF iMsg GT 0 THEN
  hMsgImage[iMsg]:LOAD-IMAGE("Graphics/16x16/check.png").
  
  IF ipMsg NE ? THEN DO:
    ASSIGN    
      iMsg = iMsg + 1
      hMsgText[iMsg]:SCREEN-VALUE = ipMsg
      .    
    hMsgImage[iMsg]:LOAD-IMAGE("Graphics/16x16/navigate_right.png").
  END.

  PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeSpan s-object 
PROCEDURE pDowntimeSpan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&includes}/{&Board}/pDowntimeSpan.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromPending s-object 
PROCEDURE pFromPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE priorEndDate  AS DATE NO-UNDO.
    DEFINE VARIABLE priorEndTime  AS INTEGER NO-UNDO.
    DEFINE VARIABLE priorDateTime AS INTEGER NO-UNDO INITIAL ?.

    DEFINE BUFFER bPendingJob FOR pendingJob.

    FOR EACH bPendingJob
          BY bPendingJob.job
          BY bPendingJob.resourceSequence
        :
      ASSIGN
        bPendingJob.startDate = TODAY
        bPendingJob.startTime = TIME
        .
      RUN newEnd (bPendingJob.timeSpan,bPendingJob.startDate,bPendingJob.startTime,
                  OUTPUT bPendingJob.endDate,OUTPUT bPendingJob.endTime).
    END. /* each bpendingjob */
    
    FOR EACH bPendingJob
        BREAK BY bPendingJob.dueDate
              BY bPendingJob.job
              BY bPendingJob.resourceSequence
        :
        CREATE ttblJob.
        BUFFER-COPY bPendingJob TO ttblJob.
        ASSIGN
          ttblJob.origStartDate = bPendingJob.startDate
          ttblJob.origStartTime = bPendingJob.startTime
          ttblJob.origEndDate   = bPendingJob.endDate
          ttblJob.origEndTime   = bPendingJob.endTime
          .
        ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
        ttblJob.endDateTime   = numericDateTime(ttblJob.endDate,ttblJob.endTime).
        ASSIGN
          ttblJob.jobBGColor = jobBGColor()
          ttblJob.jobFGColor = jobFGColor()
          ttblJob.statusLabel = jobStatus()
          .
        IF priorDateTime NE ? AND priorDateTime GE ttblJob.startDateTime THEN DO:
          ASSIGN
            ttblJob.startDate = priorEndDate
            ttblJob.startTime = priorEndTime
            .
          RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                      OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
          ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
          ttblJob.endDateTime   = numericDateTime(ttblJob.endDate,ttblJob.endTime).
          ASSIGN
            ttblJob.jobBGColor = jobBGColor()
            ttblJob.jobFGColor = jobFGColor()
            ttblJob.statusLabel = jobStatus()
            .
        END.
        RUN firstAvailable (ttblJob.resource,ROWID(ttblJob),ttblJob.timeSpan,
                            INPUT-OUTPUT ttblJob.startDateTime,
                            INPUT-OUTPUT ttblJob.endDateTime,
                            INPUT-OUTPUT ttblJob.startDate,
                            INPUT-OUTPUT ttblJob.startTime,
                            INPUT-OUTPUT ttblJob.endDate,
                            INPUT-OUTPUT ttblJob.endTime
                            ).
        RUN getPriorJobResource (ttblJob.job,ttblJob.resourceSequence,ttblJob.startDateTime,
                                 INPUT-OUTPUT ttblJob.startDate,INPUT-OUTPUT ttblJob.startTime).
        RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                    OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
        RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                          OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime,OUTPUT ttblJob.downtimeSpan).
        ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
        ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
        ASSIGN
          ttblJob.jobBGColor = jobBGColor()
          ttblJob.jobFGColor = jobFGColor()
          ttblJob.statusLabel = jobStatus()
          .
        ASSIGN
          priorEndDate  = ttblJob.endDate
          priorEndTime  = ttblJob.endTime
          priorDateTime = ttblJob.endDateTime
          .
        
        RUN pSetResourceSequence (bPendingJob.resource).
        ttblJob.sequenced = YES.
        IF LAST-OF(bPendingJob.job) THEN
        priorDateTime = ?.
        DELETE bPendingJob.
    END. /* each bpendingjob */
    RUN buildBoard (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromPendingByDueDate s-object 
PROCEDURE pFromPendingByDueDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bPendingJob FOR pendingJob.

    FOR EACH bPendingJob
        BREAK BY bPendingJob.dueDate
              BY bPendingJob.job
              BY bPendingJob.resourceSeq
        :
      IF LAST-OF(bPendingJob.job) THEN DO:
          ASSIGN
            bPendingJob.startDate = bPendingJob.dueDate - pendingDays
            bPendingJob.startTime = 0
            .
          RUN newEnd (bPendingJob.timeSpan,bPendingJob.startDate,bPendingJob.startTime,
                      OUTPUT bPendingJob.endDate,OUTPUT bPendingJob.endTime).
          CREATE ttblJob.
          BUFFER-COPY bPendingJob TO ttblJob.
          ASSIGN
            ttblJob.origStartDate = bPendingJob.startDate
            ttblJob.origStartTime = bPendingJob.startTime
            ttblJob.origEndDate   = bPendingJob.endDate
            ttblJob.origEndTime   = bPendingJob.endTime
            .
          ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
          ttblJob.endDateTime   = numericDateTime(ttblJob.endDate,ttblJob.endTime).

          RUN firstAvailable (ttblJob.resource,ROWID(ttblJob),ttblJob.timeSpan,
                              INPUT-OUTPUT ttblJob.startDateTime,
                              INPUT-OUTPUT ttblJob.endDateTime,
                              INPUT-OUTPUT ttblJob.startDate,
                              INPUT-OUTPUT ttblJob.startTime,
                              INPUT-OUTPUT ttblJob.endDate,
                              INPUT-OUTPUT ttblJob.endTime
                              ).
          RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                            OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime,OUTPUT ttblJob.downtimeSpan).
          ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
          ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).

          IF ttblJob.endDate GT TODAY + pendingLastDay THEN DO:
              DELETE ttblJob.
              NEXT.
          END. /* don't schedule, too far into the future */

          ASSIGN
            ttblJob.jobBGColor = jobBGColor()
            ttblJob.jobFGColor = jobFGColor()
            ttblJob.statusLabel = jobStatus()
            .

          RUN pSetResourceSequence (bPendingJob.resource).
          ttblJob.sequenced = YES.

          RUN pSetDueDateJob (ROWID(bPendingJob)).

          DELETE bPendingJob.
      END. /* last-of */
    END. /* each bpendingjob */
    
    RUN buildBoard (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPriorAvailable s-object 
PROCEDURE pPriorAvailable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&includes}/{&Board}/pPriorAvailable.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print s-object 
PROCEDURE print :
/*------------------------------------------------------------------------------
  Purpose:     run print dialog program so it has access to shared vars in
               avail from schedule.w
  Parameters:  report parameters
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  DEFINE VARIABLE hdle AS HANDLE NO-UNDO.
  DEFINE VARIABLE running AS LOGICAL NO-UNDO.

  RUN checkPopup IN containerHandle (4,OUTPUT running).
  IF running THEN RETURN.

  IF ipResource NE 'ALL' THEN
  ASSIGN
    resourceValueLo = ipResource
    resourceValueHi = ipResource.
  RUN {&prompts}/fieldFilter.w PERSISTENT SET hdle ('{&Board}','','',NO,NO,THIS-PROCEDURE,'print').
  RUN setPopup IN containerHandle (4,hdle).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDueDateJob s-object 
PROCEDURE pSetDueDateJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

    DEFINE VARIABLE priorStartDate AS DATE      NO-UNDO.
    DEFINE VARIABLE priorStartTime AS INTEGER   NO-UNDO.
    DEFINE VARIABLE priorDateTime  AS INTEGER   NO-UNDO INITIAL ?.
    DEFINE VARIABLE dDueDate       AS DATE      NO-UNDO.
    DEFINE VARIABLE cJob           AS CHARACTER NO-UNDO.

    DEFINE BUFFER bufPendingJob FOR pendingJob.

    ASSIGN
      dDueDate       = ttblJob.dueDate
      cJob           = ttblJob.job
      priorStartDate = ttblJob.startDate
      priorStartTime = ttblJob.startTime
      priorDateTime  = ttblJob.startDateTime
      .
    FOR EACH bufPendingJob
        WHERE bufPendingJob.dueDate EQ dDueDate
          AND bufPendingJob.job     EQ cJob
          AND ROWID(bufPendingJob)  NE iprRowID
        BREAK BY bufPendingJob.job
              BY bufPendingJob.resourceSeq DESCENDING
        :
        ASSIGN
          bufPendingJob.endDate = priorStartDate
          bufPendingJob.endTime = priorStartTime
          .
        RUN newStart (bufPendingJob.timeSpan,bufPendingJob.endDate,bufPendingJob.endTime,
                      OUTPUT bufPendingJob.startDate,OUTPUT bufPendingJob.startTime).
        bufPendingJob.startDateTime = numericDateTime(bufPendingJob.startDate,bufPendingJob.startTime).
        bufPendingJob.endDateTime   = numericDateTime(bufPendingJob.endDate,bufPendingJob.endTime).

        CREATE ttblJob.
        BUFFER-COPY bufPendingJob TO ttblJob.
        ASSIGN
          ttblJob.origStartDate = bufPendingJob.startDate
          ttblJob.origStartTime = bufPendingJob.startTime
          ttblJob.origEndDate   = bufPendingJob.endDate
          ttblJob.origEndTime   = bufPendingJob.endTime
          .
        /*
        RUN pPriorAvailable (ttblJob.resource,ROWID(ttblJob),ttblJob.timeSpan,
                             INPUT-OUTPUT ttblJob.startDateTime,
                             INPUT-OUTPUT ttblJob.endDateTime,
                             INPUT-OUTPUT ttblJob.startDate,
                             INPUT-OUTPUT ttblJob.startTime,
                             INPUT-OUTPUT ttblJob.endDate,
                             INPUT-OUTPUT ttblJob.endTime
                             ).

        RUN getPriorJobResource (ttblJob.job,ttblJob.resourceSequence,ttblJob.startDateTime,
                                 INPUT-OUTPUT ttblJob.startDate,INPUT-OUTPUT ttblJob.startTime).
        */
        RUN pDowntimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.endDate,ttblJob.endTime,
                           OUTPUT ttblJob.startDate,OUTPUT ttblJob.startTime,OUTPUT ttblJob.downtimeSpan).
        ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
        ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
        
        ASSIGN
          ttblJob.jobBGColor = jobBGColor()
          ttblJob.jobFGColor = jobFGColor()
          ttblJob.statusLabel = jobStatus()
          priorStartDate = ttblJob.startDate
          priorStartTime = ttblJob.startTime
          priorDateTime  = ttblJob.startDateTime
          .
        RUN pSetResourceSequence (bufPendingJob.resource).
        ttblJob.sequenced = YES.

        DELETE bufPendingJob.
    END. /* each bufpendingjob */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetResourceSequence s-object 
PROCEDURE pSetResourceSequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  FOR EACH buffJob EXCLUSIVE-LOCK WHERE buffJob.resource EQ ipResource
      BY buffJob.startDate BY buffJob.startTime:
    ASSIGN
      i = i + 1
      buffJob.jobSequence = i.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resourceClick s-object 
PROCEDURE resourceClick :
/*------------------------------------------------------------------------------
  Purpose:     call detail window when resource button clicked
  Parameters:  resource button handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  IF btnSave:SENSITIVE IN FRAME {&FRAME-NAME} EQ NO THEN RETURN.
  
  RUN detailResource (ipWidget:NAME,ipWidget:TOOLTIP).
  currentResource = ipWidget:NAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resourceFirstLast s-object 
PROCEDURE resourceFirstLast :
/*------------------------------------------------------------------------------
  Purpose:     reposition board to resource's first or last job
  Parameters:  resource name and first/last value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipDirection AS CHARACTER NO-UNDO.
  
  RUN gridLine (ipWidget:HANDLE).
  CASE ipDirection:
    WHEN 'First' THEN
    FIND FIRST ttblJob NO-LOCK USE-INDEX DateTimeIdx
         WHERE ttblJob.resource EQ ipWidget:NAME NO-ERROR.
    WHEN 'Last' THEN
    FIND LAST ttblJob NO-LOCK USE-INDEX DateTimeIdx
         WHERE ttblJob.resource EQ ipWidget:NAME NO-ERROR.
  END CASE.
  IF AVAILABLE ttblJob THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF boardDate:SCREEN-VALUE = STRING(ttblJob.startDate,'99/99/9999') THEN RETURN.
    boardDate:SCREEN-VALUE = STRING(ttblJob.startDate).
    APPLY 'RETURN' TO boardDate.
  END.
  &IF '{&Board}' NE 'Pro' &THEN
  ELSE
  MESSAGE 'No Jobs Exist for this Resource' VIEW-AS ALERT-BOX.
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runDetailJob s-object 
PROCEDURE runDetailJob :
/*------------------------------------------------------------------------------
  Purpose:     run the detail window persistent
  Parameters:  rowid of job, and rowid's from load process (comma delimited)
  Notes:       this is called from job sequencer
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipRowIDs AS CHARACTER NO-UNDO.

  detailWindow = NO.
  APPLY 'CHOOSE':U TO btnDetail IN FRAME {&FRAME-NAME}.
  RUN detailJob (ipRowID,ipRowIDs).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setColorType s-object 
PROCEDURE setColorType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.

  FOR EACH jobColors EXCLUSIVE-LOCK:
    jobColors.colorOn = (jobColors.timeColor AND ipType EQ 'T') OR
                        (NOT jobColors.timeColor AND ipType EQ 'S') OR ipType EQ 'A'.
  END. /* each jobcolors */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setGrid s-object 
PROCEDURE setGrid :
/*------------------------------------------------------------------------------
  Purpose:     display grid and set grid headers accordingly
  Parameters:  logical used to decide if entire grid is redone or only parts of it
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipSetGrid AS LOGICAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN msgFrame ('Setting Grid').
  rectWidth = TRUNCATE((FRAME {&FRAME-NAME}:WIDTH-PIXELS - resourceGrid:WIDTH-PIXELS) / 24,0).
  IF intDate[1] NE ? THEN
  boardDate:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(intDate[1]).
  DO i = 1 TO 24 WITH FRAME {&FRAME-NAME}:
    CASE i:
      {{&includes}/setGrid.i 1}
      {{&includes}/setGrid.i 2}
      {{&includes}/setGrid.i 3}
      {{&includes}/setGrid.i 4}
      {{&includes}/setGrid.i 5}
      {{&includes}/setGrid.i 6}
      {{&includes}/setGrid.i 7}
      {{&includes}/setGrid.i 8}
      {{&includes}/setGrid.i 9}
      {{&includes}/setGrid.i 10}
      {{&includes}/setGrid.i 11}
      {{&includes}/setGrid.i 12}
      {{&includes}/setGrid.i 13}
      {{&includes}/setGrid.i 14}
      {{&includes}/setGrid.i 15}
      {{&includes}/setGrid.i 16}
      {{&includes}/setGrid.i 17}
      {{&includes}/setGrid.i 18}
      {{&includes}/setGrid.i 19}
      {{&includes}/setGrid.i 20}
      {{&includes}/setGrid.i 21}
      {{&includes}/setGrid.i 22}
      {{&includes}/setGrid.i 23}
      {{&includes}/setGrid.i 24}
    END CASE.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobColors s-object 
PROCEDURE setJobColors :
/*------------------------------------------------------------------------------
  Purpose:     set all job colors, called from colorGrid.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE('GENERAL').
  FOR EACH ttblJob EXCLUSIVE-LOCK:
    ASSIGN
      ttblJob.jobBGColor = jobBGColor()
      ttblJob.jobFGColor = jobFGColor()
      ttblJob.statusLabel = jobStatus()
      .
  END.
  APPLY 'RETURN' TO boardDate IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setObjectName s-object 
PROCEDURE setObjectName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.

  RUN objectName IN containerHandle (ipType,ipValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setResourceJobDetail s-object 
PROCEDURE setResourceJobDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResourceJobDetail AS LOGICAL NO-UNDO.

  detailWindow = NOT ipResourceJobDetail.
  APPLY 'CHOOSE':U TO btnDetail IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setScreenStatus s-object 
PROCEDURE setScreenStatus :
/*------------------------------------------------------------------------------
  Purpose:     set screen message frame or simply suspend windows from
               refreshing screen
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  
  ldummy = SESSION:SET-WAIT-STATE('GENERAL').
  IF showStatus OR openBoard THEN
  VIEW FRAME msgFrame.
  ELSE
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSize s-object 
PROCEDURE setSize :
/*------------------------------------------------------------------------------
  Purpose:     called from window container, sets size and builds board
  Parameters:  calling windows height & width
  Notes:       this is the beginning process, everything starts here
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHeight AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS DECIMAL NO-UNDO.

  RUN msgFrame ('Set Scheduler Screen Size').
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = ipHeight - 39
      FRAME {&FRAME-NAME}:WIDTH-PIXELS = ipWidth - 13
      FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = ipHeight - 39
      FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = ipWidth - 13
      settingsRect:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 2
      intervalRect:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 2
      gridLine:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 2.
    RUN ID.
    RUN getConfiguration (YES).
    ASSIGN
      resourceGrid:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS -
                   IF fullBoard THEN 50 ELSE 240
      resourceList:HEIGHT-PIXELS = resourceGrid:HEIGHT-PIXELS
      FRAME msgFrame:HEIGHT-PIXELS = resourceGrid:HEIGHT-PIXELS
      FRAME msgFrame:WIDTH-PIXELS = ipWidth - 13 - resourceGrid:WIDTH-PIXELS
      COLOR-TABLE:NUM-ENTRIES = 99.
    RUN moveBoardObjects (INTEGER((FRAME {&FRAME-NAME}:WIDTH-PIXELS - 787) / 2)).
    DO i = 1 TO EXTENT(intColor) - 1:
      ldummy = COLOR-TABLE:SET-DYNAMIC(74 + i,YES).
    END.
    RUN setScreenStatus.
    RUN loadColors.
    RUN setGrid (YES).
    RUN msgFrame ('Load Jobs into Scheduler {&Board}').
    {{&includes}/loadProgram.i}
    RUN getDowntime.
    RUN getCapacity.
    RUN buildScenario.
    RUN loadBoard (NO).
  END.
  startTimeOK = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sharedVars s-object 
PROCEDURE sharedVars :
/*------------------------------------------------------------------------------
  Purpose:     run sharedVars.w from container window menu click
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN {&prompts}/sharedVars.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showBoard s-object 
PROCEDURE showBoard :
/*------------------------------------------------------------------------------
  Purpose:     after creating all objects, show them now
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  RUN timeLine.
  {{&includes}/{&Board}/hidelightBulb.i}
  {{&includes}/ttblWidgetShow.i "jobWidget" jobIdx NO}
  {{&includes}/{&Board}/showBoard.i}
  IF threeD THEN
  {{&includes}/ttblWidgetShow.i "threeDWidget" threeDIdx NO}
  {{&includes}/{&Board}/showDowntime.i}
  openBoard = NO.
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE statusCheckoff s-object 
PROCEDURE statusCheckoff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &IF '{&Board}' EQ 'Pro' &THEN
  IF VALID-HANDLE(currentJob) AND currentJob:SELECTED THEN
  APPLY 'CHOOSE':U TO btnComplete IN FRAME {&FRAME-NAME}.
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE timeLine s-object 
PROCEDURE timeLine :
/*------------------------------------------------------------------------------
  Purpose:     position red vertical line indicating current system time
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvPixel AS DECIMAL NO-UNDO.
  DEFINE VARIABLE minPixel AS DECIMAL NO-UNDO.
  DEFINE VARIABLE packOptionPromptSave AS LOGICAL NO-UNDO.

  IF NOT startTimeOK THEN RETURN.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      minPixel = resourceGrid:WIDTH-PIXELS
      lvDateTime = numericDateTime(TODAY,TIME)
      lvPixel = getDateTimePixel(lvDateTime)
      timeLine:X = IF lvPixel LT 1 OR lvPixel GE maxPixel THEN 1
                   ELSE minPixel + lvPixel
      timeLine:HEIGHT-PIXELS = IF lvPixel LT 1 OR lvPixel GE maxPixel THEN 1
                               ELSE resourceGrid:HEIGHT-PIXELS
      timeLine:BGCOLOR = timeLineColor
      timeLine:HIDDEN = IF flashTimeLine THEN NOT timeLine:HIDDEN ELSE NO
      gridLine:BGCOLOR = gridLineColor
      gridLine:HIDDEN = IF flashGridLine THEN NOT gridLine:HIDDEN ELSE NO
      .
    &IF '{&Board}' EQ 'Pro' &THEN
    IF saveInterval NE 0 THEN DO: 
      IF saveTimeInterval GT saveInterval THEN DO:
        MESSAGE 'Save Scheduler Pro?' VIEW-AS ALERT-BOX
          QUESTION BUTTONS YES-NO TITLE 'Auto Save Prompt'
          UPDATE saveNow AS LOGICAL.
        IF saveNow THEN
        APPLY 'CHOOSE':U TO btnSave.
        saveTimeInterval = 0.
      END. /* if saveinterval */
      ELSE
      saveTimeInterval = saveTimeInterval + .5.
    END. /* saveinterval ne 0 */
    accumTimeInterval = ETIME / 1000.
    RUN displayLastSave IN containerHandle (accumTimeInterval).    
    IF monitorInterval NE 0 THEN DO:
      IF autoMonitorInterval GT monitorInterval * 60 THEN DO:
        IF btnSave:SENSITIVE EQ NO THEN DO:
          autoMonitorInterval = 0.
          RUN setAutoMonitorImage IN containerHandle.
          RUN asiDC.
          ASSIGN
            packOptionPromptSave = packOptionPrompt
            packOptionPrompt     = NO
            .
          RUN packBoard.
          packOptionPrompt = packOptionPromptSave.
          APPLY 'CHOOSE':U TO btnSave.
          autoMonitorInterval = 0.
        END. /* if btnsave */
      END. /* if gt monirotinterval */
      ELSE
      autoMonitorInterval = autoMonitorInterval + .5.
    END. /* monitorinterval ne 0 */
    &ELSEIF '{&Board}' EQ 'View' &THEN
    IF viewRefresh NE 0 THEN DO:
      IF viewRefreshInterval GT viewRefresh * 60 THEN DO:
        APPLY 'CHOOSE':U TO btnTimeLine.
        RUN viewRefresh.
      END. /* if gt viewrefresh */
      ELSE 
      viewRefreshInterval = viewRefreshInterval + .5.
    END. /* viewrefresh ne 0 */
    &ENDIF
  END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewRefresh s-object 
PROCEDURE viewRefresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  viewRefreshInterval = 0.
  RUN loadBoard (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDateTimePixel s-object 
FUNCTION getDateTimePixel RETURNS INTEGER
  (ipDateTime AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:     locate screen pixel position based on date & time YYYYMMDD.TTTTT
  Parameters:  date & time, output pixel position
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnPixel AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF ipDateTime LT dateTimePixel[1] THEN
  rtnPixel = 0.
  ELSE
  IF ipDateTime GT dateTimePixel[maxPixel] THEN
  rtnPixel = maxPixel + 1.
  ELSE
  DO i = 1 TO maxPixel:
    IF ipDateTime GT dateTimePixel[i] THEN NEXT.
    rtnPixel = i.
    LEAVE.
  END.
  RETURN rtnPixel.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION jobBGColor s-object 
FUNCTION jobBGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  {{&includes}/jobBGColor.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION jobFGColor s-object 
FUNCTION jobFGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  {{&includes}/jobFGColor.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime s-object 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
  {{&includes}/numericDateTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pixelDate s-object 
FUNCTION pixelDate RETURNS DATE
  (ipDateTime AS DECIMAL) :
  {{&includes}/pixelDate.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pixelTime s-object 
FUNCTION pixelTime RETURNS INTEGER
  (ipDateTime AS DECIMAL) :
  {{&includes}/pixelTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION timeSpan s-object 
FUNCTION timeSpan RETURNS INTEGER PRIVATE
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER) :
  {{&includes}/timeSpan.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

