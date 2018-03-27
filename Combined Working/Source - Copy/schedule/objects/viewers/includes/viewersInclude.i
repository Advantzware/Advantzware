&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : viewersInclude.i
    Purpose     : shared procedures

    Syntax      : {{&includes}/{&Board}/viewersInclude.i}

    Description : used in jobBrowse.w and resourceDetail.w

    Author(s)   : Ron Stark
    Created     : 5.29.2004
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(useTtbl) EQ 0 &THEN
&SCOPED-DEFINE useTtbl ttblJob
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkDowntimeConflict Include 
FUNCTION checkDowntimeConflict RETURNS LOGICAL
  (ipResource AS CHARACTER,ipStartDateTime AS DECIMAL,ipEndDateTime AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkJobConflict Include 
FUNCTION checkJobConflict RETURNS LOGICAL
  (ipResource AS CHARACTER,ipStartDateTime AS DECIMAL,
   ipEndDateTime AS DECIMAL,ipRowID AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD invalidMove Include 
FUNCTION invalidMove RETURNS LOGICAL
  (ipJob AS CHARACTER,ipResourceSequence AS INTEGER,
   ipStartDateTime AS DECIMAL,ipRowID AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime Include 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD timeSpan Include 
FUNCTION timeSpan RETURNS INTEGER
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCellColumns Include 
PROCEDURE getCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE colField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.

  IF CAN-FIND(FIRST browseColumn) THEN RETURN.

  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/columns.dat')) NO-ECHO.
  REPEAT WITH FRAME {&FRAME-NAME}:
    CREATE browseColumn.
    IMPORT browseColumn.
    IF browseColumn.colLocked OR browseColumn.colName EQ 'Status'
   &IF DEFINED(dynColumns) EQ 0 &THEN OR browseColumn.colHidden &ENDIF
    THEN NEXT.
    IF browseColumn.colName EQ 'calcTimeField' THEN
    ASSIGN
      pHandle = {&BROWSE-NAME}:ADD-CALC-COLUMN('CHARACTER','X(11)','',browseColumn.colName)
      pHandle:LABEL-BGCOLOR = IF browseColumn.colHidden THEN 7 ELSE ?
      .
    ELSE
    ASSIGN
      colField = '{&useTtbl}.' + browseColumn.colName
      pHandle = {&BROWSE-NAME}:ADD-LIKE-COLUMN(colField)
      pHandle:LABEL-BGCOLOR = IF browseColumn.colHidden THEN 7 ELSE 14
      pHandle:PRIVATE-DATA = '14'
      .
    pHandle:LABEL = browseColumn.colLabel.
  END.
  IF browseColumn.colOrder EQ 0 THEN
  DELETE browseColumn.
  INPUT CLOSE.

  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
    IF cellColumn[i]:DATA-TYPE EQ 'DATE' THEN
    cellColumn[i]:WIDTH-CHARS = 10.
    IF cellColumn[i]:NAME EQ 'userValue' THEN
    cellColumn[i]:WIDTH-CHARS = 80.
    IF cellColumn[i]:NAME BEGINS 'udfField' THEN
    ASSIGN
      idx = INTEGER(SUBSTRING(cellColumn[i]:NAME,9,2))
      cellColumn[i]:LABEL = udfLabel[idx]
      cellColumn[i]:WIDTH-CHARS = udfWidth[idx]
      .
    IF cellColumn[i]:NAME BEGINS 'userField' THEN
    ASSIGN
      idx = INTEGER(SUBSTRING(cellColumn[i]:NAME,10,2))
      cellColumn[i]:LABEL = userLabel[idx]
      cellColumn[i]:WIDTH-CHARS = userWidth[idx]
      .
    IF cellColumn[i]:NAME EQ 'startDate' THEN
    startDateCol = i.
    ELSE
    IF cellColumn[i]:NAME EQ 'endDate' THEN
    endDateCol = i.
    ELSE
    IF cellColumn[i]:NAME EQ 'lagTime' THEN
    lagTimeCol = i.
    ELSE
    IF cellColumn[i]:NAME EQ ? THEN DO:
      CASE cellColumn[i]:LABEL:
        WHEN 'Start Time' THEN
        startTimeCol = i.
        WHEN 'End Time' THEN
        endTimeCol = i.
        WHEN 'Job Time' THEN
        jobTimeCol = i.
        WHEN 'Downtime' THEN
        downtimeCol = i.
        WHEN 'Total Time' THEN
        totalTimeCol = i.
      END CASE.
    END. /* name eq ? */
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getConfiguration Include 
PROCEDURE getConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
  &IF DEFINED(detailButton) NE 0 &THEN
  ASSIGN
    btnDetail:TOOLTIP IN FRAME {&FRAME-NAME} =
      btnDetail:PRIVATE-DATA + ' ' + STRING(NOT resourceJobDetail,'On/Off')
    btnDatePrompt:TOOLTIP IN FRAME {&FRAME-NAME} =
      btnDatePrompt:PRIVATE-DATA + ' ' + STRING(NOT datePrompt,'On/Off').
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE positionBoard Include 
PROCEDURE positionBoard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipShowBoard AS LOGICAL NO-UNDO.

  IF AVAILABLE {&useTtbl} THEN
  RUN positionBoard IN boardHandle (ROWID({&useTtbl}),{&useTtbl}.startDate,ipShowBoard).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenBrowse Include 
PROCEDURE reopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&viewers}/includes/setFilterFlag.i {&browseName}}
  CASE columnLabel:
    WHEN 'dueDate' THEN
    IF ascendingSort THEN RUN byDueDateAscending.
    ELSE RUN byDueDateDescending.
    WHEN 'endDate' THEN
    IF ascendingSort THEN RUN byEndDateAscending.
    ELSE RUN byEndDateDescending.
    WHEN 'job' THEN
    IF ascendingSort THEN RUN byJobAscending.
    ELSE RUN byJobDescending.
    WHEN 'jobSequence' THEN
    IF ascendingSort THEN RUN byJobSequenceAscending.
    ELSE RUN byJobSequenceDescending.
    WHEN 'origEndDate' THEN
    IF ascendingSort THEN RUN byOrigEndDateAscending.
    ELSE RUN byOrigEndDateDescending.
    WHEN 'origStartDate' THEN
    IF ascendingSort THEN RUN byOrigStartDateAscending.
    ELSE RUN byOrigStartDateDescending.
    WHEN 'prodDate' THEN
    IF ascendingSort THEN RUN byProdDateAscending.
    ELSE RUN byProdDateDescending.
    WHEN 'resource' THEN
    IF ascendingSort THEN RUN byResourceAscending.
    ELSE RUN byResourceDescending.
    WHEN 'resourceSequence' THEN
    IF ascendingSort THEN RUN byResourceSequenceAscending.
    ELSE RUN byResourceSequenceDescending.
    WHEN 'startDate' THEN
    IF ascendingSort THEN RUN byStartDateAscending.
    ELSE RUN byStartDateDescending.
    WHEN 'userValue' THEN
    IF ascendingSort THEN RUN byUserValueAscending.
    ELSE RUN byUserValueDescending.
    {{&viewers}/includes/byReopenBrowse.i "udf" 01}
    {{&viewers}/includes/byReopenBrowse.i "udf" 02}
    {{&viewers}/includes/byReopenBrowse.i "udf" 03}
    {{&viewers}/includes/byReopenBrowse.i "udf" 04}
    {{&viewers}/includes/byReopenBrowse.i "udf" 05}
    {{&viewers}/includes/byReopenBrowse.i "udf" 06}
    {{&viewers}/includes/byReopenBrowse.i "udf" 07}
    {{&viewers}/includes/byReopenBrowse.i "udf" 08}
    {{&viewers}/includes/byReopenBrowse.i "udf" 09}
    {{&viewers}/includes/byReopenBrowse.i "udf" 10}
    {{&viewers}/includes/byReopenBrowse.i "udf" 11}
    {{&viewers}/includes/byReopenBrowse.i "udf" 12}
    {{&viewers}/includes/byReopenBrowse.i "udf" 13}
    {{&viewers}/includes/byReopenBrowse.i "udf" 14}
    {{&viewers}/includes/byReopenBrowse.i "udf" 15}
    {{&viewers}/includes/byReopenBrowse.i "udf" 16}
    {{&viewers}/includes/byReopenBrowse.i "udf" 17}
    {{&viewers}/includes/byReopenBrowse.i "udf" 18}
    {{&viewers}/includes/byReopenBrowse.i "udf" 19}
    {{&viewers}/includes/byReopenBrowse.i "udf" 20}
    {{&viewers}/includes/byReopenBrowse.i "user" 01}
    {{&viewers}/includes/byReopenBrowse.i "user" 02}
    {{&viewers}/includes/byReopenBrowse.i "user" 03}
    {{&viewers}/includes/byReopenBrowse.i "user" 04}
    {{&viewers}/includes/byReopenBrowse.i "user" 05}
    {{&viewers}/includes/byReopenBrowse.i "user" 06}
    {{&viewers}/includes/byReopenBrowse.i "user" 07}
    {{&viewers}/includes/byReopenBrowse.i "user" 08}
    {{&viewers}/includes/byReopenBrowse.i "user" 09}
    {{&viewers}/includes/byReopenBrowse.i "user" 10}
    {{&viewers}/includes/byReopenBrowse.i "user" 11}
    {{&viewers}/includes/byReopenBrowse.i "user" 12}
    {{&viewers}/includes/byReopenBrowse.i "user" 13}
    {{&viewers}/includes/byReopenBrowse.i "user" 14}
    {{&viewers}/includes/byReopenBrowse.i "user" 15}
    {{&viewers}/includes/byReopenBrowse.i "user" 16}
    {{&viewers}/includes/byReopenBrowse.i "user" 17}
    {{&viewers}/includes/byReopenBrowse.i "user" 18}
    {{&viewers}/includes/byReopenBrowse.i "user" 19}
    {{&viewers}/includes/byReopenBrowse.i "user" 20}
    {{&viewers}/includes/byReopenBrowse.i "user" 21}
    {{&viewers}/includes/byReopenBrowse.i "user" 22}
    {{&viewers}/includes/byReopenBrowse.i "user" 23}
    {{&viewers}/includes/byReopenBrowse.i "user" 24}
    {{&viewers}/includes/byReopenBrowse.i "user" 25}
    {{&viewers}/includes/byReopenBrowse.i "user" 26}
    {{&viewers}/includes/byReopenBrowse.i "user" 27}
    {{&viewers}/includes/byReopenBrowse.i "user" 28}
    {{&viewers}/includes/byReopenBrowse.i "user" 29}
    {{&viewers}/includes/byReopenBrowse.i "user" 30}
    {{&viewers}/includes/byReopenBrowse.i "user" 31}
    {{&viewers}/includes/byReopenBrowse.i "user" 32}
    {{&viewers}/includes/byReopenBrowse.i "user" 33}
    {{&viewers}/includes/byReopenBrowse.i "user" 34}
    {{&viewers}/includes/byReopenBrowse.i "user" 35}
    {{&viewers}/includes/byReopenBrowse.i "user" 36}
    {{&viewers}/includes/byReopenBrowse.i "user" 37}
    {{&viewers}/includes/byReopenBrowse.i "user" 38}
    {{&viewers}/includes/byReopenBrowse.i "user" 39}
    {{&viewers}/includes/byReopenBrowse.i "user" 40}
    {{&viewers}/includes/byReopenBrowse.i "user" 41}
    {{&viewers}/includes/byReopenBrowse.i "user" 42}
    {{&viewers}/includes/byReopenBrowse.i "user" 43}
    {{&viewers}/includes/byReopenBrowse.i "user" 44}
    {{&viewers}/includes/byReopenBrowse.i "user" 45}
    {{&viewers}/includes/byReopenBrowse.i "user" 46}
    {{&viewers}/includes/byReopenBrowse.i "user" 47}
    {{&viewers}/includes/byReopenBrowse.i "user" 48}
    {{&viewers}/includes/byReopenBrowse.i "user" 49}
    {{&viewers}/includes/byReopenBrowse.i "user" 50}
    {{&viewers}/includes/byReopenBrowse.i "user" 51}
    {{&viewers}/includes/byReopenBrowse.i "user" 52}
    {{&viewers}/includes/byReopenBrowse.i "user" 53}
    {{&viewers}/includes/byReopenBrowse.i "user" 54}
    {{&viewers}/includes/byReopenBrowse.i "user" 55}
    {{&viewers}/includes/byReopenBrowse.i "user" 56}
    {{&viewers}/includes/byReopenBrowse.i "user" 57}
    {{&viewers}/includes/byReopenBrowse.i "user" 58}
    {{&viewers}/includes/byReopenBrowse.i "user" 59}
    {{&viewers}/includes/byReopenBrowse.i "user" 60}
    {{&viewers}/includes/byReopenBrowse.i "user" 61}
    {{&viewers}/includes/byReopenBrowse.i "user" 62}
    {{&viewers}/includes/byReopenBrowse.i "user" 63}
    {{&viewers}/includes/byReopenBrowse.i "user" 64}
    {{&viewers}/includes/byReopenBrowse.i "user" 65}
    {{&viewers}/includes/byReopenBrowse.i "user" 66}
    {{&viewers}/includes/byReopenBrowse.i "user" 67}
    {{&viewers}/includes/byReopenBrowse.i "user" 68}
    {{&viewers}/includes/byReopenBrowse.i "user" 69}
    {{&viewers}/includes/byReopenBrowse.i "user" 70}
    {{&viewers}/includes/byReopenBrowse.i "user" 71}
    {{&viewers}/includes/byReopenBrowse.i "user" 72}
    {{&viewers}/includes/byReopenBrowse.i "user" 73}
    {{&viewers}/includes/byReopenBrowse.i "user" 74}
    {{&viewers}/includes/byReopenBrowse.i "user" 75}
    {{&viewers}/includes/byReopenBrowse.i "user" 76}
    {{&viewers}/includes/byReopenBrowse.i "user" 77}
    {{&viewers}/includes/byReopenBrowse.i "user" 78}
    {{&viewers}/includes/byReopenBrowse.i "user" 79}
    {{&viewers}/includes/byReopenBrowse.i "user" 80}
    {{&viewers}/includes/byReopenBrowse.i "user" 81}
    {{&viewers}/includes/byReopenBrowse.i "user" 82}
    {{&viewers}/includes/byReopenBrowse.i "user" 83}
    {{&viewers}/includes/byReopenBrowse.i "user" 84}
    {{&viewers}/includes/byReopenBrowse.i "user" 85}
    {{&viewers}/includes/byReopenBrowse.i "user" 86}
    {{&viewers}/includes/byReopenBrowse.i "user" 87}
    {{&viewers}/includes/byReopenBrowse.i "user" 88}
    {{&viewers}/includes/byReopenBrowse.i "user" 89}
    {{&viewers}/includes/byReopenBrowse.i "user" 90}
    {{&viewers}/includes/byReopenBrowse.i "user" 91}
    {{&viewers}/includes/byReopenBrowse.i "user" 92}
    {{&viewers}/includes/byReopenBrowse.i "user" 93}
    {{&viewers}/includes/byReopenBrowse.i "user" 94}
    {{&viewers}/includes/byReopenBrowse.i "user" 95}
    {{&viewers}/includes/byReopenBrowse.i "user" 96}
    {{&viewers}/includes/byReopenBrowse.i "user" 97}
    {{&viewers}/includes/byReopenBrowse.i "user" 98}
  END CASE.
END PROCEDURE.

&SCOPED-DEFINE startDatePhrase ~
AND {&useTtbl}.startDate GE fromDate AND {&useTtbl}.startDate LE toDate
IF fromDate EQ ? THEN fromDate = {{&includes}/firstDate.i}.
IF toDate EQ ? THEN toDate = {{&includes}/lastDate.i}.
  
&SCOPED-DEFINE dateRangePhrase ~
AND {&useTtbl}.dueDate GE fromDueDate AND {&useTtbl}.dueDate LE toDueDate ~
 BY {&useTtbl}.dueDate ~{&sorting} BY {&useTtbl}.dueTime ~{&sorting}

PROCEDURE byDueDateAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byDueDateDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase ~
AND {&useTtbl}.endDate GE fromDate AND {&useTtbl}.endDate LE toDate ~
 BY {&useTtbl}.endDate ~{&sorting} BY {&useTtbl}.endTime ~{&sorting}

PROCEDURE byEndDateAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byEndDateDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase {&startDatePhrase} ~
 BY {&useTtbl}.job ~{&sorting} BY {&useTtbl}.resourceSequence ~{&sorting}

PROCEDURE byJobAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byJobDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase {&startDatePhrase} ~
 BY {&useTtbl}.jobSequence ~{&sorting}

PROCEDURE byJobSequenceAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byJobSequenceDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase ~
AND {&useTtbl}.origEndDate GE fromDate AND {&useTtbl}.origEndDate LE toDate ~
 BY {&useTtbl}.origEndDate ~{&sorting} BY {&useTtbl}.origEndTime ~{&sorting}

PROCEDURE byOrigEndDateAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byOrigEndDateDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase ~
AND {&useTtbl}.origStartDate GE fromDate AND {&useTtbl}.origStartDate LE toDate ~
 BY {&useTtbl}.origStartDate ~{&sorting} BY {&useTtbl}.origStartTime ~{&sorting}

PROCEDURE byOrigStartDateAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byOrigStartDateDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase ~
AND {&useTtbl}.prodDate GE fromDate AND {&useTtbl}.prodDate LE toDate ~
 BY {&useTtbl}.prodDate ~{&sorting} BY {&useTtbl}.dueTime ~{&sorting}

PROCEDURE byProdDateAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byProdDateDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase {&startDatePhrase} ~
 BY {&useTtbl}.resource ~{&sorting} BY {&useTtbl}.jobSequence ~{&sorting}

PROCEDURE byResourceAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byResourceDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase {&startDatePhrase} ~
 BY {&useTtbl}.resourceSequence ~{&sorting}

PROCEDURE byResourceSequenceAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byResourceSequenceDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase {&startDatePhrase} ~
 BY {&useTtbl}.startDate ~{&sorting} BY {&useTtbl}.startTime ~{&sorting}

PROCEDURE byStartDateAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byStartDateDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

&SCOPED-DEFINE dateRangePhrase {&startDatePhrase} ~
 BY {&useTtbl}.userValue ~{&sorting}

PROCEDURE byUserValueAscending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE byUserValueDescending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

{{&viewers}/includes/reopenBrowse.i "user" 01}
{{&viewers}/includes/reopenBrowse.i "user" 02}
{{&viewers}/includes/reopenBrowse.i "user" 03}
{{&viewers}/includes/reopenBrowse.i "user" 04}
{{&viewers}/includes/reopenBrowse.i "user" 05}
{{&viewers}/includes/reopenBrowse.i "user" 06}
{{&viewers}/includes/reopenBrowse.i "user" 07}
{{&viewers}/includes/reopenBrowse.i "user" 08}
{{&viewers}/includes/reopenBrowse.i "user" 09}
{{&viewers}/includes/reopenBrowse.i "user" 10}
{{&viewers}/includes/reopenBrowse.i "user" 11}
{{&viewers}/includes/reopenBrowse.i "user" 12}
{{&viewers}/includes/reopenBrowse.i "user" 13}
{{&viewers}/includes/reopenBrowse.i "user" 14}
{{&viewers}/includes/reopenBrowse.i "user" 15}
{{&viewers}/includes/reopenBrowse.i "user" 16}
{{&viewers}/includes/reopenBrowse.i "user" 17}
{{&viewers}/includes/reopenBrowse.i "user" 18}
{{&viewers}/includes/reopenBrowse.i "user" 19}
{{&viewers}/includes/reopenBrowse.i "user" 20}
{{&viewers}/includes/reopenBrowse.i "user" 21}
{{&viewers}/includes/reopenBrowse.i "user" 22}
{{&viewers}/includes/reopenBrowse.i "user" 23}
{{&viewers}/includes/reopenBrowse.i "user" 24}
{{&viewers}/includes/reopenBrowse.i "user" 25}
{{&viewers}/includes/reopenBrowse.i "user" 26}
{{&viewers}/includes/reopenBrowse.i "user" 27}
{{&viewers}/includes/reopenBrowse.i "user" 28}
{{&viewers}/includes/reopenBrowse.i "user" 29}
{{&viewers}/includes/reopenBrowse.i "user" 30}
{{&viewers}/includes/reopenBrowse.i "user" 31}
{{&viewers}/includes/reopenBrowse.i "user" 32}
{{&viewers}/includes/reopenBrowse.i "user" 33}
{{&viewers}/includes/reopenBrowse.i "user" 34}
{{&viewers}/includes/reopenBrowse.i "user" 35}
{{&viewers}/includes/reopenBrowse.i "user" 36}
{{&viewers}/includes/reopenBrowse.i "user" 37}
{{&viewers}/includes/reopenBrowse.i "user" 38}
{{&viewers}/includes/reopenBrowse.i "user" 39}
{{&viewers}/includes/reopenBrowse.i "user" 40}
{{&viewers}/includes/reopenBrowse.i "user" 41}
{{&viewers}/includes/reopenBrowse.i "user" 42}
{{&viewers}/includes/reopenBrowse.i "user" 43}
{{&viewers}/includes/reopenBrowse.i "user" 44}
{{&viewers}/includes/reopenBrowse.i "user" 45}
{{&viewers}/includes/reopenBrowse.i "user" 46}
{{&viewers}/includes/reopenBrowse.i "user" 47}
{{&viewers}/includes/reopenBrowse.i "user" 48}
{{&viewers}/includes/reopenBrowse.i "user" 49}
{{&viewers}/includes/reopenBrowse.i "user" 50}
{{&viewers}/includes/reopenBrowse.i "user" 51}
{{&viewers}/includes/reopenBrowse.i "user" 52}
{{&viewers}/includes/reopenBrowse.i "user" 53}
{{&viewers}/includes/reopenBrowse.i "user" 54}
{{&viewers}/includes/reopenBrowse.i "user" 55}
{{&viewers}/includes/reopenBrowse.i "user" 56}
{{&viewers}/includes/reopenBrowse.i "user" 57}
{{&viewers}/includes/reopenBrowse.i "user" 58}
{{&viewers}/includes/reopenBrowse.i "user" 59}
{{&viewers}/includes/reopenBrowse.i "user" 60}
{{&viewers}/includes/reopenBrowse.i "user" 61}
{{&viewers}/includes/reopenBrowse.i "user" 62}
{{&viewers}/includes/reopenBrowse.i "user" 63}
{{&viewers}/includes/reopenBrowse.i "user" 64}
{{&viewers}/includes/reopenBrowse.i "user" 65}
{{&viewers}/includes/reopenBrowse.i "user" 66}
{{&viewers}/includes/reopenBrowse.i "user" 67}
{{&viewers}/includes/reopenBrowse.i "user" 68}
{{&viewers}/includes/reopenBrowse.i "user" 69}
{{&viewers}/includes/reopenBrowse.i "user" 70}
{{&viewers}/includes/reopenBrowse.i "user" 71}
{{&viewers}/includes/reopenBrowse.i "user" 72}
{{&viewers}/includes/reopenBrowse.i "user" 73}
{{&viewers}/includes/reopenBrowse.i "user" 74}
{{&viewers}/includes/reopenBrowse.i "user" 75}
{{&viewers}/includes/reopenBrowse.i "user" 76}
{{&viewers}/includes/reopenBrowse.i "user" 77}
{{&viewers}/includes/reopenBrowse.i "user" 78}
{{&viewers}/includes/reopenBrowse.i "user" 79}
{{&viewers}/includes/reopenBrowse.i "user" 80}
{{&viewers}/includes/reopenBrowse.i "user" 81}
{{&viewers}/includes/reopenBrowse.i "user" 82}
{{&viewers}/includes/reopenBrowse.i "user" 83}
{{&viewers}/includes/reopenBrowse.i "user" 84}
{{&viewers}/includes/reopenBrowse.i "user" 85}
{{&viewers}/includes/reopenBrowse.i "user" 86}
{{&viewers}/includes/reopenBrowse.i "user" 87}
{{&viewers}/includes/reopenBrowse.i "user" 88}
{{&viewers}/includes/reopenBrowse.i "user" 89}
{{&viewers}/includes/reopenBrowse.i "user" 90}
{{&viewers}/includes/reopenBrowse.i "user" 91}
{{&viewers}/includes/reopenBrowse.i "user" 92}
{{&viewers}/includes/reopenBrowse.i "user" 93}
{{&viewers}/includes/reopenBrowse.i "user" 94}
{{&viewers}/includes/reopenBrowse.i "user" 95}
{{&viewers}/includes/reopenBrowse.i "user" 96}
{{&viewers}/includes/reopenBrowse.i "user" 97}
{{&viewers}/includes/reopenBrowse.i "user" 98}

&UNDEFINE startDatePhrase
&UNDEFINE dateRangePhrase
&UNDEFINE sorting

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSize Include 
PROCEDURE setSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHeight AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS DECIMAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN lockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  ASSIGN
    browseJob:HIDDEN IN FRAME {&FRAME-NAME} = YES
    &IF DEFINED(dynColumns) EQ 0 &THEN
    RECT-1:HIDDEN = YES
    &ENDIF
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = ipHeight
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = ipWidth
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = ipHeight
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = ipWidth
    &IF DEFINED(dynColumns) EQ 0 &THEN
    RECT-1:WIDTH-PIXELS IN FRAME {&FRAME-NAME} = ipWidth - 2
    &ENDIF
    browseJob:HEIGHT-PIXELS = ipHeight - 28
    browseJob:WIDTH-PIXELS = ipWidth - 2
    &IF DEFINED(detailButton) NE 0 &THEN
    - 30
    &ENDIF
    &IF '{&browseName}' EQ 'pendingJob' &THEN
    - 123
    &ENDIF
    &IF DEFINED(dynColumns) EQ 0 &THEN
    RECT-1:HIDDEN = NO
    &ENDIF
    browseJob:HIDDEN IN FRAME {&FRAME-NAME} = NO.
  RUN lockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkDowntimeConflict Include 
FUNCTION checkDowntimeConflict RETURNS LOGICAL
  (ipResource AS CHARACTER,ipStartDateTime AS DECIMAL,ipEndDateTime AS DECIMAL) :
  {{&includes}/{&Board}/checkDowntimeConflict.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkJobConflict Include 
FUNCTION checkJobConflict RETURNS LOGICAL
  (ipResource AS CHARACTER,ipStartDateTime AS DECIMAL,
   ipEndDateTime AS DECIMAL,ipRowID AS ROWID) :
  {{&includes}/{&Board}/checkJobConflict.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION invalidMove Include 
FUNCTION invalidMove RETURNS LOGICAL
  (ipJob AS CHARACTER,ipResourceSequence AS INTEGER,
   ipStartDateTime AS DECIMAL,ipRowID AS ROWID) :
  {{&includes}/{&Board}/invalidMove.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime Include 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
  {{&includes}/numericDateTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION timeSpan Include 
FUNCTION timeSpan RETURNS INTEGER
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER) :
  {{&includes}/timeSpan.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

