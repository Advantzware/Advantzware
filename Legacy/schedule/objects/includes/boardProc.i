&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : boardProc.i 
    Purpose     : contains Scheduler Pro routines

    Syntax      : {{&includes}/Pro/boardProc.i}

    Description : 

    Author(s)   : Ron Stark
    Created     : 5.5.2004
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcPriority Include 
FUNCTION calcPriority RETURNS INTEGER
  (ipPriority AS INTEGER,ipResource AS CHARACTER,
   ipJobSequence AS INTEGER,ipResourceSequence AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resourcePriority Include 
FUNCTION resourcePriority RETURNS INTEGER
  (ipResource AS CHARACTER)  FORWARD.

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
         HEIGHT             = 35.33
         WIDTH              = 59.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

{{&includes}/addTime.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adjustSequence Include 
PROCEDURE adjustSequence :
{{&includes}/{&Board}/adjustSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDowntime Include 
PROCEDURE afterDowntime :
{{&includes}/{&Board}/afterDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeDowntime Include 
PROCEDURE beforeDowntime :
{{&includes}/{&Board}/beforeDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bringForward Include 
PROCEDURE bringForward :
/*------------------------------------------------------------------------------
  Purpose:     move all past due jobs forward to desired date
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvForwardDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvForwardTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE thisDate AS LOGICAL NO-UNDO.

  IF boardDate NE TODAY THEN
  MESSAGE 'Board Date:' boardDate 'is not Today!' SKIP(1)
    'Bring Past Jobs Forward to Board Date?' SKIP
    '(Note: NO will bring jobs forward to Today''s Date)' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO-CANCEL TITLE 'Bring Forward for Resource: ' + ipResource
    UPDATE thisDate.
  CASE thisDate:
    WHEN YES THEN
    ASSIGN
      lvForwardDate = boardDate
      lvForwardTime = intSTime[1].
    WHEN NO THEN
    ASSIGN
      lvForwardDate = TODAY
      lvForwardTime = TIME.
    OTHERWISE
    RETURN.
  END CASE.
  IF threeD THEN
  DO:
    MESSAGE '3D Appearance is disabled for this operation!' VIEW-AS ALERT-BOX.
    ASSIGN
      threeDBottom = NO
      threeDLeft = NO
      threeDRight = NO
      threeDTop = NO.
  END.
  MESSAGE 'Bring Forward Maintaining Sequence?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO-CANCEL TITLE 'Bring Forward for Resource: ' + ipResource
    UPDATE adjustBoard AS LOGICAL.
  IF adjustBoard EQ ? THEN RETURN.
  RUN setScreenStatus.
  RUN msgFrame ('Bring Past Jobs Forward').
  ASSIGN
    lvStartDate = lvForwardDate
    lvStartTime = lvForwardTime
    lvStartDateTime = numericDateTime(lvForwardDate,lvForwardTime).
  FOR EACH ttblJob EXCLUSIVE-LOCK WHERE (ttblJob.resource EQ ipResource
                                     OR ipResource EQ '<ALL>')
                                    AND ttblJob.startDateTime LE lvStartDateTime
                                    AND ttblJob.jobLocked EQ NO
                                    AND ttblJob.jobCompleted EQ NO
      BY ttblJob.job BY ttblJob.resourceSequence:
    FIND LAST buffJob NO-LOCK
         WHERE buffJob.job EQ ttblJob.job
           AND buffJob.resourceSequence LT ttblJob.resourceSequence NO-ERROR.
    ASSIGN
      lvStartDate = IF AVAILABLE buffJob THEN buffJob.endDate ELSE lvForwardDate
      lvStartTime = IF AVAILABLE buffJob THEN buffJob.endTime ELSE lvForwardTime
      .
    RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
    ASSIGN
      ttblJob.startDate = lvStartDate
      ttblJob.startTime = lvStartTime
      ttblJob.endDate = lvEndDate
      ttblJob.endTime = lvEndTime
      ttblJob.downtimeSpan = lvDowntimeSpan
      .
    ttblJob.startDateTime = numericDateTime(lvStartDate,lvStartTime).
    ttblJob.endDateTime = numericDateTime(lvEndDate,lvEndTime).
    ASSIGN
      ttblJob.jobBGColor = jobBGColor()
      ttblJob.jobFGColor = jobFGColor()
      ttblJob.statusLabel = jobStatus()
      .
    /*
    RUN jobMoveHistory (ROWID(ttblJob),lvStartDate,lvStartTime,
                        lvEndDate,lvEndTime,ttblJob.jobLocked,
                        ttblJob.downtimeSpan). */
  END.
  IF adjustBoard THEN
  DO:
    RUN setJobPriority.
    RUN setJobDateTime (ROWID(ttblJob),NO).
  END.
  RUN positionBoard (?,lvStartDate,YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildBoardDowntime Include 
PROCEDURE buildBoardDowntime :
{{&includes}/{&Board}/buildBoardDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildDowntime Include 
PROCEDURE buildDowntime :
{{&includes}/{&Board}/buildDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createDowntime Include 
PROCEDURE createDowntime :
{{&includes}/{&Board}/createDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLightBulb Include 
PROCEDURE createLightBulb :
{{&includes}/{&Board}/createLightBulb.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLock Include 
PROCEDURE createLock :
/*------------------------------------------------------------------------------
  Purpose:     create lock image button for a job bar and triggers
  Parameters:  object number assigned, X, Y, job rowid & if job is locked
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipY AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipLocked AS LOGICAL NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  IF ipX GE FRAME {&FRAME-NAME}:WIDTH-PIXELS - 14 THEN RETURN.
  
  {{&includes}/ttblWidgetFind.i "lockWidget" ipIdx}
  {{&includes}/ttblWidgetAssign.i "lockWidget" pWidget}
  ELSE
  DO:
    CREATE BUTTON pWidget IN WIDGET-POOL 'lockPool'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          SENSITIVE = YES
          HEIGHT-PIXELS = 14
    TRIGGERS:
      ON CHOOSE
         PERSISTENT RUN toggleLock IN THIS-PROCEDURE (pWidget:HANDLE).
    END TRIGGERS.
    {{&includes}/ttblWidgetCreate.i "lockWidget" ipIdx pWidget}
  END.
  ASSIGN
    pWidget:HIDDEN = YES
    pWidget:WIDTH-PIXELS = 14
    pWidget:X = ipX + 1
    pWidget:Y = ipY + 1
    pWidget:PRIVATE-DATA = STRING(ipRowID)
    ldummy = pWidget:LOAD-IMAGE(IF AVAIL(ttblJob) AND ipLocked THEN
             '{&images}/locked.gif' ELSE ?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createNote Include 
PROCEDURE createNote :
/*------------------------------------------------------------------------------
  Purpose:     create note image button for a job bar and triggers
  Parameters:  object number assigned, X, Y, job rowid & if job note exists
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/createNote.i} /* noteIcon */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE detailJob Include 
PROCEDURE detailJob :
{{&includes}/{&Board}/detailJob.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE detailResource Include 
PROCEDURE detailResource :
/*------------------------------------------------------------------------------
  Purpose:     run the resource detail window persistent
  Parameters:  resource and resource description
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDescription AS CHARACTER NO-UNDO.
  
  IF /* NOT detailWindow AND */ ipResource NE currentResource THEN
  RETURN.
  /* IF NOT proOpts[4] THEN
  DO:
    MESSAGE proOptsMsg(4) VIEW-AS ALERT-BOX.
    RETURN.
  END. */
  IF NOT VALID-HANDLE(resourceHandle) THEN
  DO:
    RUN VALUE(findProgram('{&objects}','','/detailResource.w')) PERSISTENT SET resourceHandle.
    RUN adm-initialize IN resourceHandle.
  END.
  RUN setPopup IN containerHandle (5,resourceHandle).
  RUN resource IN resourceHandle (ipResource,ipDescription,THIS-PROCEDURE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE downtimeSpan Include 
PROCEDURE downtimeSpan :
{{&includes}/{&Board}/downtimeSpan.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dueDateAvailable Include 
PROCEDURE dueDateAvailable :
{{&includes}/{&Board}/dueDateAvailable.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findResource Include 
PROCEDURE findResource :
/*------------------------------------------------------------------------------
  Purpose:     job moved over resource object, figure out which one
  Parameters:  job bar object handle, output if they cancel move
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE OUTPUT PARAMETER opCancel AS LOGICAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE resourceStartX AS INTEGER NO-UNDO.
  DEFINE VARIABLE resourceEndX AS INTEGER NO-UNDO.
  DEFINE VARIABLE resourceStartY AS INTEGER NO-UNDO.
  DEFINE VARIABLE resourceEndY AS INTEGER NO-UNDO.
  DEFINE VARIABLE jobStartX AS INTEGER NO-UNDO.
  DEFINE VARIABLE jobEndX AS INTEGER NO-UNDO.
  DEFINE VARIABLE jobStartY AS INTEGER NO-UNDO.
  DEFINE VARIABLE jobEndY AS INTEGER NO-UNDO.
  DEFINE VARIABLE choice AS CHARACTER NO-UNDO.
  
  IF NOT changeResource AND NOT copyToResource THEN
  RETURN.
  ASSIGN
    jobStartX = ipWidget:X
    jobEndX = ipWidget:X + ipWidget:WIDTH-PIXELS
    jobStartY = ipWidget:Y
    jobEndY = ipWidget:Y + ipWidget:HEIGHT-PIXELS.
  DO i = 1 TO resourceIdx:
    ASSIGN
      resourceStartX = resourceWidget[i]:X
      resourceStartX = 0
      resourceEndX = resourceWidget[i]:X + resourceWidget[i]:WIDTH-PIXELS
      resourceStartY = resourceWidget[i]:Y
      resourceEndY = resourceWidget[i]:Y + resourceWidget[i]:HEIGHT-PIXELS.
    IF ((jobStartX GE resourceStartX AND jobStartX LE resourceEndX) OR
        (jobEndX GE resourceStartX AND jobEndX LE resourceEndX)) AND
       ((jobStartY GE resourceStartY AND jobStartY LE resourceEndY) OR
        (jobEndY GE resourceStartY AND jobEndY LE resourceEndY)) AND
       ttblJob.resource NE resourceWidget[i]:NAME THEN
    DO:
      RUN {&prompts}/newResource.w (ipWidget:NAME,resourceWidget[i]:NAME,
                                    changeResource,copyToResource,OUTPUT choice).
      IF choice EQ 'Cancel' THEN
      opCancel = YES.
      ELSE
      ASSIGN
        newResourceWidget = resourceWidget[i]:HANDLE
        copy2Resource = choice EQ 'Copy'
        ipWidget:Y = resourceWidget[i]:Y
        ipWidget:X = startX.
      RETURN.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE firstAvailable Include 
PROCEDURE firstAvailable :
{{&includes}/{&Board}/firstAvailable.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCapacity Include 
PROCEDURE getCapacity :
/*------------------------------------------------------------------------------
  Purpose:     load capacity values from data files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN msgFrame ('Load Capacity Times').
  FOR EACH ttblDowntime EXCLUSIVE-LOCK WHERE ttblDowntime.dayID EQ 0:
    DELETE ttblDowntime.
  END.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/capacity.dat')) NO-ECHO.
  REPEAT:
    IMPORT tempDowntime.
    IF CAN-FIND(ttblDowntime WHERE ttblDowntime.resource EQ tempDowntime.resource
                               AND ttblDowntime.startDate EQ tempDowntime.startDate
                               AND ttblDowntime.startTime EQ tempDowntime.startTime
                               AND ttblDowntime.endTime EQ tempDowntime.endTime) THEN
    NEXT.
    CREATE ttblDowntime.
    BUFFER-COPY tempDowntime TO ttblDowntime.
    ttblDowntime.startDateTime = numericDateTime(ttblDowntime.startDate,ttblDowntime.startTime).
    ttblDowntime.endDateTime = numericDateTime(ttblDowntime.startDate,ttblDowntime.endTime).
  END.
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDowntime Include 
PROCEDURE getDowntime :
/*------------------------------------------------------------------------------
  Purpose:     load downtime values from data files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/getDowntime.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getJobNotes Include 
PROCEDURE getJobNotes :
/*------------------------------------------------------------------------------
  Purpose:     load job notes from data files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/getJobNotes.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPriorJobResource Include 
PROCEDURE getPriorJobResource :
{{&includes}/{&Board}/getPriorJobResource.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPriorJobSequence Include 
PROCEDURE getPriorJobSequence :
{{&includes}/{&Board}/getPriorJobSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideDowntime Include 
PROCEDURE hideDowntime :
/*------------------------------------------------------------------------------
  Purpose:     instead of deleting widgets, simply hide unused objects
  Parameters:  last used object count
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/hideDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideLock Include 
PROCEDURE hideLock :
/*------------------------------------------------------------------------------
  Purpose:     instead of deleting widgets, simply hide unused objects
  Parameters:  last used object count
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  {{&includes}/ttblWidgetHide.i "lockWidget" ipIdx}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideNote Include 
PROCEDURE hideNote :
/*------------------------------------------------------------------------------
  Purpose:     instead of deleting widgets, simply hide unused objects
  Parameters:  last used object count
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  {{&includes}/ttblWidgetHide.i "noteWidget" ipIdx} /* noteIcon */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobClick Include 
PROCEDURE jobClick :
/*------------------------------------------------------------------------------
  Purpose:     execute from click job bar object
  Parameters:  job bar object handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  RUN jobStartMove (ipWidget).
  RUN jobEndMove (ipWidget).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobDeselection Include 
PROCEDURE jobDeselection :
/*------------------------------------------------------------------------------
  Purpose:     selected job bar no longer selected
  Parameters:  job bar object handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  RUN jobReset (ipWidget).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobDowntimeSpan Include 
PROCEDURE jobDowntimeSpan :
{{&includes}/{&Board}/jobDowntimeSpan.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobEndMove Include 
PROCEDURE jobEndMove :
/*------------------------------------------------------------------------------
  Purpose:     end of drop and drag move
  Parameters:  job bar object handle
  Notes:       prompts to verify move date & time, sets date & time, creates a
               history record of move to allow undo & redo functions
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE checkResource AS CHARACTER NO-UNDO.
  DEFINE VARIABLE conflictChoice AS CHARACTER NO-UNDO.
  DEFINE VARIABLE continue AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvLagTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvTimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE moveOK AS LOGICAL NO-UNDO.
  DEFINE VARIABLE opCancel AS LOGICAL NO-UNDO.
  DEFINE VARIABLE xpixel AS INTEGER NO-UNDO.
  
  /* IF NOT proOpts[1] OR endMove THEN DO:
    RUN jobReset (ipWidget).
    RETURN.
  END. */
  ASSIGN
    moveOK = NOT boardDatePrompt
    jobMoving = NO.
  RUN hoganPopup IN containerHandle (NO).
  IF endMove THEN RETURN.
  DO WITH FRAME {&FRAME-NAME}:
    IF ipWidget:X LT resourceGrid:WIDTH-PIXELS AND
      (changeResource OR copyToResource) THEN
    RUN findResource (ipWidget:HANDLE,OUTPUT opCancel).
    IF opCancel THEN DO:
      RUN jobReset (ipWidget).
      RETURN.
    END.
    IF ipWidget:X GE btnPending:X AND
       ipWidget:X LE btnPending:X + btnPending:WIDTH-PIXELS + btnPendingJobs:X AND
       ipWidget:Y GE btnPending:Y AND
       ipWidget:Y LE btnPending:Y + btnPending:HEIGHT-PIXELS THEN DO:
      FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(ipWidget:PRIVATE-DATA).
      IF ttblJob.jobLocked THEN DO:
        MESSAGE 'Cannot Return Locked Job to Pending' VIEW-AS ALERT-BOX.
        RUN jobReset (ipWidget).
        RETURN.
      END.
      RUN pendingReturn (ttblJob.job,OUTPUT moveOK).
      jobMovingDisplay:HIDDEN = YES.
      IF NOT moveOK THEN
      RUN jobReset (ipWidget).
      RETURN.
    END.
    IF newResourceWidget EQ ? THEN
    ipWidget:Y = startY.
    ipWidget:HEIGHT-PIXELS = startHeight.
    IF NOT resizeJob THEN
    ipWidget:WIDTH-PIXELS = 2.
    IF ipWidget:X LT resourceGrid:WIDTH-PIXELS THEN
    ipWidget:X = resourceGrid:WIDTH-PIXELS + 1.
    FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(ipWidget:PRIVATE-DATA).
    ASSIGN
      endMove = NO
      xpixel = ipWidget:X - resourceGrid:WIDTH-PIXELS.
    IF xpixel GT maxpixel THEN xpixel = maxpixel.
    ASSIGN
      lvStartDate = pixelDate(dateTimePixel[xpixel])
      lvStartTime = pixelTime(dateTimePixel[xpixel])
      lvEndDate = ttblJob.endDate
      lvEndTime = ttblJob.endTime
      lvLagTime = ttblJob.lagTime.
    IF resizeJob THEN
    ASSIGN
      xpixel = xpixel + 1 + ipWidget:WIDTH-PIXELS
      lvEndDate = pixelDate(dateTimePixel[xpixel])
      lvEndTime = pixelTime(dateTimePixel[xpixel]).
    lvTimeSpan = IF resizeJob THEN timeSpan(lvStartDate,lvStartTime,lvEndDate,lvEndTime)
                 ELSE ttblJob.timeSpan.
  END.
  IF boardDatePrompt THEN
  RUN {&prompts}/move.w (ROWID(ttblJob),resizeJob,
                         INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                         INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime,
                         INPUT-OUTPUT lvLagTime,OUTPUT moveOK).
  ELSE
  RUN newEnd (lvTimeSpan,lvStartDate,lvStartTime,
              OUTPUT lvEndDate,OUTPUT lvEndTime).
  IF moveOK THEN DO:
    IF resizeJob THEN
    ASSIGN
      ttblJob.timeSpan = timeSpan(lvStartDate,lvStartTime,lvEndDate,lvEndTime)
      ttblJob.downtimeSpan = 0.
    RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
    ASSIGN
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
      checkResource = IF newResourceWidget EQ ? THEN ttblJob.resource
                      ELSE newResourceWidget:NAME.
    {{&includes}/{&Board}/jobEndMove.i ipWidget boardDatePrompt}
    /*
    RUN jobMoveHistory (ROWID(ttblJob),lvStartDate,lvStartTime,
                        lvEndDate,lvEndTime,ttblJob.jobLocked,lvDowntimeSpan).
    */
    IF newResourceWidget NE ? THEN
    RUN newResource (ipWidget).
    RUN unknownResource (lvEndDateTime).
    RUN setJobSequence.
  END.
  ELSE
  RUN jobReset (ipWidget).
  ASSIGN
    jobMovingDisplay:HIDDEN IN FRAME {&FRAME-NAME} = YES
    ldummy = jobMovingDisplay:MOVE-TO-BOTTOM().
  APPLY 'VALUE-CHANGED' TO intervals IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobEndResize Include 
PROCEDURE jobEndResize :
/*------------------------------------------------------------------------------
  Purpose:     keep height value the same incase it's changed
  Parameters:  job bar object handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE tempWidth AS INTEGER NO-UNDO.

  ASSIGN
    ipWidget:Y = startY
    ipWidget:HEIGHT-PIXELS = hpixels.
  /* end date can not change, but they dragged the end date portion, so move
     the starting point porportional to the end point */
  IF NOT endDateMove AND
     ipWidget:WIDTH-PIXELS NE startWidth AND
     ipWidget:X EQ startX THEN
  ASSIGN
    tempWidth = ipWidget:WIDTH-PIXELS
    ipWidget:WIDTH-PIXELS = startWidth
    ipWidget:X = ipWidget:X + tempWidth - startWidth.
  IF ipWidget:X NE startX OR ipWidget:WIDTH-PIXELS NE startWidth THEN
  RUN jobEndMove (ipWidget).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobMouseDown Include 
PROCEDURE jobMouseDown :
/*------------------------------------------------------------------------------
  Purpose:     executed from job widget trigger of mouse down action
  Parameters:  job object widget handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  RUN mousePosition.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobMoveHistory Include 
PROCEDURE jobMoveHistory :
/*------------------------------------------------------------------------------
  Purpose:     create move history record for undo & redo functions
  Parameters:  job rowid, start date & time, end date & time
  Notes:       sets undo & redo buttons accordingly
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipStartTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipEndDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipEndTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobLocked AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipDowntimeSpan AS INTEGER NO-UNDO.

  FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ ipRowID NO-ERROR.
  IF NOT AVAILABLE ttblJob THEN RETURN.
  FOR EACH ttblJob-do EXCLUSIVE-LOCK WHERE ttblJob-do.order GT currentOrder:
    DELETE ttblJob-do.
  END.
  CREATE ttblJob-do.
  ASSIGN
    currentOrder = currentOrder + 1
    ttblJob-do.order = currentOrder
    ttblJob-do.ttblJobRowID = ROWID(ttblJob)
    ttblJob-do.resource[1] = ttblJob.resource
    ttblJob-do.resourceSequence[1] = ttblJob.resourceSequence
    ttblJob-do.resourceDescription[1] = ttblJob.resourceDescription
    ttblJob-do.startDate[1] = ttblJob.startDate
    ttblJob-do.startTime[1] = ttblJob.startTime
    ttblJob-do.endDate[1] = ttblJob.endDate
    ttblJob-do.endTime[1] = ttblJob.endTime
    ttblJob-do.jobLocked[1] = ttblJob.jobLocked
    ttblJob-do.resource[2] = IF newResourceWidget EQ ? THEN ttblJob.resource
                             ELSE newResourceWidget:NAME
    ttblJob-do.resourceSequence[2] = IF newResourceWidget EQ ? THEN ttblJob.resourceSequence
                                     ELSE 0
    ttblJob-do.resourceDescription[2] = IF newResourceWidget EQ ? THEN ttblJob.resourceDescription
                                        ELSE newResourceWidget:TOOLTIP
    ttblJob-do.startDate[2] = ipStartDate
    ttblJob-do.startTime[2] = ipStartTime
    ttblJob-do.endDate[2] = ipEndDate
    ttblJob-do.endTime[2] = ipEndTime
    ttblJob-do.jobLocked[2] = ipJobLocked
    ttblJob-do.copyOf = copy2Resource
    ttblJob.startDate = ipStartDate
    ttblJob.startTime = ipStartTime
    ttblJob.endDate = ipEndDate
    ttblJob.endTime = ipEndTime
    ttblJob.jobLocked = ipJobLocked
    ttblJob.downtimeSpan = ipDowntimeSpan
    schdChanged = YES
    .
  ttblJob.startDateTime = numericDateTime(ipStartDate,ipStartTime).
  ttblJob.endDateTime = numericDateTime(ipEndDate,ipEndTime).
  ASSIGN
    ttblJob.jobBGColor = jobBGColor()
    ttblJob.jobFGColor = jobFGColor()
    ttblJob.statusLabel = jobStatus()
    .
  FIND CURRENT ttblJob NO-LOCK.
  /*
  ENABLE btnUndo WITH FRAME {&FRAME-NAME}.
  DISABLE btnRedo WITH FRAME {&FRAME-NAME}.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobNote Include 
PROCEDURE jobNote :
/*------------------------------------------------------------------------------
  Purpose:     access job notes via job object on board
  Parameters:  lock buttom image object handle
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/jobNote.i} /* noteIcon */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobReset Include 
PROCEDURE jobReset :
/*------------------------------------------------------------------------------
  Purpose:     move job bar back to it's original position
  Parameters:  job bar object handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  ASSIGN
    ipWidget:HIDDEN = YES
    ipWidget:HEIGHT-PIXELS = startHeight
    ipWidget:X = startX
    ipWidget:Y = startY
    ipWidget:WIDTH-PIXELS = startWidth
    ipWidget:HIDDEN = NO
    resizeJob = NO
    newResourceWidget = ?
    copy2Resource = NO
    jobMoving = NO
    jobMovingDisplay:HIDDEN IN FRAME {&FRAME-NAME} = YES
    ldummy = jobMovingDisplay:MOVE-TO-BOTTOM()
    .
  {{&includes}/{&Board}/jobDeselection.i}
  IF NOT AVAILABLE ttblJob THEN
  FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(ipWidget:PRIVATE-DATA).
  ASSIGN
    ipWidget:BGCOLOR = jobBGColor()
    ipWidget:FGCOLOR = jobFGColor()
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobSelection Include 
PROCEDURE jobSelection :
/*------------------------------------------------------------------------------
  Purpose:     set color if job bar is selected
  Parameters:  job bar object handle
  Notes:       calls detail window for selected job
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

/*  MESSAGE ipWidget:TOOLTIP VIEW-AS ALERT-BOX.*/
  
  RUN gridLine (ipWidget).
  IF endMove THEN
  DO:
    endMove = NO.
    RETURN.
  END.
  ASSIGN
    currentJob = ipWidget:HANDLE
    startHeight = ipWidget:HEIGHT-PIXELS
    startWidth = ipWidget:WIDTH-PIXELS
    startX = ipWidget:X
    startY = ipWidget:Y.
  {{&includes}/{&Board}/jobSelection.i}
  RUN detailJob (ROWID(ttblJob),ttblJob.rowIDs).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobStacker Include 
PROCEDURE jobStacker :
/*------------------------------------------------------------------------------
  Purpose:     get the next open slot or leave job as is if it's ok
  Parameters:  lots of stuff
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipResourceFirstDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipResourceFirstTime AS INTEGER NO-UNDO.
  
  DEFINE INPUT-OUTPUT PARAMETER iopJobMoved AS LOGICAL NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopStartDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopStartTime AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStackDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStackTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStackDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvResourceDateTime AS DECIMAL NO-UNDO.
  
  ASSIGN
    lvStartDate = iopStartDate
    lvStartTime = iopStartTime
    lvEndDate = iopEndDate
    lvEndTime = iopEndTime
    lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
    lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
    lvResourceDateTime = numericDateTime(ipResourceFirstDate,ipResourceFirstTime)
    lvDateTime = lvStartDateTime.
  IF ipType EQ 'Job' THEN
  DO:
    FIND LAST jobStacker NO-LOCK
         WHERE jobStacker.job EQ ipJob
           AND jobStacker.endDateTime GT lvResourceDateTime NO-ERROR.
    IF AVAILABLE jobStacker THEN DO:
      IF cascadeJob THEN DO:
        ASSIGN
          lvStartDate = jobStacker.endDate
          lvStartTime = jobStacker.endTime.
        IF jobStacker.lagTime NE 0 THEN
        RUN addTime (jobStacker.startDate,jobStacker.startTime,jobStacker.lagTime,
                     OUTPUT lvStartDate,OUTPUT lvStartTime).
      END. /* if cascadejob */
      ELSE
      ASSIGN
        lvStartDate = jobStacker.startDate
        lvStartTime = jobStacker.startTime.
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
    END. /* avail jobstacker */
    IF useSequence THEN
    DO:
      FIND LAST buffStack NO-LOCK
           WHERE buffStack.resource EQ ipResource
             AND buffStack.jobSequence LT ipJobSequence
             AND buffStack.endDateTime GT lvResourceDateTime NO-ERROR.
      IF AVAILABLE buffStack THEN
      DO:
        ASSIGN
          lvStackDate = IF cascadeJob THEN buffStack.endDate ELSE buffStack.startDate
          lvStackTime = IF cascadeJob THEN buffStack.endTime ELSE buffStack.startTime
          lvStackDateTime = numericDateTime(lvStackDate,lvStackTime).
        IF lvStackDateTime GT lvStartDateTime THEN
        ASSIGN
          lvStartDate = lvStackDate
          lvStartTime = lvStackTime
          lvStartDateTime = lvStackDateTime.
      END. /* avail buffstack */
    END. /* if usesequence */
    {{&includes}/{&Board}/findBoardDowntime.i}
    RUN downtimeSpan (ipResource,ipTimeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
  END. /* if iptype eq Job */
  ELSE
  IF ipType EQ 'Resource' THEN
  DO:
    IF NOT {{&includes}/{&Board}/conflictFunc.i jobStacker ipResource lv} THEN
    RETURN.
    FOR EACH jobStacker NO-LOCK
        WHERE jobStacker.resource EQ ipResource
          AND jobStacker.endDateTime GE lvDateTime:
      ASSIGN
        lvStartDate = jobStacker.endDate
        lvStartTime = jobStacker.endTime
        lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
      {{&includes}/{&Board}/findBoardDowntime.i}
      RUN downtimeSpan (ipResource,ipTimeSpan,lvStartDate,lvStartTime,
                        OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
      ASSIGN
        lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
        lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
      IF NOT {{&includes}/{&Board}/conflictFunc.i jobStacker ipResource lv} THEN
      LEAVE.
    END. /* each jobStacker */
  END. /* iptype eq resource */
  IF iopStartDate NE lvStartDate OR iopStartTime NE lvStartTime OR
     iopEndDate NE lvEndDate OR iopEndTime NE lvEndTime THEN
  ASSIGN
    iopStartDate = lvStartDate
    iopStartTime = lvStartTime
    iopEndDate = lvEndDate
    iopEndTime = lvEndTime
    iopDowntimeSpan = lvDowntimeSpan
    iopJobMoved = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobStackerCreate Include 
PROCEDURE jobStackerCreate :
{{&includes}/{&Board}/jobStackerCreate.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobStartMove Include 
PROCEDURE jobStartMove :
/*------------------------------------------------------------------------------
  Purpose:     grab starting and ending coords before beginning move
  Parameters:  job bar object handle
  Notes:       checks if job is locked to prevent moving
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  RUN jobSelection (ipWidget).
  /* IF NOT proOpts[1] THEN
  DO:
    MESSAGE proOptsMsg(1) VIEW-AS ALERT-BOX.
    ASSIGN
      endMove = TRUE
      resizeJob = NO.
    RETURN.
  END. */
  FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(ipWidget:PRIVATE-DATA).
  IF ttblJob.jobLocked AND ttblJob.jobCompleted THEN
  DO:
    IF ttblJob.jobLocked THEN
    MESSAGE 'Job is Locked and can not be Moved' VIEW-AS ALERT-BOX WARNING.
    ELSE
    MESSAGE 'Job is Completed and can not be Moved' VIEW-AS ALERT-BOX WARNING.
    ASSIGN
      endMove = YES
      resizeJob = NO.
    RUN jobReset (ipWidget).
    RETURN.
  END.
  RUN hoganPopup IN containerHandle (YES).
  IF NOT resizeJob THEN
  ASSIGN
    jobMoving = YES
    ipWidget:WIDTH-PIXELS = 2
    ipWidget:X = cursorX
    jobMovingDisplay:HIDDEN IN FRAME {&FRAME-NAME} = NO
    ldummy = jobMovingDisplay:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobStartResize Include 
PROCEDURE jobStartResize :
/*------------------------------------------------------------------------------
  Purpose:     set logical indicating job is being resized
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  resizeJob = endDateMove.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadDowntime Include 
PROCEDURE loadDowntime :
{{&includes}/{&Board}/loadDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE menuItem Include 
PROCEDURE menuItem :
/*------------------------------------------------------------------------------
  Purpose:     menu item selected from popup attached to container
  Parameters:  name of menu item format m_<name>, <name> is object name in board.w
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMenuItem AS CHARACTER NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  ASSIGN
    pWidget = FRAME {&FRAME-NAME}:HANDLE
    pWidget = pWidget:FIRST-CHILD
    pWidget = pWidget:FIRST-CHILD.
  DO WHILE pWidget NE ?:
    IF pWidget:TYPE EQ 'BUTTON' AND pWidget:NAME EQ SUBSTR(ipMenuItem,3) THEN
    DO:
      APPLY 'CHOOSE' TO pWidget.
      RETURN.
    END.
    pWidget = pWidget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mousePosition Include 
PROCEDURE mousePosition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cursorPos AS MEMPTR NO-UNDO.

  SET-SIZE(cursorPos) = 16.
  RUN GetCursorPos(INPUT-OUTPUT cursorPos).
  RUN ScreenToClient(FRAME {&FRAME-NAME}:HWND,cursorPos).
  ASSIGN
    cursorX = GET-LONG(cursorPos,1)
    cursorY = GET-LONG(cursorPos,5).
  SET-SIZE(cursorPos) = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveExisting Include 
PROCEDURE moveExisting :
&SCOPED-DEFINE endDateTime numericDateTime(intDate[24],intETime[24])
{{&includes}/{&Board}/moveExisting.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd Include 
PROCEDURE newEnd :
{{&includes}/{&Board}/newEnd.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newJobSequence Include 
PROCEDURE newJobSequence :
{{&includes}/{&Board}/newJobSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newResource Include 
PROCEDURE newResource :
/*------------------------------------------------------------------------------
  Purpose:     change resource information for job after change or copy action
  Parameters:  job bar object handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  IF copy2Resource THEN DO:
    CREATE buffJob.
    BUFFER-COPY ttblJob TO buffJob.
  END.
  FIND FIRST ttblResource
       WHERE ttblResource.resource EQ newResourceWidget:NAME NO-ERROR.
  FIND CURRENT ttblJob EXCLUSIVE-LOCK.
  ASSIGN
    ttblJob.resource = newResourceWidget:NAME
    ttblJob.altResource = newResourceWidget:NAME
    ttblJob.resourceDescription = newResourceWidget:TOOLTIP
    ttblJob.department = IF AVAIL(ttblResource) THEN ttblResource.department ELSE ''
    /*ttblJob.resourceSequence = 0*/
    newResourceWidget = ?
    copy2Resource = NO.
  FIND CURRENT ttblJob NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newStart Include 
PROCEDURE newStart :
{{&includes}/{&Board}/newStart.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE objectName Include 
PROCEDURE objectName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipName AS CHARACTER NO-UNDO.

  RUN objectName IN containerHandle (ipType,ipName).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE packBoard Include 
PROCEDURE packBoard :
{{&includes}/{&Board}/packBoard.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE packJob Include 
PROCEDURE packJob :
{{&includes}/{&Board}/packJob.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE packResource Include 
PROCEDURE packResource :
{{&includes}/{&Board}/packResource.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pendingReturn Include 
PROCEDURE pendingReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opContinue AS LOGICAL NO-UNDO.

  RUN {&prompts}/pendingReturn.w (ipJob,OUTPUT opContinue).
  /*
  MESSAGE 'Return Job' ipJob 'to Pending?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE opContinue.
  */
  IF NOT opContinue THEN RETURN.
  /*
  FOR EACH ttblJob EXCLUSIVE-LOCK WHERE ttblJob.job EQ ipJob:
    CREATE pendingJob.
    BUFFER-COPY ttblJob TO pendingJob
      ASSIGN pendingJob.origStartTime = ttblJob.timeSpan.
    DELETE ttblJob.
  END. /* each ttbljob */
  */
  RUN setJobSequence.
  RUN buildBoard (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE positionBoard Include 
PROCEDURE positionBoard :
{{&includes}/{&Board}/positionBoard.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rightClick Include 
PROCEDURE rightClick :
/*------------------------------------------------------------------------------
  Purpose:     user did right mouse click, popup pending/ready dialog
  Parameters:  job bar object handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  MESSAGE 'right-mouse-click' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveBoard Include 
PROCEDURE saveBoard :
/*------------------------------------------------------------------------------
  Purpose:     called from container window when user closes board
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY 'CHOOSE' TO btnSave IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveScenario Include 
PROCEDURE saveScenario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipInitialDir AS CHARACTER NO-UNDO.

  RUN msgFrame ('Saving Board to ' + scenario).
  asOfTime = {{&includes}/asOfTime.i}.
  OUTPUT TO VALUE(SEARCH(ipInitialDir + '/' + scenario + '.dat')).
  EXPORT '{&version}' asOfTime.
  FOR EACH ttblJob NO-LOCK:
    EXPORT
      ttblJob.resource
      ttblJob.jobSequence
      ttblJob.job
      ttblJob.jobSort
      ttblJob.jobDescription
      ttblJob.resourceSequence
      ttblJob.origResSeq
      ttblJob.altResSeq
      ttblJob.resourceDescription
      ttblJob.altResource
      ttblJob.department
      ttblJob.startDate
      ttblJob.startTime
      ttblJob.endDate
      ttblJob.endTime
      ttblJob.timeSpan
      ttblJob.jobLocked
      ttblJob.dueDate
      ttblJob.dueTime
      ttblJob.prodDate
      ttblJob.userValue
      ttblJob.jobCompleted
      ttblJob.rowIDs
      ttblJob.keyValue
      ttblJob.udfField01
      ttblJob.udfField02
      ttblJob.udfField03
      ttblJob.udfField04
      ttblJob.udfField05
      ttblJob.udfField06
      ttblJob.udfField07
      ttblJob.udfField08
      ttblJob.udfField09
      ttblJob.udfField10
      ttblJob.udfField11
      ttblJob.udfField12
      ttblJob.udfField13
      ttblJob.udfField14
      ttblJob.udfField15
      ttblJob.udfField16
      ttblJob.udfField17
      ttblJob.udfField18
      ttblJob.udfField19
      ttblJob.udfField20
      ttblJob.userField01
      ttblJob.userField02
      ttblJob.userField03
      ttblJob.userField04
      ttblJob.userField05
      ttblJob.userField06
      ttblJob.userField07
      ttblJob.userField08
      ttblJob.userField09
      ttblJob.userField10
      ttblJob.userField11
      ttblJob.userField12
      ttblJob.userField13
      ttblJob.userField14
      ttblJob.userField15
      ttblJob.userField16
      ttblJob.userField17
      ttblJob.userField18
      ttblJob.userField19
      ttblJob.userField20
      ttblJob.userField21
      ttblJob.userField22
      ttblJob.userField23
      ttblJob.userField24
      ttblJob.userField25
      ttblJob.userField26
      ttblJob.userField27
      ttblJob.userField28
      ttblJob.userField29
      ttblJob.userField30
      ttblJob.userField31
      ttblJob.userField32
      ttblJob.userField33
      ttblJob.userField34
      ttblJob.userField35
      ttblJob.userField36
      ttblJob.userField37
      ttblJob.userField38
      ttblJob.userField39
      ttblJob.userField40
      ttblJob.userField41
      ttblJob.userField42
      ttblJob.userField43
      ttblJob.userField44
      ttblJob.userField45
      ttblJob.userField46
      ttblJob.userField47
      ttblJob.userField48
      ttblJob.userField49
      ttblJob.userField50
      ttblJob.userField51
      ttblJob.userField52
      ttblJob.userField53
      ttblJob.userField54
      ttblJob.userField55
      ttblJob.userField56
      ttblJob.userField57
      ttblJob.userField58
      ttblJob.userField59
      ttblJob.userField60
      ttblJob.userField61
      ttblJob.userField62
      ttblJob.userField63
      ttblJob.userField64
      ttblJob.userField65
      ttblJob.userField66
      ttblJob.userField67
      ttblJob.userField68
      ttblJob.userField69
      ttblJob.userField70
      ttblJob.userField71
      ttblJob.userField72
      ttblJob.userField73
      ttblJob.userField74
      ttblJob.userField75
      ttblJob.userField76
      ttblJob.userField77
      ttblJob.userField78
      ttblJob.userField79
      ttblJob.userField80
      ttblJob.userField81
      ttblJob.userField82
      ttblJob.userField83
      ttblJob.userField84
      ttblJob.userField85
      ttblJob.userField86
      ttblJob.userField87
      ttblJob.userField88
      ttblJob.userField89
      ttblJob.userField90
      ttblJob.userField91
      ttblJob.userField92
      ttblJob.userField93
      ttblJob.userField94
      ttblJob.userField95
      ttblJob.userField96
      ttblJob.userField97
      ttblJob.userField98
      ttblJob.jobStatus
      ttblJob.statusTimeStamp
      ttblJob.liveUpdate
      ttblJob.lagTime
      ttblJob.jobToolTip.
  END.
  OUTPUT CLOSE.
  justOpened = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobColor Include 
PROCEDURE setJobColor :
/*------------------------------------------------------------------------------
  Purpose:     set a job's background color 
  Parameters:  job rowid
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ ipRowID NO-ERROR.
  IF NOT AVAILABLE ttblJob THEN RETURN.
  ASSIGN
    ttblJob.jobBGColor = jobBGColor()
    ttblJob.jobFGColor = jobFGColor()
    ttblJob.statusLabel = jobStatus()
    .
  IF ttblJob.widgetIdx NE 0 THEN
  DO:
    {{&includes}/ttblWidgetFind.i "jobWidget" ttblJob.widgetIdx}
    jobWidget.jobBGColor = ttblJob.jobBGColor.
  END.
  IF completedHide THEN RUN buildBoard (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobDateTime Include 
PROCEDURE setJobDateTime :
{{&includes}/{&Board}/setJobDateTime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobPriority Include 
PROCEDURE setJobPriority :
{{&includes}/{&Board}/setJobPriority.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobSequence Include 
PROCEDURE setJobSequence :
{{&includes}/{&Board}/setJobSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setProOptions Include 
PROCEDURE setProOptions :
/*------------------------------------------------------------------------------
  Purpose:     set vars used to enable/disable functions between base and pro
               versions
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  j = proOptions.
  DO i = EXTENT(proOpts) TO 1 BY -1:
    proOpts[i] = NO.
    IF j GE EXP(2,i - 1) THEN
    ASSIGN
      j = j - EXP(2,i - 1)
      proOpts[i] = YES.
  END.
  
  IF proOpts[2] THEN
  ENABLE scenario WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE scenario WITH FRAME {&FRAME-NAME}.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSortSequence Include 
PROCEDURE setSortSequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.

  IF ttblJob.sortSequence NE 0 THEN RETURN.
  ASSIGN
    sortSeq = sortSeq + 1
    ttblJob.sortSequence = sortSeq.
  IF ENTRY(1,ipValue) EQ 'Resource' THEN
  ttblJob.anchored = NOT CAN-FIND(FIRST buffJob
                                  WHERE buffJob.resource EQ ENTRY(2,ipValue)
                                    AND buffJob.job EQ ttblJob.job).
  ELSE
  IF ENTRY(1,ipValue) EQ 'Job' AND ttblJob.job NE ENTRY(3,ipValue) THEN
  ttblJob.anchored = YES.
  ELSE
  IF ENTRY(6,ipValue) NE '' AND TO-ROWID(ENTRY(6,ipValue)) EQ ROWID(ttblJob) THEN
  ttblJob.anchored = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shiftJobs Include 
PROCEDURE shiftJobs :
&SCOPED-DEFINE endDateTime numericDateTime(intDate[24],intETime[24])
{{&includes}/{&Board}/shiftJobs.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shiftJobSequence Include 
PROCEDURE shiftJobSequence :
{{&includes}/{&Board}/shiftJobSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showFlashLight Include 
PROCEDURE showFlashLight :
{{&includes}/{&Board}/showFlashLight.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showLightBulb Include 
PROCEDURE showLightBulb :
{{&includes}/{&Board}/showLightBulb.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toggleLock Include 
PROCEDURE toggleLock :
/*------------------------------------------------------------------------------
  Purpose:     toggles lock status on lock buttom image click
  Parameters:  lock buttom image object handle
  Notes:       creates a history record used in redo & undo functions
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE jobLocked AS LOGICAL NO-UNDO.

  FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(ipWidget:PRIVATE-DATA) NO-ERROR.
  IF AVAILABLE ttblJob THEN
  ASSIGN
    jobLocked = NOT ttblJob.jobLocked
    ldummy = ipWidget:LOAD-IMAGE-UP(IF jobLocked THEN '{&images}/locked.gif' ELSE '')
    ldummy = ipWidget:MOVE-TO-TOP().
  RUN jobMoveHistory (ROWID(ttblJob),ttblJob.startDate,ttblJob.startTime,
                      ttblJob.endDate,ttblJob.endTime,jobLocked,
                      ttblJob.downtimeSpan).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE unknownResource Include 
PROCEDURE unknownResource :
{{&includes}/{&Board}/unknownResource.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updatesPending Include 
PROCEDURE updatesPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT VALID-HANDLE(statusHandle) THEN
  DO:
    RUN VALUE(findProgram('{&viewers}','','/statusViewer.w')) PERSISTENT SET statusHandle.
    RUN adm-initialize IN statusHandle.
    RUN setPopup IN containerHandle (11,statusHandle).
    RUN passHandle IN statusHandle (THIS-PROCEDURE,'{&Board}').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE useSequence Include 
PROCEDURE useSequence :
/*------------------------------------------------------------------------------
  Purpose:     set useSequence logical value from schedule.w
  Parameters:  useSequence logical
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipUseSequence AS LOGICAL NO-UNDO.

  useSequence = ipUseSequence.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcPriority Include 
FUNCTION calcPriority RETURNS INTEGER
  (ipPriority AS INTEGER,ipResource AS CHARACTER,
   ipJobSequence AS INTEGER,ipResourceSequence AS INTEGER) :
  {{&includes}/{&Board}/calcPriority.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resourcePriority Include 
FUNCTION resourcePriority RETURNS INTEGER
  (ipResource AS CHARACTER) :
  {{&includes}/{&Board}/resourcePriority.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

