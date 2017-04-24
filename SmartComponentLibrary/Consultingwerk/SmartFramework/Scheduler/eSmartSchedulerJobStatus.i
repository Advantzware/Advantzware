/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/ 
/*------------------------------------------------------------------------
    File        : eSmartSchedulerJobStatus.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 28.08.2016 19:49:36
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="SchedulerJobStatusGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartSchedulerJobStatus{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartSchedulerJobStatusBefore{&SUFFIX} &ENDIF
    FIELD SchedulerJobStatusGuid AS CHARACTER FORMAT "x(36)":U LABEL "Job Status ID":T
    FIELD SchedulerJobGuid AS CHARACTER FORMAT "x(36)":U LABEL "Job Guid":T
    FIELD SchedulerJobPlanGuid AS CHARACTER FORMAT "x(36)":U INIT ? LABEL "Job Status ID":T
    FIELD ScheduleDateTime AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "Scheduled Job Execution":T
    FIELD JobName AS CHARACTER FORMAT "x(40)":U LABEL "JobName":T
    FIELD Recurring AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring":T
    FIELD JobStatus AS CHARACTER FORMAT "x(20)":U LABEL "Job Status":T
    FIELD StatusText AS CHARACTER FORMAT "x(40)":U LABEL "Status Text":T
    FIELD Percentage AS INTEGER FORMAT "zz9":U INIT "0":U LABEL "Progress":T
    FIELD MaxExecutionDelay AS INTEGER FORMAT "zzzz9":U INIT "0":U LABEL "Max. Delay":T
    FIELD Cancel AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Cancel Req.":T
    FIELD StartDateTime AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "Start DateTime":T
    FIELD FinishedDateTime AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "Finished DateTime":T
    FIELD JobParamter AS CLOB FORMAT "x(8)":U INIT ? LABEL "JobParamter":T
    FIELD ContextDataSet AS CLOB FORMAT "x(8)":U INIT ? LABEL "ContextDataSet":T
    FIELD UserGuid AS CHARACTER FORMAT "x(36)":U LABEL "UserGuid":T
    FIELD DataBaseConnections AS CHARACTER FORMAT "x(120)":U LABEL "DataBaseConnections":T
    FIELD ClientPrincipal AS RAW
    FIELD SchedulerJobName AS CHARACTER FORMAT "x(40)":U LABEL "JobName":T
    FIELD Description AS CHARACTER FORMAT "x(400)":U LABEL "Description":T

    INDEX Job SchedulerJobGuid ASCENDING Recurring ASCENDING
    INDEX JobStatus AS UNIQUE PRIMARY SchedulerJobStatusGuid ASCENDING

    .

    