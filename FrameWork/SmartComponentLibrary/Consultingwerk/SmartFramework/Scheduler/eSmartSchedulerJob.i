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
    File        : eSmartSchedulerJob.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 25.05.2016 19:55:49
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartSchedulerJob{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartSchedulerJobBefore{&SUFFIX} &ENDIF
    FIELD SchedulerJobGuid AS CHARACTER FORMAT "x(36)":U LABEL "Job Guid":T
    FIELD SchedulerJobName AS CHARACTER FORMAT "x(40)":U LABEL "JobName":T
    FIELD Description AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD MaxRuntime AS INTEGER FORMAT "zzzz9":U INIT "0":U LABEL "MaxRuntime":T
    FIELD JobCommand AS CLOB FORMAT "x(8)":U INIT ? LABEL "JobCommand":T

    INDEX Job AS UNIQUE PRIMARY SchedulerJobGuid ASCENDING
    INDEX JobName SchedulerJobName ASCENDING

    .

    