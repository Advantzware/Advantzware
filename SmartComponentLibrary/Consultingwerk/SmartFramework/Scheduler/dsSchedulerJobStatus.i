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
    File        : dsSchedulerJobStatus.i
    Purpose     : Business Entity for SmartSchedulerJobStatus

    Syntax      :

    Description :

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 19:11:37
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsSchedulerJobStatus

{ Consultingwerk/SmartFramework/Scheduler/eSmartSchedulerJobStatus.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsSchedulerJobStatus{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartSchedulerJobStatus{&SUFFIX}

    .
