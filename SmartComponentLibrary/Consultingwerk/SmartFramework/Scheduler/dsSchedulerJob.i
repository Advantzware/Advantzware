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
    File        : dsSchedulerJob.i
    Purpose     : Business Entity for SmartSchedulerJob

    Syntax      :

    Description : 

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 25.05.2016 19:55:49
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsSchedulerJob

{ Consultingwerk/SmartFramework/Scheduler/eSmartSchedulerJob.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsSchedulerJob{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartSchedulerJob{&SUFFIX} 

    .    
