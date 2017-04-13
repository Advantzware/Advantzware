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
    File        : dsSchedulerJobPlan.i
    Purpose     : Business Entity for SmartSchedulerJobPlan

    Syntax      :

    Description : 

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 10:32:39
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsSchedulerJobPlan

{ Consultingwerk/SmartFramework/Scheduler/eSmartSchedulerJobPlan.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsSchedulerJobPlan{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartSchedulerJobPlan{&SUFFIX} 

    .    
