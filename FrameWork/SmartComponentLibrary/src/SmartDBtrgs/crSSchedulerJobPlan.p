/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : crSSchedulerJobPlan.p
    Purpose     : Create trigger for SmartSchedulerJobPlan table of SmartDB

    Syntax      :

    Description : 

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed May 25 17:39:01 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

TRIGGER PROCEDURE FOR CREATE OF SmartSchedulerJobPlan.

ASSIGN SmartSchedulerJobPlan.SchedulerJobPlanGuid = GUID.