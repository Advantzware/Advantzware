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
    File        : IScheduleJobResponse
    Purpose     : Interface for the response of the ISchedulerService:ScheduleJob
                  method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 13 09:29:26 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartFramework.Scheduler.IScheduleJobResponse:

    /**
     * Purpose: Returns the Guid of the SchedulerJob
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SchedulerJobGuid AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the Guid of the SchedulerJobStatus
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SchedulerJobStatusGuid AS CHARACTER NO-UNDO
    GET.

END INTERFACE.
