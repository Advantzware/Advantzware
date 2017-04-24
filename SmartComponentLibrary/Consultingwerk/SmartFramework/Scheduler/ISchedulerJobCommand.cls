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
    File        : ISchedulerJobCommand
    Purpose     : Interface for SchedulerJobCommand implementations
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 11:32:28 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.Scheduler.ISchedulerJobCommand:

    /**
     * Purpose: Starts the Scheduler Job Command
     * Notes:
     * @param poParameter The parmeter object for the job execution (from the SmartSchedulerJobStatus table)
     */
    METHOD PUBLIC VOID Start (poParameter AS Progress.Lang.Object).

END INTERFACE.
