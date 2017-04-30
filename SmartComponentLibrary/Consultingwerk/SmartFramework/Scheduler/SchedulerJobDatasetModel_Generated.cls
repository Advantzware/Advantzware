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
    File        : SchedulerJob_Generated
    Purpose     : Auto Generated portion of the SmartSchedulerJob class
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 25/05/2016 19:55:59,734+02:00
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobDatasetModel_Generated
    INHERITS DatasetModel ABSTRACT:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJob.i &ACCESS=PROTECTED }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartSchedulerJob table model
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartSchedulerJob AS SmartSchedulerJobTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobDatasetModel_Generated class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobDatasetModel_Generated ():
        SUPER (DATASET dsSchedulerJob:HANDLE).

        THIS-OBJECT:SmartSchedulerJob = NEW SmartSchedulerJobTableModel (THIS-OBJECT, BUFFER eSmartSchedulerJob:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartSchedulerJob) .


    END CONSTRUCTOR.



END CLASS.
