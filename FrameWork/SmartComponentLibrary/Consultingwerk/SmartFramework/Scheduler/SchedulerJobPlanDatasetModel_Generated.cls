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
    File        : SchedulerJobPlan_Generated
    Purpose     : Auto Generated portion of the SchedulerJobPlan class
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30/05/2016 10:32:42,857+02:00
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanDatasetModel_Generated
    INHERITS DatasetModel ABSTRACT:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJobPlan.i &ACCESS=PROTECTED }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SchedulerJobPlan table model
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartSchedulerJobPlan AS SmartSchedulerJobPlanTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobPlanDatasetModel_Generated class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobPlanDatasetModel_Generated ():
        SUPER (DATASET dsSchedulerJobPlan:HANDLE).

        THIS-OBJECT:SmartSchedulerJobPlan = NEW SmartSchedulerJobPlanTableModel (THIS-OBJECT, BUFFER eSmartSchedulerJobPlan:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartSchedulerJobPlan) .


    END CONSTRUCTOR.



END CLASS.
