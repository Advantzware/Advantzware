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
    File        : SchedulerJobPlan
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30/05/2016 10:32:42,862+02:00
    Notes       : Custom constructors that should not be replaced when the
                  DatasetModel class is regenerated, need to be annotated
                  using the @CustomConstructor. annotation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanDatasetModel
    INHERITS SchedulerJobPlanDatasetModel_Generated:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobPlanDatasetModel class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobPlanDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobPlanDatasetModel class
        Notes:
        @param pSchedulerJobPlanGuid The filter value for the eSmartSchedulerJobPlan.SchedulerJobPlanGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobPlanDatasetModel (pSchedulerJobPlanGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartSchedulerJobPlan:Filter:SchedulerJobPlanGuid:EQ (pSchedulerJobPlanGuid) .

        THIS-OBJECT:SmartSchedulerJobPlan:Filter:Run () .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobPlanDatasetModel class
        Notes:   Retrieves data based on a named query
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobPlanDatasetModel (pcTables AS CHARACTER,
                                       poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (pcTables, poNamedQuery) .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobPlanDatasetModel class
        Notes:   Retrieves data based on a named query
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobPlanDatasetModel (poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (poNamedQuery) .

    END CONSTRUCTOR.

END CLASS.
