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
    File        : SchedulerJobBusinessEntity
    Purpose     : Business Entity for SmartSchedulerJob
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 25.05.2016 19:55:49
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

@BusinessEntityTable (name="eSmartSchedulerJob", MandatoryColumns="SchedulerJobName", DefaultSort="BY eSmartSchedulerJob.SchedulerJobName") .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJob.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SchedulerJobBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobBusinessEntity ():
        SUPER (DATASET dsSchedulerJob:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Scheduler.SchedulerJobDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        FOR EACH eSmartSchedulerJob ON ERROR UNDO, THROW:

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSchedulerJob:HANDLE,
                                               "SchedulerJobName":U,
                                               "Job Name cannot be empty.":U).

        END.

    END METHOD.

END CLASS.
