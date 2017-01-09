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
    File        : SchedulerJobStatusDatasetController
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 19:11:37
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity", type="DatasetController") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusDatasetController
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF :

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJobStatus.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller
        Notes:   Interface IDatasetController member
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SchedulerJobStatusDatasetController class
        Notes:   Assigns the DatasetHandle property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobStatusDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsSchedulerJobStatus:HANDLE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object detaching from the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object attaching to the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

END CLASS.