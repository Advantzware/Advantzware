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
    File        : SchedulerJobStatus_Generated
    Purpose     : Auto Generated portion of the SchedulerJobStatus class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 28/08/2016 23:03:58,473+02:00
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusDatasetModel_Generated
    INHERITS DatasetModel ABSTRACT:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJobStatus.i &ACCESS=PROTECTED }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartSchedulerJobStatus table model
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartSchedulerJobStatus AS SmartSchedulerJobStatusTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobStatusDatasetModel_Generated class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobStatusDatasetModel_Generated ():
        SUPER (DATASET dsSchedulerJobStatus:HANDLE).

        THIS-OBJECT:SmartSchedulerJobStatus = NEW SmartSchedulerJobStatusTableModel (THIS-OBJECT, BUFFER eSmartSchedulerJobStatus:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartSchedulerJobStatus) .


    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Cleanup of the SmartSchedulerJobStatus records
        Notes:
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CleanupJobStatus (poParameter AS Consultingwerk.SmartFramework.Scheduler.CleanupJobStatusParameter):

        DATASET dsSchedulerJobStatus:EMPTY-DATASET () .

        IF THIS-OBJECT:UseInterface = Consultingwerk.OERA.UseInterfaceEnum:ServiceAdapter THEN
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    "CleanupJobStatus":U,
                                                                                    INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                                                    poParameter) .
        ELSE
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                                               "CleanupJobStatus":U,
                                                               INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                               poParameter) .

        {Consultingwerk/foreachABL.i Consultingwerk.OERA.TableModel oTable in THIS-OBJECT:TableModels}
            IF VALID-HANDLE (oTable:QueryHandle) THEN DO:
                oTable:QueryHandle:QUERY-OPEN () .
                oTable:QueryHandle:GET-FIRST () .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Business Entity Method GetNextAvailableJobStatus
        Notes:   Receives the dataset GetNextAvailableJobStatus from the backend
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetNextAvailableJobStatus (poParameter AS GetNextAvailableJobStatusParameter):

        DATASET dsSchedulerJobStatus:EMPTY-DATASET () .

        IF THIS-OBJECT:UseInterface = Consultingwerk.OERA.UseInterfaceEnum:ServiceAdapter THEN
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    "GetNextAvailableJobStatus":U,
                                                                                    INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                                                    poParameter) .
        ELSE
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                                               "GetNextAvailableJobStatus":U,
                                                               INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                               poParameter) .

        {Consultingwerk/foreachABL.i Consultingwerk.OERA.TableModel oTable in THIS-OBJECT:TableModels}
            IF VALID-HANDLE (oTable:QueryHandle) THEN DO:
                oTable:QueryHandle:QUERY-OPEN () .
                oTable:QueryHandle:GET-FIRST () .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Business Entity Method ScheduleNextJobStatus
        Notes:   Receives the dataset ScheduleNextJobStatus from the backend
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScheduleNextJobStatus (poParameter AS Consultingwerk.SmartFramework.Scheduler.ScheduleNextJobStatusParameter):

        DATASET dsSchedulerJobStatus:EMPTY-DATASET () .

        IF THIS-OBJECT:UseInterface = Consultingwerk.OERA.UseInterfaceEnum:ServiceAdapter THEN
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    "ScheduleNextJobStatus":U,
                                                                                    INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                                                    poParameter) .
        ELSE
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                                               "ScheduleNextJobStatus":U,
                                                               INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                               poParameter) .

        {Consultingwerk/foreachABL.i Consultingwerk.OERA.TableModel oTable in THIS-OBJECT:TableModels}
            IF VALID-HANDLE (oTable:QueryHandle) THEN DO:
                oTable:QueryHandle:QUERY-OPEN () .
                oTable:QueryHandle:GET-FIRST () .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Business Entity Method SetJobStatus
        Notes:   Receives the dataset SetJobStatus from the backend
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetJobStatus (poParameter AS Consultingwerk.SmartFramework.Scheduler.SetJobStatusParameter):

        DATASET dsSchedulerJobStatus:EMPTY-DATASET () .

        IF THIS-OBJECT:UseInterface = Consultingwerk.OERA.UseInterfaceEnum:ServiceAdapter THEN
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    "SetJobStatus":U,
                                                                                    INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                                                    poParameter) .
        ELSE
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                                               "SetJobStatus":U,
                                                               INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                               poParameter) .

        {Consultingwerk/foreachABL.i Consultingwerk.OERA.TableModel oTable in THIS-OBJECT:TableModels}
            IF VALID-HANDLE (oTable:QueryHandle) THEN DO:
                oTable:QueryHandle:QUERY-OPEN () .
                oTable:QueryHandle:GET-FIRST () .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Business Entity Method UpdateJobStatus
        Notes:   Receives the dataset UpdateJobStatus from the backend
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateJobStatus (poParameter AS Consultingwerk.SmartFramework.Scheduler.UpdateJobStatusParameter):

        DATASET dsSchedulerJobStatus:EMPTY-DATASET () .

        IF THIS-OBJECT:UseInterface = Consultingwerk.OERA.UseInterfaceEnum:ServiceAdapter THEN
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    "UpdateJobStatus":U,
                                                                                    INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                                                    poParameter) .
        ELSE
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                                               "UpdateJobStatus":U,
                                                               INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                               poParameter) .

        {Consultingwerk/foreachABL.i Consultingwerk.OERA.TableModel oTable in THIS-OBJECT:TableModels}
            IF VALID-HANDLE (oTable:QueryHandle) THEN DO:
                oTable:QueryHandle:QUERY-OPEN () .
                oTable:QueryHandle:GET-FIRST () .
            END.
        END.

    END METHOD .



END CLASS.
