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
    File        : ServiceManagerMonitoringTask
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 05 14:13:42 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH.
USING Consultingwerk.Framework.*       FROM PROPATH .
USING Consultingwerk.OERA.*            FROM PROPATH .
USING Consultingwerk.OERA.Monitoring.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Monitoring.ServiceManagerMonitoringTask
    IMPLEMENTS IBusinessService:

    {Consultingwerk/OERA/Monitoring/dsServiceManagerMonitoring.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns the eBusinessServices Temp-Table with the monitoring information
                 about the service manager
        Notes:
        @param dsServiceManagerMonitoring Dataset from Consultingwerk/OERA/Monitoring/dsServiceManagerMonitoring.i
        @param poParameter Dummy parameter object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetServiceManagerMonitoring (INPUT-OUTPUT DATASET dsServiceManagerMonitoring,
                                                    poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE oServiceManager AS IServiceManagerMonitoring NO-UNDO .

        oServiceManager = CAST (FrameworkSettings:ServiceContainer:GetMandatoryService
                                    (Progress.Lang.Class:GetClass ("Consultingwerk.OERA.Monitoring.IServiceManagerMonitoring":U)),
                                IServiceManagerMonitoring) .

        CREATE eSessionInfo .
        ASSIGN eSessionInfo.Pid     = Consultingwerk.Environment:Pid
               eSessionInfo.Started = Consultingwerk.Environment:TimeStarted .

        oServiceManager:GetServiceManagerMonitoring (TABLE eBusinessServices BY-REFERENCE) .

    END METHOD .

END CLASS.
