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
    File        : IServiceManagerMonitoring
    Purpose     : Interface for Service Manager Implementations that
                  support monitoring
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 05 13:23:58 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.Monitoring.IServiceManagerMonitoring:

    {Consultingwerk/OERA/Monitoring/dsServiceManagerMonitoring.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns the eBusinessServices Temp-Table with the monitoring information
                 about the service manager
        Notes:
        @param eBusinessServices Temp-Table from Consultingwerk/OERA/Monitoring/eBusinessServices.i
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetServiceManagerMonitoring (OUTPUT TABLE eBusinessServices).

END INTERFACE.
