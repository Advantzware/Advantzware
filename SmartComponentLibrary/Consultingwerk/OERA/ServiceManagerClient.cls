/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ServiceManagerClient
    Purpose     : Default IServiceManagerClient implementation
    Syntax      :
    Description : Allows to shut down business tasks or business entities
                  on the AppServer from the client
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 09 11:28:54 CET 2014
    Notes       : Espacially useful, with bound AppServer sessions with
                  PASOE or state-aware, state-reset
                  Calls into the Consultingwerk.OERA.ServiceManagerClientTask
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Framework.* FROM PROPATH .
USING Consultingwerk.OERA.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceManagerClient
    IMPLEMENTS IServiceManagerClient:

    /**
     * Purpose: Stops all Business Services Instances
     * Notes:
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     */
    METHOD PUBLIC VOID StopAllBusinessServices (pcPartition AS CHARACTER):

        DEFINE VARIABLE oServiceAdapter AS IServiceAdapterWithInvokeTask NO-UNDO .

        oServiceAdapter = CAST (FrameworkSettings:ServiceAdapter, IServiceAdapterWithInvokeTask) .

        oServiceAdapter:InvokeTask (pcPartition,
                                    "Consultingwerk.OERA.ServiceManagerClientTask":U,
                                    "StopAllBusinessServices":U,
                                    ?,
                                    ?) .

    END METHOD .

    /**
     * Purpose: Stops an Business Service Instance
     * Notes:
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
     */
    METHOD PUBLIC VOID StopBusinessService (pcPartition AS CHARACTER,
                                            pcServiceName AS CHARACTER):

        DEFINE VARIABLE oServiceAdapter AS IServiceAdapterWithInvokeTask NO-UNDO .

        oServiceAdapter = CAST (FrameworkSettings:ServiceAdapter, IServiceAdapterWithInvokeTask) .

        oServiceAdapter:InvokeTask (pcPartition,
                                    "Consultingwerk.OERA.ServiceManagerClientTask":U,
                                    "StopBusinessService":U,
                                    NEW CharacterHolder (pcServiceName),
                                    ?) .

    END METHOD .

END CLASS.
