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
    File        : IServiceManagerClient
    Purpose     : Service Interface for classes that can shut down business
                  tasks on the server (client for the ServiceManager)
    Syntax      :
    Description : Allows to shut down business tasks or business entities
                  on the AppServer from the client
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 09 11:16:01 CET 2014
    Notes       : Espacially useful, with bound AppServer sessions with
                  PASOE or state-aware, state-reset
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.OERA.IServiceManagerClient:

    /**
     * Purpose: Stops all Business Services Instances
     * Notes:
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     */
    METHOD PUBLIC VOID StopAllBusinessServices (pcPartition AS CHARACTER) .

    /**
     * Purpose: Stops an Business Service Instance
     * Notes:
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
     */
    METHOD PUBLIC VOID StopBusinessService (pcPartition AS CHARACTER,
                                            pcServiceName AS CHARACTER) .

END INTERFACE.
