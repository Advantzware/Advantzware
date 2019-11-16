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
    File        : ServiceManagerClientTask
    Purpose     : Business Task used by the Consultingwerk.OERA.ServiceManagerClient
    Syntax      :
    Description : Allows to shut down business tasks or business entities
                  on the AppServer from the client
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 09 11:36:44 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceManagerClientTask
    IMPLEMENTS IBusinessService:

    /**
     * Purpose: Stops all Business Services Instances
     * Notes:
     * @param poParameter Ignored
     */
    METHOD PUBLIC VOID StopAllBusinessServices (poParameter AS Consultingwerk.ISerializable):

        ServiceManager:StopAllBusinessServices () .

    END METHOD .

    /**
     * Purpose: Stops an Business Service Instance
     * Notes:
     * @param poServiceName A CharacterHolder instance containing the name of the Business Service to stop
     */
    METHOD PUBLIC VOID StopBusinessService (poServiceName AS CharacterHolder):

        ServiceManager:StopBusinessService (poServiceName:Value) .

    END METHOD .

END CLASS.
