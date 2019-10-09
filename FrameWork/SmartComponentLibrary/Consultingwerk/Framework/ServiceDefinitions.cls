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
    File        : ServiceDefinitions
    Purpose     : Default Implementation of the IServiceDefinitions Interface
                  Service class that can keep record of services definition
                  files (services.xml) loaded through the ServiceLoader:Load
                  method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 23 21:45:33 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Framework.ServiceDefinitions
    INHERITS CharacterList
    IMPLEMENTS IServiceDefinitions:

    /**
     * Purpose: Removes the specified value from the List.
     * Notes:
     * @param pcValue The value to remove from the List
     */
    METHOD PUBLIC VOID Remove (pcValue AS CHARACTER):

        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

        iIndex = THIS-OBJECT:IndexOf (pcValue) .

        IF iIndex > 0 THEN
            THIS-OBJECT:Remove (iIndex) .

    END METHOD .

END CLASS.
