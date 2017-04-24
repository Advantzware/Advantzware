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
    File        : IServiceDefinitions
    Purpose     : Interface for classes keeping record of services
                  definition files (services.xml) loaded through the
                  ServiceLoader:Load method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 23 21:38:14 CEST 2014
    Notes       : May be implemented using a CharacterList class
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

INTERFACE Consultingwerk.Framework.IServiceDefinitions:

    /**
     * Purpose: Adds the specified value to the List.
     * Notes:
     * @param pcValue The value to add to the List
     */
    METHOD PUBLIC VOID Add (pcValue AS CHARACTER) .

    /**
     * Purpose: Returns the Enumerator for the List
     * Notes:
     * @return The Enumerator instance
     */
    METHOD PUBLIC CharacterListEnumerator GetEnumerator () .

    /**
     * Purpose: Removes the specified value from the List.
     * Notes:
     * @param pcValue The value to remove from the List
     */
    METHOD PUBLIC VOID Remove (pcValue AS CHARACTER) .

END INTERFACE.
