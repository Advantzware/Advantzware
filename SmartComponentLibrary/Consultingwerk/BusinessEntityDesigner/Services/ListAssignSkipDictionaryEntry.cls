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
    File        : ListAssignSkipDictionaryEntry
    Purpose     : List of AssignSkipDictionary instances
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 01 20:14:18 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING org.prorefactor.core.JPNode                      FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.ListAssignSkipDictionaryEntry
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i AssignSkipDictionaryEntry}

    /**
     * Purpose: Adds an AssignSkipDictionaryEntry to the list
     * Notes:
     * @param pcBufferName The Buffer Name
     * @param pcSkipFields The Skip Fields
     * @param poJPNode The JPNode Reference
     * @return The new AssignSkipDictionaryEntry
     */
    METHOD PUBLIC AssignSkipDictionaryEntry Add (pcBufferName AS CHARACTER,
                                                 pcSkipFields AS CHARACTER,
                                                 poJPNode AS JPNode):

        RETURN THIS-OBJECT:Add (NEW AssignSkipDictionaryEntry (pcBufferName, pcSkipFields, poJPNode)) .

    END METHOD.

END CLASS.
