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
    File        : AssignSkipDictionaryEntry
    Purpose     : Describes a single statement that calls into the
                  AssignSkipDictionary method of a DataAccess class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 01 20:11:54 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING org.prorefactor.core.JPNode                      FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.AssignSkipDictionaryEntry:

    /**
     * Purpose: Returns the BufferName (first method argument)
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the JPNode Reference
     * Notes:
     */
    DEFINE PUBLIC PROPERTY JPNode AS JPNode NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the Skip Fields (second method argument)
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SkipFields AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor for the AssignSkipDictionaryEntry class
     * Notes:
     * @param pcBufferName The Buffer Name
     * @param pcSkipFields The Skip Fields
     * @param poJPNode The JPNode Reference
     */
    CONSTRUCTOR PUBLIC AssignSkipDictionaryEntry (pcBufferName AS CHARACTER,
                                                  pcSkipFields AS CHARACTER,
                                                  poJPNode AS JPNode):
        ASSIGN THIS-OBJECT:BufferName = pcBufferName
               THIS-OBJECT:SkipFields = pcSkipFields
               THIS-OBJECT:JPNode     = poJPNode .

    END CONSTRUCTOR.

END CLASS.
