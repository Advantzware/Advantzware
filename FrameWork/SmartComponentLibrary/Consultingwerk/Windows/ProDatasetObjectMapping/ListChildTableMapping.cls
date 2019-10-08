/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ListChildTableMapping
    Purpose     : List of ChildTableMapping instances
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 09 10:20:28 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*                  FROM PROPATH .
USING Consultingwerk.Windows.ProDatasetObjectMapping.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Windows.ProDatasetObjectMapping.ListChildTableMapping
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i Consultingwerk.Windows.ProDatasetObjectMapping.ChildTableMapping}

    /*------------------------------------------------------------------------------
        Purpose: Adds and returns a ChildTableMapping instance to the list based on
                 the passed parameter values
        Notes:
        @param pcChildRelationName The name of the data-relation for the child table
        @param pcPropertyName The List<T> property for teh list of mapped objects
        @param pcFieldMapping The field mapping for the child table/object
        @return The resulting ChildTabelMapping instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ChildTableMapping Add (pcChildRelationName AS CHARACTER,
                                         pcPropertyName AS CHARACTER,
                                         pcFieldMapping AS CHARACTER):

        DEFINE VARIABLE oMapping AS ChildTableMapping NO-UNDO.

        oMapping = NEW ChildTableMapping () .

        ASSIGN oMapping:ChildRelationName = pcChildRelationName
               oMapping:PropertyName      = pcPropertyName
               oMapping:FieldMapping      = pcFieldMapping .

        THIS-OBJECT:Add (oMapping) .

        RETURN oMapping .

    END METHOD .

END CLASS.
