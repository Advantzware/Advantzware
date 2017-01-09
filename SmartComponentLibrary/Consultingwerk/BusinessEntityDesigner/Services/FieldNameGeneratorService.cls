/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : FieldNameGeneratorService
    Purpose     : Default field name generator service
    Syntax      :
    Description : Returns just the field name
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 17 11:06:08 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.*          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.BusinessEntityDesigner.Services.FieldNameGeneratorService IMPLEMENTS IFieldNameGeneratorService:

    { Consultingwerk/BusinessEntityDesigner/Services/eField.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    /*------------------------------------------------------------------------------
        Purpose:    Generates a temp-table field name
        Notes:      Used by the Business Entity Generator when a table is dragged to the
                    design canvas or an existing temp-table (add new source table) or
                    when refreshing the table
        @param pcDatabaseTableName The name of the database talbe (source table)
        @param pcDatabaseFieldName The name of the database field (source field)
        @param pcTempTableName The name of the temp-table
        @param eField The temp-table with the fields currently present in the business entity model
        @return The name for the new temp-table field
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateFieldName (pcDatabaseTableName AS CHARACTER,
                                               pcDatabaseFieldName AS CHARACTER,
                                               pcTempTableName AS CHARACTER,
                                               TABLE FOR eField):

        IF BusinessEntityDesignerSettings:ReplaceHyphenInTableName THEN
            pcDatabaseFieldName = REPLACE (pcDatabaseFieldName, "-":U, " ":U) .

        IF BusinessEntityDesignerSettings:ReplaceUnderScoreInTableName THEN
            pcDatabaseFieldName = REPLACE (pcDatabaseFieldName, "_":U, " ":U) .

        IF BusinessEntityDesignerSettings:ReplaceHyphenInTableName OR
           BusinessEntityDesignerSettings:ReplaceUnderScoreInTableName THEN

           pcDatabaseFieldName = StringHelper:CamelCaseWords2 (pcDatabaseFieldName) .

        RETURN LEFT-TRIM (pcDatabaseFieldName, "_":U) .

    END METHOD.

END CLASS.
