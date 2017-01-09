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
    File        : TableNameGeneratorService
    Purpose     : Default TableNameGeneratorService implementation
                  The Temp-Table name prefix and suffix settings will still
                  be applied to the temp-table names.
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 25 09:55:06 CET 2013
    Notes       : Supports BusinessEntityDesignerSettings:ReplaceHyphenInTableName
                  and BusinessEntityDesignerSettings:ReplaceUnderScoreInTableName
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.*          FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH.
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.TableNameGeneratorService
    IMPLEMENTS ITableNameGeneratorService:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    /*------------------------------------------------------------------------------
        Purpose: Generates a name for a temp-table
        Notes:
        @param pcDatabaseName The name of the database table
        @param pcTableName The name of the database table
        @param dsBusinessEntity The Business Entity Designer Dataset
        @return The name for the new temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateTempTableName (pcDatabaseName AS CHARACTER,
                                                   pcTableName AS CHARACTER,
                                                   DATASET dsBusinessEntity):

/*        MESSAGE "db" pcDatabaseName SKIP*/
/*                "table" pcTableName     */
/*            VIEW-AS ALERT-BOX.    */

        IF BusinessEntityDesignerSettings:ReplaceHyphenInTableName THEN
            pcTableName = REPLACE (pcTableName, "-":U, " ":U) .

        IF BusinessEntityDesignerSettings:ReplaceUnderScoreInTableName THEN
            pcTableName = REPLACE (pcTableName, "_":U, " ":U) .

        IF BusinessEntityDesignerSettings:ReplaceHyphenInTableName OR
           BusinessEntityDesignerSettings:ReplaceUnderScoreInTableName THEN

           pcTableName = StringHelper:CamelCaseWords (pcTableName) .

        RETURN LEFT-TRIM (pcTableName, "_":U) .

    END METHOD .

END CLASS.
