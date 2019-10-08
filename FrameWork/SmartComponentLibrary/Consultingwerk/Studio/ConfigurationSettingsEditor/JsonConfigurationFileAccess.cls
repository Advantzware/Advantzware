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
    File        : JsonConfigurationFileAccess
    Purpose     : Reads and Writes Json Configuration Settings
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed Nov 30 17:17:52 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Assertion.*                          FROM PROPATH.
USING Consultingwerk.Util.*                               FROM PROPATH.
USING Consultingwerk.Studio.ConfigurationSettingsEditor.* FROM PROPATH.
USING Progress.Json.ObjectModel.*                         FROM PROPATH.
USING Progress.Lang.*                                     FROM PROPATH.


BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Studio.ConfigurationSettingsEditor.JsonConfigurationFileAccess
    IMPLEMENTS IConfigurationFileAccess:

    { Consultingwerk/Studio/ConfigurationSettingsEditor/ttConfigurationSettings.i }

    /**
     * Purpose: Parses a configuration File and returns the content as in a TempTable
     * Notes:
     * @param pcFileName      The file name to parse
     * @param ttConfiguration THe Temp-Table containing the Attribute Names and Values
     */
     METHOD PUBLIC VOID ReadFile (INPUT  pcFileName AS CHARACTER,
                                  OUTPUT TABLE FOR ttConfiguration):

        DEFINE VARIABLE cNameArray         AS CHARACTER NO-UNDO EXTENT .
        DEFINE VARIABLE iElements          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE oJsonObject        AS JsonObject        NO-UNDO .
        DEFINE VARIABLE oObjectModelParser AS ObjectModelParser NO-UNDO .

        EMPTY TEMP-TABLE ttConfiguration.

        Consultingwerk.Assertion.FileAssert:Exists (pcFileName) .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        oObjectModelParser = NEW ObjectModelParser () .
        oJsonObject = CAST (oObjectModelParser:ParseFile (FILE-INFORMATION:FULL-PATHNAME),
                            JsonObject) .

        ASSIGN cNameArray = oJsonObject:GetNames().

        DO iElements = 1 TO EXTENT (cNameArray):

            CREATE ttConfiguration.
            ASSIGN
                ttConfiguration.ConfigurationName  = cNameArray[iElements]
                ttConfiguration.ConfigurationValue = oJsonObject:GetCharacter(cNameArray[iElements])
                .

        END.

    END METHOD.

    /**
     * Purpose: Writes the content of a TempTable into a configuration File
     * Notes:
     * @param pcFileName      The file name to write the data in
     * @param ttConfiguration THe Temp-Table containing the Attribute Names and Values
     */
    METHOD PUBLIC VOID WriteFile (INPUT pcFileName AS CHARACTER,
                                  INPUT TABLE FOR ttConfiguration):

        DEFINE VARIABLE oJsonObject        AS JsonObject        NO-UNDO .

        oJsonObject = NEW JsonObject().

        FOR EACH ttConfiguration:

            oJsonObject:Add (ttConfiguration.ConfigurationName,
                             ttConfiguration.ConfigurationValue).

        END.

        oJsonObject:WriteFile (pcFileName,
                               TRUE,
                               "UTF-8":U).

    END METHOD.

END CLASS.