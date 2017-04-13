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
    File        : XMLConfigurationFileAccess
    Purpose     : Reads and Writes XML Configuration Settings
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed Nov 30 20:33:03 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Studio.ConfigurationSettingsEditor.* FROM PROPATH.
USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Studio.ConfigurationSettingsEditor.XMLConfigurationFileAccess
    IMPLEMENTS IConfigurationFileAccess:

    { Consultingwerk/Studio/ConfigurationSettingsEditor/ttConfigurationSettings.i }

    /**
     * Purpose: Parses a configuration File and returns the content as in a TempTable
     * Notes:
     * @param pcFileName      The file name to parse
     * @param ttConfiguration THe Temp-Table containing the Attribute Names and Values
     */
    METHOD PUBLIC VOID ReadFile( INPUT  pcFileName AS CHARACTER,
                                 OUTPUT TABLE FOR ttConfiguration ):

        Consultingwerk.Assertion.FileAssert:Exists (pcFileName) .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        TEMP-TABLE ttConfiguration:READ-XML ("FILE":U,
                                             FILE-INFORMATION:FULL-PATHNAME,
                                             "EMPTY":U,
                                             ?,
                                             ?) .

    END METHOD.

    /**
     * Purpose: Writes the content of a TempTable into a configuration File
     * Notes:
     * @param pcFileName      The file name to write the data in
     * @param ttConfiguration THe Temp-Table containing the Attribute Names and Values
     */
    METHOD PUBLIC VOID WriteFile( INPUT pcFileName AS CHARACTER,
                                  INPUT TABLE FOR ttConfiguration ):

        TEMP-TABLE ttConfiguration:WRITE-XML("FILE":U, pcFileName, TRUE, "UTF-8":U).

    END METHOD.

END CLASS.