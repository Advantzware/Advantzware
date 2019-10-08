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
    File        : IConfigurationFileAccess
    Purpose     : Interface for ConfigurationFileAccess implementations
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed Nov 30 17:02:47 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Studio.ConfigurationSettingsEditor.IConfigurationFileAccess:

    {Consultingwerk/Studio/ConfigurationSettingsEditor/ttConfigurationSettings.i}

    /**
     * Purpose: Parses a configuration File and returns the content as in a TempTable
     * Notes:
     * @param pcFileName      The file name to parse
     * @param ttConfiguration THe Temp-Table containing the Attribute Names and Values
     */
    METHOD PUBLIC VOID ReadFile (pcFileName AS CHARACTER,
                                 OUTPUT TABLE ttConfiguration).

    /**
     * Purpose: Writes the content of a TempTable into a configuration File
     * Notes:
     * @param pcFileName      The file name to write the data in
     * @param ttConfiguration THe Temp-Table containing the Attribute Names and Values
     */
    METHOD PUBLIC VOID WriteFile (pcFileName AS CHARACTER,
                                  INPUT TABLE ttConfiguration).

END INTERFACE.