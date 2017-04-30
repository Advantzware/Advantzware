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
    File        : ConfigurationProvider
    Purpose     : Configuration provider based on a JSON file
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 20 16:22:28 CET 2013
    Notes       : Supported from OpenEdge 11.x only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Framework.*  FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
&IF PROVERSION NE "10.2B" &THEN
USING Progress.Json.ObjectModel.* FROM PROPATH .
&ENDIF
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Framework.ConfigurationProvider
    IMPLEMENTS IConfigurationProvider:

    DEFINE VARIABLE oListNameValuePair AS ListNameValuePair NO-UNDO .

    /**
     * Purpose: Constructor for the ConfigurationProvider class
     * Notes:
     * @param pcFileName The file name to read configuration data from
     */
    CONSTRUCTOR PUBLIC ConfigurationProvider (pcFileName AS CHARACTER):

        DEFINE VARIABLE cNames AS CHARACTER NO-UNDO EXTENT .
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

&IF PROVERSION NE "10.2B" &THEN
        DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO .
        DEFINE VARIABLE oObjectModel AS ObjectModelParser NO-UNDO .
&ENDIF

        SUPER ().

&IF PROVERSION NE "10.2B" &THEN
        Consultingwerk.Assertion.FileAssert:Exists (pcFileName) .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        LogManager:WriteMessage(SUBSTITUTE ("########################################################################~n":U +
                                            "### Loading configuration~n":U +
                                            "###    File Name:          &1~n":U +
                                            "###    Absolute File Name: &2~n":U +
                                            "########################################################################~n":U,
                                            pcFileName,
                                            FILE-INFORMATION:FULL-PATHNAME), "ConfigurationProvider":U) .

        oObjectModel = NEW ObjectModelParser () .
        oJsonObject = CAST (oObjectModel:ParseFile (FILE-INFORMATION:FULL-PATHNAME),
                            JsonObject) .

        ASSIGN cNames             = oJsonObject:GetNames()
               oListNameValuePair = NEW ListNameValuePair ().

        DO i = 1 TO EXTENT (cNames):

            oListNameValuePair:Add (cNames[i],
                                    oJsonObject:GetCharacter (cNames[i])) .
        END.

&ELSE
        UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException () .
&ENDIF

    END CONSTRUCTOR.

    /**
     * Purpose: Returns a configuration setting
     * Notes:
     * @param pcName The configuration property name
     * @return The value for the configuration property or ? when not set
     */
    METHOD PUBLIC CHARACTER GetValue (pcName AS CHARACTER).

        DEFINE VARIABLE oPair AS NameValuePair NO-UNDO .

        oPair = oListNameValuePair:FindByName (pcName) .

        IF VALID-OBJECT (oPair) THEN
            RETURN oPair:Value .
        ELSE
            RETURN ? .

    END METHOD .

    /**
     * Purpose: Returns a configuration setting or the default value when the setting
     *          is not available
     * Notes:
     * @param pcName The configuration property name
     * @param pcDefault The default value to return when the setting is not availalbe
     * @return The value for the configuration property or ? when not set
     */
    METHOD PUBLIC CHARACTER GetValue (pcName AS CHARACTER,
                                      pcDefault AS CHARACTER):

        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

        ASSIGN cValue = THIS-OBJECT:GetValue (pcName) .

        IF cValue = ? THEN
            RETURN pcDefault .
        ELSE
            RETURN cValue .

    END METHOD .

END CLASS.
