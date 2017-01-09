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
    File        : ProWcAppParser
    Purpose     : Parser for .prowcapp files
    Syntax      : 
    Description : Returns version information
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 15 23:37:23 CEST 2015
    Notes       : Consultingwerk/Windows/WebClient/ProWcAppParser/dsProWcApp.i 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                        FROM PROPATH .
USING Consultingwerk.Framework.IniFileParser.*          FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Consultingwerk.Windows.WebClient.ProWcAppParser.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.Windows.WebClient.ProWcAppParser.ProWcAppParser: 

    { Consultingwerk/Framework/IniFileParser/dsIniFile.i }
    { Consultingwerk/Windows/WebClient/ProWcAppParser/dsProWcApp.i }

    /*------------------------------------------------------------------------------
        Purpose: Parses the .prowcapp file refereced by the -prowcapp startup parameter
        Notes:   
        @param dsProWcApp OUTPUT The dataset with the .prowcapp file contents
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ParseProWcAppFile (OUTPUT DATASET dsProWcApp):
		
		DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        ASSIGN cFileName = StartupParameterHelper:GetStartupParameterValue ("prowcapp":U) .

        THIS-OBJECT:ParseProWcAppFile (cFileName, OUTPUT DATASET dsProWcApp) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the .prowcapp file with the given file name
        Notes:   
        @param pcFileName The name of the .prowcapp file to parse
        @param dsProWcApp OUTPUT The dataset with the .prowcapp file contents
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParseProWcAppFile (pcFileName AS CHARACTER,
                                          OUTPUT DATASET dsProWcApp):
        
        DEFINE VARIABLE oParser AS IniFileParser NO-UNDO . 
        DEFINE BUFFER b_ttEntries FOR ttEntries . 
        
        DATASET dsProWcApp:EMPTY-DATASET () .

        oParser = NEW IniFileParser () .
        
        oParser:ParseIniFile (pcFileName, 
                              OUTPUT DATASET dsIniFile) .

        FOR EACH ttEntries WHERE ttEntries.Section = "Versions":U ON ERROR UNDO, THROW:    
            CREATE ttVersions .
            ASSIGN ttVersions.VersionNumber = INTEGER (ttEntries.EntryName)
                   ttVersions.VersionName   = ttEntries.EntryValue .
                   
            IF CAN-FIND (FIRST b_ttEntries WHERE b_ttEntries.Section   = ttEntries.EntryName
                                             AND b_ttEntries.EntryName = "EndUserDescription":U) THEN DO:
                                                 
                FIND FIRST b_ttEntries WHERE b_ttEntries.Section   = ttEntries.EntryName
                                         AND b_ttEntries.EntryName = "EndUserDescription":U . 

                ASSIGN ttVersions.EndUserDescription = b_ttEntries.EntryValue .                                                                             
            END .                                                                  
        END.

        FOR EACH ttEntries WHERE LOOKUP (ttEntries.Section,
                                         "Main,VersionInfo,Prowcapp,Codebase,ApplicationProgramGroup,Components":U) > 0
                                                                ON ERROR UNDO, THROW:
            CREATE ttApplicationProperties .
            BUFFER-COPY ttEntries TO ttApplicationProperties .
        END.                                             

    END METHOD .

END CLASS.
