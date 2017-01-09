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
    File        : IniFileParser
    Purpose     : Parser for Windows .ini file style configuration files
    Syntax      :
    Description : Works also for WebClient .prowcapp files
                  It is not guaranteed that entries in a section are unique
                  (although this would be desirable)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 15 11:14:32 CEST 2015
    Notes       : Consultingwerk\Framework\IniFileParser\dsIniFile.i
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*               FROM PROPATH .
USING Consultingwerk.Framework.IniFileParser.* FROM PROPATH .
USING Consultingwerk.Util.*                    FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.Framework.IniFileParser.IniFileParser:

    {Consultingwerk/Framework/IniFileParser/dsIniFile.i}

    /*------------------------------------------------------------------------------
        Purpose: Parses the given ini file
        Notes:
        @param pcFileName The name of the ini file to parse
        @param dsIniFile OUTPUT The Dataset with the ini file contents
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParseIniFile (pcFileName AS CHARACTER,
                                     OUTPUT DATASET dsIniFile):

        DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cCurrentSection AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cLine           AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iLine           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iPos            AS INTEGER   NO-UNDO .

        FileAssert:Exists (pcFileName) .

        DATASET dsIniFile:EMPTY-DATASET () .

        ASSIGN cFileName = FileHelper:FindFile (pcFileName) .

        INPUT FROM VALUE (cFileName) .

        REPEAT:
            IMPORT UNFORMATTED cLine .

            ASSIGN iLine = iLine + 1
                   cLine = TRIM (cLine) .

            IF cLine = "":U THEN
                NEXT .

            IF cLine BEGINS "[":U THEN DO:
                ASSIGN cCurrentSection = SUBSTRING (cLine, 2, StringHelper:FindClosingBracket (cLine, 1) - 2, "CHARACTER":U) .

                CREATE ttSection.
                ASSIGN ttSection.Section = cCurrentSection .

                NEXT .
            END.

            CREATE ttEntries.
            ASSIGN ttEntries.Section    = cCurrentSection
                   ttEntries.LineNumber = iLine
                   ttEntries.EntryName  = TRIM (ENTRY (1, cLine, "=":U)) .

            ASSIGN iPos = INDEX (cLine, "=":U) .

            IF iPos > 0 AND LENGTH (cLine, "CHARACTER":U) > iPos + 1 THEN
                ASSIGN ttEntries.EntryValue = TRIM (SUBSTRING (cLine, iPos + 1, -1, "CHARACTER":U)) .
        END.

        IF         CAN-FIND (FIRST ttEntries WHERE ttEntries.Section = "":U)
           AND NOT CAN-FIND (FIRST ttSection WHERE ttSection.Section = "":U) THEN

            CREATE ttSection .

    END METHOD .

END CLASS.
