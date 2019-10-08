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
    File        : GenerateLlConst
    Purpose     : Generator for Constants definitions for List & Label API's
    Syntax      :
    Description :
    Author(s)   : Mohamed Elcaid / Consultingerk Ltd.
    Created     : Mon Jun 20 13:06:05 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.GenerateLlConst:

    DEFINE TEMP-TABLE ttImport NO-UNDO
        FIELD PropLine     AS INTEGER
        FIELD PropName     AS CHARACTER
        FIELD PropValue    AS CHARACTER
        FIELD PropComment  AS CHARACTER
        FIELD PropTextLine AS CHARACTER
        INDEX PropLine     IS PRIMARY UNIQUE PropLine
        .

    DEFINE VARIABLE iLengthName  AS INTEGER NO-UNDO .
    DEFINE VARIABLE iLengthValue AS INTEGER NO-UNDO .

    DEFINE STREAM sInputFile .
    DEFINE STREAM sOutputFile .

    /*------------------------------------------------------------------------------
        Purpose: Reads the file content (comments and &GLOBAL-DEFINE lines)
                 into a Temp-Table
        Notes:
        @param pcFileToImport Absolute path and filename of the source file
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ReadFileToTempTable (pcFileToImport AS CHARACTER):

        DEFINE VARIABLE cTextLine AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cComment  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cName     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cValue    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iLine     AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iPosition AS INTEGER   NO-UNDO .

        FILE-INFORMATION:FILENAME = pcFileToImport .

        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Could not find input file '&1'!":U, pcFileToImport), 0).

        INPUT STREAM sInputFile FROM VALUE (FILE-INFORMATION:FULL-PATHNAME) .

        DO ON ERROR UNDO, THROW:
            importLoop: REPEAT ON ERROR UNDO, THROW:

                IMPORT STREAM sInputFile UNFORMATTED cTextLine .

                IF NOT cTextLine BEGINS ("&GLOBAL-DEFINE":U) THEN
                    NEXT importLoop.

                cTextLine = TRIM (cTextLine) .

                DO WHILE INDEX (cTextLine, "  ":U) > 0:
                    cTextLine = REPLACE (cTextLine, "  ":U, " ":U) .
                END.

                IF NOT NUM-ENTRIES (cTextLine,  " ":U) >= 3 THEN
                    NEXT importLoop.

                ASSIGN
                    iLine     = iLine + 1
                    cName     = ENTRY (1, cTextLine, " ":U) + " ":U + ENTRY (2, cTextLine, " ":U)
                    cValue    = ENTRY (3, cTextLine, " ":U)
                    cComment  = "":U
                    iPosition = INDEX (cTextLine, "/*":U)
                    .

                /* Get the comment of this text line. */
                IF iPosition > 0 THEN
                    cComment = SUBSTRING (cTextLine, iPosition) .

                CREATE ttImport.
                ASSIGN
                    ttImport.PropLine    = iLine
                    ttImport.PropName    = cName
                    ttImport.PropValue   = cValue
                    ttImport.PropComment = cComment
                    .

                /* Save the maximum length of this value. */
                IF iLengthName < LENGTH (ttImport.PropName) THEN
                    iLengthName = LENGTH (ttImport.PropName) .

                IF iLengthValue < LENGTH (ttImport.PropValue) THEN
                    iLengthValue = LENGTH (ttImport.PropValue) .
            END.

            /* Cleanup */
            FINALLY:
                INPUT STREAM sInputFile CLOSE .
            END FINALLY.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates a class file from the original include file
        Notes:
        @param pcFileToChange Absolute path and filename of the source file
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GenerateClassFile (pcFileToChange AS CHARACTER):

        THIS-OBJECT:ReadFileToTempTable (pcFileToChange).

        THIS-OBJECT:ReplaceText ("&GLOBAL-DEFINE":U,
                                 "DEFINE PUBLIC STATIC PROPERTY":U) .

        THIS-OBJECT:WriteTempTableToFile () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Replace text in the class-file.
        Notes:
        @param pcFromText Text to be replaced
        @param pcToText Text to replace with
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ReplaceText (pcFromText AS CHARACTER,
                                     pcToText   AS CHARACTER):

        FOR EACH ttImport NO-LOCK
            ON ERROR UNDO, THROW:

            IF ttImport.propName BEGINS ("/*":U) THEN
                ttImport.PropTextLine = FILL (" ":U, 4) + ttImport.propName .

            ELSE
                ttImport.PropTextLine = FILL (" ":U, 4) +
                                        SUBSTITUTE ("&1&2 AS INTEGER NO-UNDO INIT &3&4 GET. &5":U,
                                                    ttImport.propName,
                                                    FILL (" ":U, iLengthName  - LENGTH (ttImport.propName)),
                                                    FILL (" ":U, iLengthValue - LENGTH (ttImport.propValue)),
                                                    ttImport.PropValue,
                                                    ttImport.propComment
                                                    ).

            /* Replace the text. */
            ttImport.PropTextLine = REPLACE (ttImport.PropTextLine, pcFromText, pcToText) .

        END.

    END METHOD. /* ReplaceText */

    /*------------------------------------------------------------------------------
        Purpose: Writes the Temp-Table content into a class file
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID WriteTempTableToFile ():

        DEFINE VARIABLE lcSource     AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE lcProperties AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cUserName    AS CHARACTER NO-UNDO .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/ListAndLabel/LlConst.template":U .

        IF NOT FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            UNDO, THROW NEW AppError ("Template not found!":U, 0) .

        COPY-LOB FROM FILE FILE-INFORMATION:FULL-PATHNAME TO lcSource .

        FOR EACH ttImport NO-LOCK
           ON ERROR UNDO, THROW :

            lcProperties = lcProperties + ttImport.PropTextLine + System.Environment:NewLine .

        END.

        ASSIGN
            cUserName = Consultingwerk.Windows.API.Win32:GetUserName (Consultingwerk.Windows.API.ExtendedNameFormatEnum:NameDisplay)
            lcSource  = SUBSTITUTE (lcSource, lcProperties)
            lcSource  = REPLACE (lcSource, "@USER@":U, cUserName)
            lcSource  = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
            lcSource  = REPLACE (lcSource, "@DATE@":U, STRING (NOW, "99.99.9999 HH:MM:SS":U))
            lcSource  = REPLACE (lcSource, "@YEAR@":U, STRING (YEAR (TODAY), "9999":U))
            .

        COPY-LOB FROM lcSource TO FILE "Consultingwerk/Windows/ListAndLabel/LlConst.cls":U .

        FINALLY:

            /* Cleanup */
            EMPTY TEMP-TABLE ttImport .

            OUTPUT STREAM sOutputFile CLOSE .

        END FINALLY.

    END METHOD.

END CLASS.
