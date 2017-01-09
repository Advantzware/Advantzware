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
    File        : CompilerHelper
    Purpose     : Contains routines to support working with the Progress
                  compiler
    Syntax      :
    Description : Static code
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 25 00:28:57 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Json.ObjectModel.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{ Consultingwerk/products.i }

CLASS Consultingwerk.Studio.CompilerHelper:

    {Consultingwerk/Studio/Listing/dsListing.i &ACCESS="PROTECTED STATIC"}
    {Consultingwerk/Studio/Xref/dsXmlXref.i &ACCESS="PROTECTED STATIC"}
    {Consultingwerk/Util/TempTables/ttFileNames.i &ACCESS=STATIC}

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE CompilerHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Compiles a file
        Notes:   Throws an CompileException in case of compile errors
        @param pcFileName The file name to compile
        @param pcSaveInto The directory to compile into, optional
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID CompileFile (pcFileName AS CHARACTER,
                                           pcSaveInto AS CHARACTER):

        DEFINE VARIABLE i      AS INTEGER                                    NO-UNDO.
        DEFINE VARIABLE oError AS Consultingwerk.Exceptions.CompileException NO-UNDO .

        IF pcSaveInto > "":U THEN
            COMPILE VALUE (pcFilename) SAVE INTO VALUE (pcSaveInto) .
        ELSE
            COMPILE VALUE (pcFileName) SAVE .

        IF COMPILER:ERROR THEN DO:

          oError = NEW Consultingwerk.Exceptions.CompileException () .
          oError:FileName = pcFileName .

          DO i = 1 TO COMPILER:NUM-MESSAGES:
              oError:AddMessage (COMPILER:GET-MESSAGE (i),0 ) .
          END.

          UNDO, THROW oError .
        END.

        CATCH err AS Progress.Lang.Error:

          oError = NEW Consultingwerk.Exceptions.CompileException () .
          oError:FileName = pcFileName .

          DO i = 1 TO err:NumMessages:
              oError:AddMessage (err:GetMessage (i), err:GetMessageNum (i)) .
          END.

          UNDO, THROW oError .

        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Compiles a set of files, optionally supporting MD5 hashing them for
                 webclient optimization
        Notes:   Processes the file masks in order, so that when *.cls is passed as the
                 first entry, all class files are compiled before the procedures that
                 might be referencing them
        @param pcSourcePath The source file path
        @param pcFileMask A comma-delimited list of file patterns to compile (e.g. *.cls,*.w,*.p)
        @param pcTargetPath The directory to compile into
        @param pcLogfileName The name of the logfile to create
        @param plMD5 Create MD5 hash during compilation (Web Client optimization)
        @return TRUE when all files compiled, FALSE in case of errors
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL CompileFiles (pcSourcePath AS CHARACTER,
                                               pcFileMask AS CHARACTER,
                                               pcTargetPath AS CHARACTER,
                                               pcLogfileName AS CHARACTER,
                                               plMD5 AS LOGICAL):

        DEFINE VARIABLE cMask     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFile     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cRelative AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTarget   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iMessage  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lError    AS LOGICAL   NO-UNDO INIT FALSE .
        DEFINE VARIABLE iEntries  AS INTEGER   NO-UNDO.

        FILE-INFORMATION:FILE-NAME = pcSourcePath .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN pcSourcePath = FILE-INFORMATION:FULL-PATHNAME .
        ELSE
            UNDO, THROW NEW InvalidParameterValueException ("pcSourcePath":U,
                                                            pcSourcePath,
                                                            "Consultingwerk.Studio.CompilerHelper":U) .

        FILE-INFORMATION:FILE-NAME = pcTargetPath .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN pcTargetPath = FILE-INFORMATION:FULL-PATHNAME .
        ELSE
            UNDO, THROW NEW InvalidParameterValueException ("pcTargetPath":U,
                                                            pcTargetPath,
                                                            "Consultingwerk.Studio.CompilerHelper":U) .

        IF pcLogfileName > "":U THEN
            OUTPUT TO VALUE (pcLogfileName) APPEND UNBUFFERED .

        ASSIGN iEntries = NUM-ENTRIES (pcFileMask) .

        DO i = 1 TO iEntries:
            ASSIGN cMask = ENTRY (i, pcFileMask) .

            IF cMask > "":U THEN .
            ELSE NEXT .

            EMPTY TEMP-TABLE ttFileNames .

            /* Get a list of files to compile */
            Consultingwerk.Util.FileHelper:GetFileList (pcSourcePath,
                                                        cMask,
                                                        OUTPUT TABLE ttFileNames BY-REFERENCE) .

            FOR EACH ttFileNames:

                ASSIGN cFile     = REPLACE (ttFileNames.FileName, "~\":U, "/":U)
                       cRelative = SUBSTRING (SUBSTRING (cFile, 1, R-INDEX (cFile, "/":U) - 1, "CHARACTER":U),
                                              LENGTH (pcSourcePath, "CHARACTER":U) +  2,
                                              -1,
                                              "CHARACTER":U).

                IF cRelative > "":U THEN
                    cTarget = pcTargetPath + "/":U + cRelative .
                ELSE
                    cTarget = pcTargetPath .

                ASSIGN cTarget = REPLACE (cTarget, "~\":U, "/":U).

                IF pcLogfileName > "":U THEN
                    PUT UNFORMATTED "[":U STRING (NOW, "99.99.9999 HH:MM:SS":U) "]":U
                                    " Compiling ":U
                                    cFile
                                    " into ":U
                                    cTarget SKIP .

                FILE-INFORMATION:FILE-NAME = cTarget .
                IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN .
                ELSE
                    Consultingwerk.Util.FileHelper:CreateDirectory (cTarget) .

                IF cFile MATCHES "*.cls":U THEN
                    COMPILE VALUE (cFile)
                            SAVE INTO VALUE (pcTargetPath /*cTarget*/)
                            GENERATE-MD5 = plMD5
                            NO-ERROR .
                ELSE
                    COMPILE VALUE (cFile)
                            SAVE INTO VALUE (cTarget)
                            GENERATE-MD5 = plMD5
                            NO-ERROR .

                DO iMessage = 1 TO ERROR-STATUS:NUM-MESSAGES:
                    IF pcLogfileName > "":U THEN
                        PUT UNFORMATTED "** ":U + ERROR-STATUS:GET-MESSAGE (iMessage) SKIP .
                END.

                IF COMPILER:ERROR = TRUE THEN
                    lError = TRUE .
            END.
        END.

        RETURN NOT lError .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Compiles a file to generate xml xref
        Notes:   Supresses warning messages from the compiler because they cannot be
                 distinguished from error messages
        @param pcFileName The file name to compile
        @param dsXmlXref The dsXmlXref dataset to load the XML XREF into. Should be called BY-REFERENCE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GenerateXmlXref (pcFileName AS CHARACTER,
                                               OUTPUT DATASET dsXmlXref):

        DEFINE VARIABLE oError    AS AppError  NO-UNDO .
        DEFINE VARIABLE cXrefFile AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO .
        DEFINE VARIABLE lSupress  AS LOGICAL   NO-UNDO .

        ASSIGN cXrefFile = Consultingwerk.Util.FileHelper:GetTempFileName() .

        lSupress = SESSION:SUPPRESS-WARNINGS .

        SESSION:SUPPRESS-WARNINGS = TRUE .

        COMPILE VALUE (pcFileName)
                XREF-XML VALUE (cXrefFile)
                NO-ERROR .

        /* Mike Fechner, Consultingwerk Ltd. 25.04.2011
           Handle warning:
           SAVE not specified, and r-code file .... exists.
           List/xref/preprocess/debug-list listing will proceed,
           but no r-code will be generated. (6430) */
        IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            IF ERROR-STATUS:GET-NUMBER (1) = 6430 THEN .
            ELSE DO:
                oError = NEW AppError (ERROR-STATUS:GET-MESSAGE (1), 0) .

                DO i = 2 TO ERROR-STATUS:NUM-MESSAGES:
                    oError:AddMessage (ERROR-STATUS:GET-MESSAGE (i), 0) .
                END.

                UNDO, THROW oError .
            END.
        END.

        DATASET dsXmlXref:READ-XML ("file":U, cXrefFile, "empty":U, ?, ?) .

        FINALLY:
            OS-DELETE VALUE (cXrefFile) .

            SESSION:SUPPRESS-WARNINGS = lSupress .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Listing Dataset of the last compile run
        Notes:
        @param dsListing OUTPUT DATASET from Consultingwerk/Studio/Listing/dsListing.i
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetLastListing (OUTPUT DATASET FOR dsListing):

        /* noop */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the XmlXref Dataset of the last compile run
        Notes:
        @param dsXmlXref OUTPUT DATASET from Consultingwerk/Studio/Xref/dsXmlXref.i
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetLastXmlXref (OUTPUT DATASET FOR dsXmlXref):

        /* noop */

    END METHOD.

END CLASS.
