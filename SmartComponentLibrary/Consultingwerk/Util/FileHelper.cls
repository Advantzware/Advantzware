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
    File        : FileHelper
    Purpose     : File System Helper methods
    Syntax      : Static methods only, Protected constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 26 11:07:56 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*  FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Util.FileHelper:

    {Consultingwerk/Util/TempTables/ttFileNames.i &ACCESS=STATIC}

    /**
     * Purpose: Disallow instance creation
     * Notes:
     */
    CONSTRUCTOR PROTECTED FileHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Returns if the given string value can be found in the provided file
     *          name
     * Notes:
     * @param pcSearchString The string value to search
     * @param pcFileName The file name
     * @return Logical value indicating if the file can be found
     */
    METHOD PUBLIC STATIC LOGICAL CanFindTextInBinaryFile (pcSearchString AS CHARACTER,
                                                          pcFileName AS CHARACTER):

        DEFINE VARIABLE mpData  AS MEMPTR    NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iLength AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cString AS CHARACTER NO-UNDO .

        COPY-LOB FROM FILE pcFileName
                 TO mpData .

        iLength = GET-SIZE (mpData) .

        i = 1 .

        DO WHILE i < iLength:
            cString = TRIM (GET-STRING (mpData, i , 1010)) .

            IF INDEX (cString, pcSearchString)  > 0 THEN
                RETURN TRUE .

            i = i + LENGTH (cString, "CHARACTER":U) + 1 .
        END.

        RETURN FALSE .

        FINALLY:
            SET-SIZE (mpData) = 0 .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @param pcPath4 The fourth path entry to combine
     * @param pcPath5 The fifth path entry to combine
     * @param pcPath6 The sixth path entry to combine
     * @param pcPath7 The seventh path entry to combine
     * @param pcPath8 The eight path entry to combine
     * @param pcPath9 The ninth path entry to combine
     * @param pcPath10 The tenth path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER,
                                                pcPath4 AS CHARACTER,
                                                pcPath5 AS CHARACTER,
                                                pcPath6 AS CHARACTER,
                                                pcPath7 AS CHARACTER,
                                                pcPath8 AS CHARACTER,
                                                pcPath9 AS CHARACTER,
                                                pcPath10 AS CHARACTER):

        DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.

        ASSIGN cPath = pcPath1 .

        IF pcPath2 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath2 .

        IF pcPath3 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath3 .

        IF pcPath4 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath4 .

        IF pcPath5 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath5 .

        IF pcPath6 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath6 .

        IF pcPath7 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath7 .

        IF pcPath8 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath8 .

        IF pcPath9 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath9 .

        IF pcPath10 > "":U THEN
            ASSIGN cPath = cPath + (IF cPath > "":U THEN "/":U ELSE "":U) + pcPath10 .

        RETURN cPath .

    END METHOD.

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @param pcPath4 The fourth path entry to combine
     * @param pcPath5 The fifth path entry to combine
     * @param pcPath6 The sixth path entry to combine
     * @param pcPath7 The seventh path entry to combine
     * @param pcPath8 The eight path entry to combine
     * @param pcPath9 The ninth path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER,
                                                pcPath4 AS CHARACTER,
                                                pcPath5 AS CHARACTER,
                                                pcPath6 AS CHARACTER,
                                                pcPath7 AS CHARACTER,
                                                pcPath8 AS CHARACTER,
                                                pcPath9 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, pcPath3, pcPath4, pcPath5, pcPath6, pcPath7, pcPath8, pcPath9, ?) .

    END METHOD .

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @param pcPath4 The fourth path entry to combine
     * @param pcPath5 The fifth path entry to combine
     * @param pcPath6 The sixth path entry to combine
     * @param pcPath7 The seventh path entry to combine
     * @param pcPath8 The eight path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER,
                                                pcPath4 AS CHARACTER,
                                                pcPath5 AS CHARACTER,
                                                pcPath6 AS CHARACTER,
                                                pcPath7 AS CHARACTER,
                                                pcPath8 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, pcPath3, pcPath4, pcPath5, pcPath6, pcPath7, pcPath8, ?, ?) .

    END METHOD .

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @param pcPath4 The fourth path entry to combine
     * @param pcPath5 The fifth path entry to combine
     * @param pcPath6 The sixth path entry to combine
     * @param pcPath7 The seventh path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER,
                                                pcPath4 AS CHARACTER,
                                                pcPath5 AS CHARACTER,
                                                pcPath6 AS CHARACTER,
                                                pcPath7 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, pcPath3, pcPath4, pcPath5, pcPath6, pcPath7, ?, ?, ?) .

    END METHOD .

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @param pcPath4 The fourth path entry to combine
     * @param pcPath5 The fifth path entry to combine
     * @param pcPath6 The sixth path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER,
                                                pcPath4 AS CHARACTER,
                                                pcPath5 AS CHARACTER,
                                                pcPath6 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, pcPath3, pcPath4, pcPath5, pcPath6, ?, ?, ?, ?) .

    END METHOD .

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @param pcPath4 The fourth path entry to combine
     * @param pcPath5 The fifth path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER,
                                                pcPath4 AS CHARACTER,
                                                pcPath5 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, pcPath3, pcPath4, pcPath5, ?, ?, ?, ?, ?) .

    END METHOD .

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @param pcPath4 The fourth path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER,
                                                pcPath4 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, pcPath3, pcPath4, ?, ?, ?, ?, ?, ?) .

    END METHOD .

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @param pcPath3 The thrid path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER,
                                                pcPath3 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, pcPath3, ?, ?, ?, ?, ?, ?, ?) .

    END METHOD .

    /**
     * Purpose: Combines the provided path into a single path
     * Notes:   Does not validate the resulting path entry
     * @param pcPath1 The first path entry to combine
     * @param pcPath2 The second path entry to combine
     * @return The combined path
     */
    METHOD PUBLIC STATIC CHARACTER CombinePath (pcPath1 AS CHARACTER,
                                                pcPath2 AS CHARACTER):

        RETURN FileHelper:CombinePath (pcPath1, pcPath2, ?, ?, ?, ?, ?, ?, ?, ?) .

    END METHOD .

    /**
     * Purpose: Copies files from one path to a target directory
     * Notes:
     * @param pcSourcePath The source file path
     * @param pcFileMask A comma-delimited list of file patterns to copy (e.g. *.xml,*.resx)
     * @param pcTargetPath The directory to copy into
     */
    METHOD PUBLIC STATIC VOID CopyFiles (pcSourcePath AS CHARACTER,
                                         pcFileMask AS CHARACTER,
                                         pcTargetPath AS CHARACTER):

        DEFINE VARIABLE cMask     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFile     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cRelative AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTarget   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.

        FILE-INFORMATION:FILE-NAME = pcSourcePath .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN pcSourcePath = FILE-INFORMATION:FULL-PATHNAME .
        ELSE
            UNDO, THROW NEW InvalidParameterValueException ("pcSourcePath":U,
                                                            pcSourcePath,
                                                            "Consultingwerk.Util.FileHelper":U) .

        FILE-INFORMATION:FILE-NAME = pcTargetPath .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN pcTargetPath = FILE-INFORMATION:FULL-PATHNAME .
        ELSE
            UNDO, THROW NEW InvalidParameterValueException ("pcTargetPath":U,
                                                            pcTargetPath,
                                                            "Consultingwerk.Util.FileHelper":U) .

        maskLoop:
        DO i = 1 TO NUM-ENTRIES (pcFileMask) ON ERROR UNDO, THROW:
            ASSIGN cMask = ENTRY (i, pcFileMask) .

            IF cMask > "":U THEN .
            ELSE NEXT maskLoop .

            EMPTY TEMP-TABLE ttFileNames .

            /* Get a list of files to compile */
            Consultingwerk.Util.FileHelper:GetFileList (pcSourcePath,
                                                        cMask,
                                                        OUTPUT TABLE ttFileNames BY-REFERENCE) .

            FOR EACH ttFileNames ON ERROR UNDO, THROW:

                ASSIGN cFile     = REPLACE (ttFileNames.FileName, "~\":U, "/":U)
                       cRelative = SUBSTRING (SUBSTRING (cFile, 1, R-INDEX (cFile, "/":U) - 1, "CHARACTER":U),
                                              LENGTH (pcSourcePath, "CHARACTER":U) +  2,
                                              -1,
                                              "CHARACTER":U)
                       cFileName = SUBSTRING (cFile, R-INDEX (cFile, "/":U) + 1, -1, "CHARACTER":U).

                IF cRelative > "":U THEN
                    cTarget = pcTargetPath + "/":U + cRelative .
                ELSE
                    cTarget = pcTargetPath .

                ASSIGN cTarget = REPLACE (cTarget, "~\":U, "/":U).

                FILE-INFORMATION:FILE-NAME = cTarget .
                IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN .
                ELSE
                    Consultingwerk.Util.FileHelper:CreateDirectory (cTarget) .

                &IF DEFINED (DotNetAccessible) NE 0 &THEN
                System.IO.File:Copy (cFile, REPLACE (cTarget, "/":U, "~\":U) + "~\":U + cFileName, TRUE) .
                System.IO.File:SetAttributes (REPLACE (cTarget, "/":U, "~\":U) + "~\":U + cFileName,
                                              System.IO.FileAttributes:Normal) .

                &ELSE
                OS-COPY VALUE (cFile) VALUE (cTarget) .
                IF OS-ERROR > 0 THEN
                    UNDO, THROW NEW OsErrorException (OS-ERROR) .
                &ENDIF
            END.
        END.


    END METHOD.

    /**
     * Purpose: Creates all directories and subdirectories in the specified path.
     * Notes:
     * @param pcPath The path to create
     * @return 0 when success otherwise 999 on windows (Exception) or the OS-ERROR on UNIX
     */
    METHOD PUBLIC STATIC INTEGER CreateDirectory (pcPath AS CHARACTER):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE iPos AS INTEGER NO-UNDO INITIAL 2.

        &IF DEFINED (DotNetAccessible) NE 0 &THEN
        System.IO.Directory:CreateDirectory (REPLACE (pcPath, "/":U, "~\":U)) .

        RETURN 0 .

        @SuppressUnusedWarnings.
        CATCH err AS System.Exception:
            RETURN 999 .
        END CATCH.
        &ELSE
        ASSIGN pcPath = REPLACE (pcPath, "~\":U, "/":U) .

        iPos = INDEX (pcPath, "/":U, iPos) .

        DO WHILE iPos > 0:
            OS-CREATE-DIR VALUE (SUBSTRING (pcPath, 1, iPos - 1)) .

            iPos = INDEX (pcPath, "/":U, iPos + 1) .
        END.

        RETURN OS-ERROR .
        &ENDIF

    END METHOD.

    /**
     * Purpose: Returns the directory portion of a file name, absolutely or relatively
     * Notes:
     * @param pcFileName The base file name
     * @return The directory portion of the file name
     */
    METHOD PUBLIC STATIC CHARACTER DirectoryName (pcFileName AS CHARACTER):

        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

        IF OPSYS = "MS-DOS":U OR OPSYS = "WIN32":U THEN
            ASSIGN pcFileName = REPLACE (pcFileName, "~\":U, "/":U) .

        iIndex = R-INDEX (pcFileName, "/":U) .

        IF iIndex > 1 THEN
            RETURN SUBSTRING (pcFileName, 1, iIndex - 1) .

        RETURN pcFileName .

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Ensures that the given OS file is cased in exactly the same way as
     *          passed in
     * Notes:
     * @param pcFileName The file name to verify
     */
    METHOD PUBLIC STATIC VOID EnsureFileNameCase (pcFileName AS CHARACTER):

        DEFINE VARIABLE oFileSystemInfoArray AS "System.IO.FileSystemInfo[]" NO-UNDO .

        DEFINE VARIABLE cFileNameOnDisk      AS CHARACTER NO-UNDO CASE-SENSITIVE .
        DEFINE VARIABLE cFileName            AS CHARACTER NO-UNDO CASE-SENSITIVE .

        FileAssert:Exists (pcFileName) .

        ASSIGN cFileName            = FileHelper:ShortFileName (pcFileName)

               oFileSystemInfoArray = (NEW System.IO.DirectoryInfo (FileHelper:DirectoryName (pcFileName)))
                                               :GetFileSystemInfos (FileHelper:ShortFileName (pcFileName))

               cFileNameOnDisk      = CAST (oFileSystemInfoArray:GetValue(0), System.IO.FileSystemInfo):Name.

        IF cFileName <> cFileNameOnDisk THEN DO:
            ASSIGN FILE-INFORMATION:FILE-NAME = pcFileName
                   cFileName           = FileHelper:DirectoryName (FILE-INFORMATION:FULL-PATHNAME) +
                                         "~\":U +
                                         FileHelper:ShortFileName (pcFileName)
                   cFileName           = REPLACE (cFileName, "/":U, "~\":U).

            Microsoft.VisualBasic.FileSystem:Rename (FILE-INFORMATION:FULL-PATHNAME, cFileName) .
        END.

    END METHOD .
&ENDIF

    /**
     * Purpose: Returns True when the file can be found in the PROPATH
     * Notes:
     * @param pcFileName The file name to search
     * @return Logical value indicating if the file can be found
     */
    METHOD PUBLIC STATIC LOGICAL Exists (pcFileName AS CHARACTER):

        FILE-INFORMATION:FILE-NAME = pcFileName .

        IF FILE-INFORMATION:FULL-PATHNAME <> ? THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Returns the absolute file name for the given file
     * Notes:
     * @param pcFileName The file name to search
     * @return The absolute path to the file or ? when the file is not found
     */
    METHOD PUBLIC STATIC CHARACTER FindFile (pcFileName AS CHARACTER):

        FILE-INFORMATION:FILE-NAME = pcFileName .

        RETURN FILE-INFORMATION:FULL-PATHNAME .

    END METHOD.

    /**
     * Purpose: Returns the file name extension of an operating system file excluding
     *          the dot (.). If the file does not have an extension, returns Null ("")
     * Notes:   Wrapper/facade to adecomm/_osfext.p, but excludes the leading dot
     * @param pcFileName The name of the file to return the extension for
     * @return The file extension
     */
    METHOD PUBLIC STATIC CHARACTER FileExtension (pcFileName AS CHARACTER):

        DEFINE VARIABLE cExtension AS CHARACTER NO-UNDO.

        RUN adecomm/_osfext.p (pcFileName, OUTPUT cExtension) .

        IF cExtension BEGINS ".":U THEN
            RETURN SUBSTRING (cExtension, 2) .

        RETURN cExtension.

    END METHOD.

    /**
     * Purpose: Returns the List of files on disk for the specified criteria
     * Notes:
     * @param pcDirectory The directory to search files in (including subdirectories)
     * @param pcFileMask A comma-delimited list of file name patterns to return (e.g. *.cls,*.w,*.p)
     * @param ttFileNames OUTPUT TABLE containing the matching file names (Consultingwerk/Util/TempTables/ttFileNames.i)
     */
    METHOD PUBLIC STATIC VOID GetFileList (pcDirectory AS CHARACTER,
                                           pcFileMask AS CHARACTER,
                                           OUTPUT TABLE ttFileNames):

        DEFINE VARIABLE cMask AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

        EMPTY TEMP-TABLE ttFileNames .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        DEFINE VARIABLE oFiles AS "System.String[]" NO-UNDO .
&ENDIF

        maskLoop:
        DO i = 1 TO NUM-ENTRIES (pcFileMask):
            ASSIGN cMask = ENTRY (i, pcFileMask) .

            IF cMask > "":U THEN .
            ELSE NEXT maskLoop .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
            oFiles = System.IO.Directory:GetFiles (pcDirectory,
                                                   cMask,
                                                   System.IO.SearchOption:AllDirectories) .

            {Consultingwerk/foreach.i System.Object oFile in oFiles}
                CREATE ttFileNames.
                ASSIGN ttFileNames.FileName = UNBOX (oFile) .
            END.
&ELSE
            RUN Consultingwerk/Util/Support/getfilenames.p
                    (pcDirectory,
                     cMask,
                     INPUT-OUTPUT TABLE ttFileNames BY-REFERENCE) .


&ENDIF
        END.

    END METHOD.

    /**
     * Purpose: Returns the List of files on disk for the specified criteria
     * Notes:   The routine is an alternative to GetFileList and does not require
     *          access to the .NET Framework (suitable for 10.2B AppServer and
     *          UNIX)
     * @param pcDirectory The directory to search files in (including subdirectories)
     * @param pcFileMask A comma-delimited list of file name patterns to return (e.g. *.cls,*.w,*.p)
     * @param ttFileNames OUTPUT TABLE containing the matching file names (Consultingwerk/Util/TempTables/ttFileNames.i)
     */
    METHOD PUBLIC STATIC VOID GetFileListNoDotNet (pcDirectory AS CHARACTER,
                                                   pcFileMask AS CHARACTER,
                                                   OUTPUT TABLE ttFileNames):

        DEFINE VARIABLE cMask AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

        EMPTY TEMP-TABLE ttFileNames .

        maskLoop:
        DO i = 1 TO NUM-ENTRIES (pcFileMask):
            ASSIGN cMask = ENTRY (i, pcFileMask) .

            IF cMask > "":U THEN .
            ELSE NEXT maskLoop .

            RUN Consultingwerk/Util/Support/getfilenames.p
                    (pcDirectory,
                     cMask,
                     INPUT-OUTPUT TABLE ttFileNames BY-REFERENCE) .
        END.

    END METHOD.

    /**
     * Purpose: Returns a temporary filename either using a .NET API or based on
     *          a GUID and the AVM SESSION:TEMP-DIRECTORY
     * Notes:   The .NET GUI will create an empty temp-file to "block" the file
     * @return The name for a temporary file
     */
    METHOD PUBLIC STATIC CHARACTER GetTempFileName ():

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        RETURN System.IO.Path:GetTempFileName () .

        CATCH ex AS System.IO.IOException:
            IF SESSION:BATCH THEN DO:
                MESSAGE SUBSTITUTE ("System.IO.IOException: &1":U, ex:Message) .
                MESSAGE SUBSTITUTE ("Temp-Path:             &1":U, System.IO.Path:GetTempPath ()) .
            END .

            UNDO, THROW ex .
        END CATCH.

&ELSE
        RETURN SESSION:TEMP-DIRECTORY + GUID + ".tmp":U .
&ENDIF

    END METHOD.

    /**
     * Purpose: Returns a human readable file size including the Unit
     * Notes:   E.g 10 Bytes, 20 kB, 1.2 MB, ...
     * @param piFileSize The file size to convert into a human readable file size
     * @return The string with the file size including the unit
     */
    METHOD PUBLIC STATIC CHARACTER HumanReadableFileSize (piFileSize AS INT64):

        DEFINE VARIABLE cUnits   AS CHARACTER NO-UNDO INITIAL "Bytes,kB,MB,GB,TB":U.
        DEFINE VARIABLE iSize    AS INTEGER   NO-UNDO EXTENT 5.
        DEFINE VARIABLE iLargest AS INTEGER   NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iRest    AS INT64     NO-UNDO .
        DEFINE VARIABLE deResult AS DECIMAL   NO-UNDO .

        IF piFileSize = 0 THEN
            RETURN SUBSTITUTE ("0 &1":U, ENTRY (1, cUnits)) .

        ASSIGN iRest = piFileSize .

        loopBlk:
        DO i = 1 TO 4:

            ASSIGN iSize [i] = iRest MODULO 1024
                   iRest     = (iRest - iSize [i]) / 1024 .

            IF iSize[i] > 0 THEN
                iLargest = i .

            IF iRest = 0 THEN
                LEAVE loopBlk .
        END.

        IF iRest > 0 THEN
            ASSIGN iSize [5] = iRest
                   iLargest  = 5.

        IF iLargest <= 2 THEN
            RETURN SUBSTITUTE ("&1 &2":U, iSize[1], ENTRY (iLargest, cUnits)) .

        deResult = iSize[iLargest] + ROUND (iSize[iLargest - 1] / 1024, 1) .

        RETURN SUBSTITUTE ("&1 &2":U, deResult, ENTRY (iLargest, cUnits)) .

    END METHOD.

    /**
     * Purpose: Returns a normalized path name
     * Notes:   Replaces back slash with forward slash
     * @param pcSourcePathname The source path name
     * @return The path name with forward slahes only
     */
    METHOD PUBLIC STATIC CHARACTER NormalizePathname (pcSourcePathname AS CHARACTER):

        RETURN REPLACE (pcSourcePathname, "~\":U, "/":U).

    END METHOD .

    /**
     * Purpose: Returns the error message of an OS-ERROR value
     * Notes:
     * @param piOSError The OS-ERROR Error code
     * @return The OS-ERROR Text
     */
    METHOD PUBLIC STATIC CHARACTER OS-ERROR-TEXT (piOSError AS INTEGER):

        CASE piOSError:

            WHEN 0 THEN RETURN "No error"{&TRAN} .
            WHEN 1 THEN RETURN "Not owner"{&TRAN} .
            WHEN 2 THEN RETURN "No such file or directory"{&TRAN} .
            WHEN 3 THEN RETURN "Interrupted system call"{&TRAN} .
            WHEN 4 THEN RETURN "I/O error"{&TRAN} .
            WHEN 5 THEN RETURN "Bad file number"{&TRAN} .
            WHEN 6 THEN RETURN "No more processes"{&TRAN} .
            WHEN 7 THEN RETURN "Not enough core memory"{&TRAN} .
            WHEN 8 THEN RETURN "Permission denied"{&TRAN} .
            WHEN 9 THEN RETURN "Bad address"{&TRAN} .
            WHEN 10 THEN RETURN "File exists"{&TRAN} .
            WHEN 11 THEN RETURN "No such device"{&TRAN} .
            WHEN 12 THEN RETURN "Not a directory"{&TRAN} .
            WHEN 13 THEN RETURN "Is a directory"{&TRAN} .
            WHEN 14 THEN RETURN "File table overflow"{&TRAN} .
            WHEN 15 THEN RETURN "Too many open files"{&TRAN} .
            WHEN 16 THEN RETURN "File too large"{&TRAN} .
            WHEN 17 THEN RETURN "No space left on device"{&TRAN} .
            WHEN 18 THEN RETURN "Directory not empty"{&TRAN} .
            WHEN 999 THEN RETURN "Unmapped error (ABL default)"{&TRAN} .

            OTHERWISE RETURN "Unknown OS-ERROR"{&TRAN} .

        END CASE .

    END METHOD.

    /**
     * Purpose: Return relative path if file is in propath
     * Notes:   Based on adecomm/_relfile.p
     * @param pcFileName The (absolute) name of a file
     * @return The relative file name based on the current propath
     */
    METHOD PUBLIC STATIC CHARACTER RelativeFileName (pcFileName AS CHARACTER):

        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

        RUN adecomm/_relfile.p (pcFileName,
                                FALSE,
                                "":U,
                                OUTPUT cReturn) .

        RETURN cReturn .

    END METHOD .

    /**
     * Purpose: Replaces the extension of a file
     * Notes:   When the source file name does not have an extension, the extension
     *          will be added
     * @param pcSourceFileName The original file name optionally including a path
     * @param pcExtension The extension to use in the returned file name
     * @return The target file name with the replaces extension
     */
    METHOD PUBLIC STATIC CHARACTER ReplaceFileExtension (pcSourceFileName AS CHARACTER,
                                                         pcExtension AS CHARACTER):

        DEFINE VARIABLE cTarget   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iPathPos  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iDotPos   AS INTEGER   NO-UNDO.

        ASSIGN cTarget = FileHelper:NormalizePathname (pcSourceFileName) .

        iPathPos = R-INDEX (cTarget, "/":U) .

        IF iPathPos > 0 THEN
            ASSIGN cFileName = SUBSTRING (cTarget, iPathPos + 1, -1, "CHARACTER":U) .
        ELSE
            ASSIGN cFileName = cTarget .

        iDotPos = R-INDEX (cFileName, ".":U) .

        IF iDotPos > 0 THEN
            ASSIGN cFileName = SUBSTRING (cFileName, 1, iDotPos, "CHARACTER":U) + pcExtension .
        ELSE
            ASSIGN cFileName = cFileName + ".":U + pcExtension .

        IF iPathPos > 0 THEN
            RETURN SUBSTRING (cTarget, 1, iPathPos, "CHARACTER":U) + cFileName .
        ELSE
            RETURN cFileName .

    END METHOD .

    /**
     * Purpose: Returns just the file name portion of a relative or absolute file
     *          name
     * Notes:
     * @param pcPath The base path
     * @return The file name portion of the path
     */
    METHOD PUBLIC STATIC CHARACTER ShortFileName (pcPath AS CHARACTER):

        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

        IF OPSYS = "MS-DOS":U OR OPSYS = "WIN32":U THEN
            ASSIGN pcPath = REPLACE (pcPath, "~\":U, "/":U) .

        iIndex = R-INDEX (pcPath, "/":U) .

        IF iIndex > 0 THEN
            RETURN SUBSTRING (pcPath, iIndex + 1, -1, "CHARACTER":U) .

        RETURN pcPath .

    END METHOD .

    /**
     * Purpose: Returns just the file name portion of a relative or absolute file
     *          name without the file extension
     * Notes:
     * @param pcPath The base path
     * @return The file name portion of the path without the file extension
     */
    METHOD PUBLIC STATIC CHARACTER ShortFileNameWithoutExtension (pcPath AS CHARACTER):

        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iPos      AS INTEGER   NO-UNDO .

        ASSIGN cFileName = FileHelper:ShortFileName (pcPath)

               iPos      = R-INDEX (cFileName, ".":U).

        IF iPos > 1 THEN
            RETURN SUBSTRING (cFileName, 1, iPos - 1, "CHARACTER":U) .

    END METHOD .

END CLASS.
