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
    File        : ClassHelper
    Purpose     : Class contains generic supporting routines to work
                  with classes
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 12 20:31:09 CEST 2010
    Notes       : This file is intended for GUI for .NET only
                  The result temp-table description is in Consultingwerk/Util/TempTables/ttClassNames.i
                  Provides logging for .classpath file usage through the "ClassHelper"
                  custom log entry type
                  More extensive logging can be activated through the "ClassHelperExt"
                  logging type
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Util.ClassHelper:

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC"}
    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC" &PREFIX="return_"}
    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC" &PREFIX="return2_"}
    {Consultingwerk/Util/TempTables/ttClassPath.i &ACCESS="PRIVATE STATIC"}
    {Consultingwerk/Util/TempTables/ttFileNames.i &ACCESS="PRIVATE STATIC"}

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE STATIC VARIABLE oProcess AS System.Diagnostics.Process NO-UNDO .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Ability to force GetClassNames to include classes from the
                 Consultingwerk.SmartComponents package
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ForceIncludeSmartComponents AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Logfile that should be created during
                 the Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath calls
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LogfileName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows to start the GetClassNames () in a separate runtime
        Notes:   Typically set from the _idestartup.p procedure in a project root
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UseExternalRuntimeForGetClassNames AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constrols that ClassHelper:GetClassNamesFromExternalRuntime does only
                 search for the rcode of getclassnames.p to evaluate the absolute filename
                 passed as the -p argument of the AVN
        Notes:   Required for customers that seperate r-code and source code during
                 development but still make source code and r-code available to the
                 AVM runtime when testing for whatever reasons. See SCL-837
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UseRcodeOnlyForExternalRuntime AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassHelper class
        Notes:   Protected default constructor
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ClassHelper():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Converts a class name into the source code file name (.cls)
        Notes:
        @param pcClassName The ClassName to convert into a source code file name
        @return The file name of the class source code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ClassNameToFile (pcClassName AS CHARACTER):

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcClassName} .

        RETURN REPLACE (pcClassName, ".":U, "/":U) + ".cls":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Progress.Lang.Class reference for a type referenced
                 by either the .cls or .r file name
        Notes:
        @param pcFileName The file name
        @return The Progress.Lang.Class reference of the matching typ
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Progress.Lang.Class FileNameToClass (pcFileName AS CHARACTER):

        DEFINE VARIABLE cShortFileName AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oClass         AS Progress.Lang.Class NO-UNDO .
        DEFINE VARIABLE cExtension     AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cClassName     AS CHARACTER           NO-UNDO .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
            RETURN ? .

        ASSIGN cShortFileName = FileHelper:RelativeFileName (FILE-INFORMATION:FULL-PATHNAME) .

        /* File Name should be .cls or .r */
        ASSIGN cExtension = ENTRY (NUM-ENTRIES (cShortFileName, ".":U), cShortFileName, ".":U) .

        IF cExtension = "R":U THEN DO:
            RCODE-INFORMATION:FILE-NAME = pcFileName .

            IF NOT RCODE-INFORMATION:IS-CLASS THEN
                RETURN ? .
        END.
        ELSE IF NOT cExtension = "CLS":U THEN
            RETURN ? .

        ASSIGN cClassName = SUBSTRING (cShortFileName, 1, R-INDEX (cShortFileName, ".":U) - 1, "CHARACTER":U)
               cClassName = REPLACE (cClassName, "/":U, ".":U)
               cClassName = REPLACE (cClassName, "~\":U, ".":U).

        DO ON ERROR UNDO, THROW:

            oClass = Progress.Lang.Class:GetClass (cClassName) .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error :
                RETURN ? .
            END CATCH.
        END.

        RETURN oClass .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Class Name for a type referenced by either the .cls or
                 .r file name
        Notes:   Does not verify if it's a valid class name
        @param pcFileName The file name
        @return The resulting class name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER FileNameToClassName (pcFileName AS CHARACTER):

        DEFINE VARIABLE cShortFileName AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cExtension     AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cClassName     AS CHARACTER           NO-UNDO .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
            RETURN ? .

        ASSIGN cShortFileName = FileHelper:RelativeFileName (FILE-INFORMATION:FULL-PATHNAME) .

        /* File Name should be .cls or .r */
        ASSIGN cExtension = ENTRY (NUM-ENTRIES (cShortFileName, ".":U), cShortFileName, ".":U) .

        IF cExtension = "R":U THEN DO:
            RCODE-INFORMATION:FILE-NAME = pcFileName .

            IF NOT RCODE-INFORMATION:IS-CLASS THEN
                RETURN ? .
        END.
        ELSE IF NOT cExtension = "CLS":U THEN
            RETURN ? .

        ASSIGN cClassName = SUBSTRING (cShortFileName, 1, R-INDEX (cShortFileName, ".":U) - 1, "CHARACTER":U)
               cClassName = REPLACE (cClassName, "/":U, ".":U)
               cClassName = REPLACE (cClassName, "~\":U, ".":U).

        RETURN cClassName .

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the specified root directory
                 that match the specified base type, optionally including abstract
                 classes
        Notes:   The routine is designed to run on a windows client only, as it
                 uses the .NET Framework, uses Progress.Lang.Object as the base
                 type for the ClassLookup through GetClassNames (pcBaseType, OUTPUT TABLE ttClassNames)
        @param ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNames (OUTPUT TABLE ttClassNames):

        GetClassNames ("Progress.Lang.Object":U,
                       ".":U,
                       FALSE,
                       "":U,
                       OUTPUT TABLE ttClassNames BY-REFERENCE) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the specified root directory
                 that match the specified base type (class or interface)
        Notes:   The routine is designed to run on a windows client only, as it
                 uses the .NET Framework
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNames (pcBaseType AS CHARACTER,
                                             OUTPUT TABLE ttClassNames):

        GetClassNames (pcBaseType,
                       ".":U,
                       FALSE,
                       "":U,
                       OUTPUT TABLE ttClassNames BY-REFERENCE) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the specified root directory
                 that match the specified base type (class or interface)
        Notes:   The routine is designed to run on a windows client only, as it
                 uses the .NET Framework
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param pcPath The directory to look in (including sub folders)
        @param ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNames (pcBaseType AS CHARACTER,
                                             pcPath AS CHARACTER,
                                             OUTPUT TABLE ttClassNames):

        GetClassNames (pcBaseType,
                       pcPath,
                       FALSE,
                       "":U,
                       OUTPUT TABLE ttClassNames BY-REFERENCE) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the specified root directory
                 that match the specified base type (class or interface), optionally
                 including abstract classes
        Notes:   The routine is designed to run on a windows client only, as it
                 uses the .NET Framework
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param pcPath The directory to look in (including sub folders)
        @param plIncludeAbstract Include abstract classes in the search
        @param pcPrefix The prefix (name space/package name) to add to the returned class names
        @param ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNames (pcBaseType AS CHARACTER,
                                             pcPath AS CHARACTER,
                                             plIncludeAbstract AS LOGICAL,
                                             pcPrefix AS CHARACTER,
                                             OUTPUT TABLE ttClassNames):

        DEFINE VARIABLE oFiles     AS "System.String[]"   NO-UNDO .
        DEFINE VARIABLE cFile      AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE i          AS INT64               NO-UNDO .
        DEFINE VARIABLE oClass     AS Progress.Lang.Class NO-UNDO .
        DEFINE VARIABLE cClassName AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cRootDir   AS CHARACTER           NO-UNDO .

        EMPTY TEMP-TABLE ttClassNames .

        IF ClassHelper:UseExternalRuntimeForGetClassNames THEN DO:

            ClassHelper:WriteLogMessage ("  Using external runtime.":U) .

            IF ClassHelper:GetClassNamesFromExternalRuntime (pcBaseType,
                                                             pcPath,
                                                             plIncludeAbstract,
                                                             pcPrefix) THEN
                RETURN .
        END.

        FILE-INFORMATION:FILE-NAME = pcPath .

        ASSIGN cRootDir = FILE-INFORMATION:FULL-PATHNAME .

        ClassHelper:WriteLogMessage (SUBSTITUTE ("    Processing directory: &1":U, cRootDir)) .

        oFiles = System.IO.Directory:GetFiles (cRootDir,
                                               "*.r":U,
                                               System.IO.SearchOption:AllDirectories) .

        ClassHelper:WriteLogMessage (SUBSTITUTE ("    &1 *.r files.":U, oFiles:LongLength)) .

        filesLoop:
        DO i = 0 TO oFiles:LongLength - 1:
            ASSIGN cFile = UNBOX(oFiles:GetValue (i)) .

            RCODE-INFORMATION:FILE-NAME = cFile .

            IF RCODE-INFORMATION:IS-CLASS THEN DO:
                ASSIGN cClassName = SUBSTRING (cFile, LENGTH (cRootDir, "CHARACTER":U) + 2, -1, "CHARACTER":U)
                       cClassName = REPLACE (SUBSTRING (cClassName, 1, LENGTH (cClassName, "CHARACTER":U) - 2, "CHARACTER":U), "~\":U, ".":U)
                       NO-ERROR .

                IF pcPrefix > "":U THEN
                    ASSIGN cClassName = SUBSTITUTE ("&1.&2":U,
                                                    pcPrefix,
                                                    cClassName) .

                ClassHelper:WriteLogMessage (SUBSTITUTE ("      File: &1":U,
                                                         cFile)) .
                ClassHelper:WriteLogMessage (SUBSTITUTE ("        ClassName: &1":U,
                                                         cClassName)) .

                /* Mike Fechner, Consultingwerk Ltd. 26.10.2011
                   Exclude SmartComponent Library from the class browser
                   We have observed crashes from the Progress.Lang.Class:GetClass
                   call with some of the SmartComponents.Implementation classes. */
                IF ClassHelper:ForceIncludeSmartComponents = FALSE AND cClassName BEGINS "Consultingwerk.SmartComponents.":U THEN
                    NEXT filesLoop .

                ASSIGN oClass     = Progress.Lang.Class:GetClass (cClassName)
                       NO-ERROR .

                ClassHelper:WriteLogMessage (SUBSTITUTE ("        VALID-OBJECT (oClass): &1":U,
                                                         VALID-OBJECT (oClass))) .

                IF NOT VALID-OBJECT (oClass) THEN
                    NEXT filesLoop .

                ClassHelper:WriteLogMessage (SUBSTITUTE ("        Abstract/Interface: &1 / &2":U,
                                                         oClass:IsAbstract(), oClass:IsInterface())) .

                IF (NOT plIncludeAbstract) AND (oClass:IsAbstract() OR oClass:IsInterface()) THEN
                    NEXT filesLoop .

                ClassHelper:WriteLogMessage (SUBSTITUTE ("        IsA: &1":U,
                                                         oClass:IsA (pcBaseType))) .

                IF oClass:IsA (pcBaseType) THEN DO:
                    CREATE ttClassNames .
                    ASSIGN ttClassNames.ClassName = cClassName .
                END.
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Starts a separate Progress client to perform the Class name lookup.
        Notes:   This appears to increase the reliability (reduction of AVM crashes)
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param pcPath The directory to look in (including sub folders)
        @param plIncludeAbstract Include abstract classes in the search
        @param pcPrefix The prefix (name space/package name) to add to the returned class names
        @return Logical value indicating if the external runtime could be started
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC LOGICAL GetClassNamesFromExternalRuntime (pcBaseType AS CHARACTER,
                                                                      pcPath AS CHARACTER,
                                                                      plIncludeAbstract AS LOGICAL,
                                                                      pcPrefix AS CHARACTER):

        DEFINE VARIABLE oStartInfo AS System.Diagnostics.ProcessStartInfo NO-UNDO .
        DEFINE VARIABLE cProcedure AS CHARACTER                           NO-UNDO .
        DEFINE VARIABLE cClasses   AS CHARACTER                           NO-UNDO .
        DEFINE VARIABLE cTempLog   AS CHARACTER                           NO-UNDO .

        IF ClassHelper:UseRcodeOnlyForExternalRuntime = TRUE THEN
            ASSIGN cProcedure = ? .
        ELSE DO:
            FILE-INFORMATION:FILE-NAME = "Consultingwerk/Util/Support/getclassnames.p":U .

            ASSIGN cProcedure = FILE-INFORMATION:FULL-PATHNAME .
        END.

        IF cProcedure = ? THEN DO:
            FILE-INFORMATION:FILE-NAME = "Consultingwerk/Util/Support/getclassnames.r":U .

            ASSIGN cProcedure = FILE-INFORMATION:FULL-PATHNAME .
        END.

        IF cProcedure = ? THEN
            UNDO, THROW NEW AppError ("Unable to find Consultingwerk/Util/Support/getclassnames.p":U, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 17.11.2014
           SCL-535: When the startup parameters (of a referenced .pf file) contain the
           -clientlog and -logthreshold startup parameter, we need to override that
           to avoid file access issues.
           ** Cannot open log file client.log, errno 32 (11076) */
        IF StartupParameterHelper:GetStartupParameter ("clientlog":U) > "":U THEN
            ASSIGN cTempLog  = " -logthreshold 0":U.

        oStartInfo = NEW System.Diagnostics.ProcessStartInfo () .
        oStartInfo:FileName = Consultingwerk.Util.SessionHelper:ExecutablePath() .
        oStartInfo:Arguments = Consultingwerk.Util.SessionHelper:StartupParameters()
                + SUBSTITUTE (" -b -p ~"&1~" &8 -param ~"&2|&3|&4|&5|&6|&7~"":U,
                              cProcedure,
                              pcBaseType,
                              pcPath,
                              plIncludeAbstract,
                              pcPrefix,
                              PROPATH,
                              ClassHelper:LogfileName,
                              cTempLog) .

        ClassHelper:WriteLogMessage (SUBSTITUTE ("  Launching: &1":U, oStartInfo:FileName)) .
        ClassHelper:WriteLogMessage (SUBSTITUTE ("  Parameters: &1":U, oStartInfo:Arguments)) .

        oStartInfo:WorkingDirectory = Consultingwerk.Util.SessionHelper:CurrentDirectory() .

        /* obtain access to stdout of the process */
        oStartInfo:RedirectStandardOutput = TRUE .

        oStartInfo:UseShellExecute = FALSE .

        oProcess = System.Diagnostics.Process:Start (oStartInfo) .

        IF VALID-OBJECT (oProcess) THEN DO:
            oProcess:WaitForExit() .
            cClasses = oProcess:StandardOutput:ReadToEnd() .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 20.04.2012
           Verify that a valid, file name has been returned. If not, the
           external runtime most likely failed due to invalid environment
           settings */
        FILE-INFORMATION:FILE-NAME = cClasses .

        ClassHelper:WriteLogMessage (SUBSTITUTE ("  Response file: &1 (&2)":U, cClasses, FILE-INFORMATION:FULL-PATHNAME)) .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN .
        ELSE DO:
            MESSAGE "The external runtime did not return a list of matching class names."{&TRAN} SKIP
                    "Using the project AVM next time to return the class names"{&TRAN}
                VIEW-AS ALERT-BOX TITLE "Class Browser"{&TRAN}.

            ClassHelper:UseExternalRuntimeForGetClassNames = FALSE  .

            RETURN FALSE .
        END.

        TEMP-TABLE ttClassNames:READ-XML ("FILE":U, cClasses, "APPEND":U, ?, ?) .

        OS-DELETE VALUE (cClasses) NO-ERROR .

        oProcess = ? .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        RETURN TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the directories specified in the
                 .classpath file and that match the specified base type
        Notes:   The routine is designed to run on a windows client only, as it
                 uses the .NET Framework
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param return_ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNamesInClassPath (pcBaseType AS CHARACTER,
                                                        OUTPUT TABLE return_ttClassNames):

        GetClassNamesInClassPath (pcBaseType,
                                  FALSE,
                                  OUTPUT TABLE return_ttClassNames BY-REFERENCE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the directories specified in the
                 .classpath file and that match the specified base type, optionally
                 including or excluding abstract classes
        Notes:   The routine is designed to run on a windows client only, as it
                 uses the .NET Framework
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param plIncludeAbstract Include abstract classes in the search
        @param return_ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNamesInClassPath (pcBaseType AS CHARACTER,
                                                        plIncludeAbstract AS LOGICAL,
                                                        OUTPUT TABLE return_ttClassNames):

        DEFINE VARIABLE iCount         AS INTEGER NO-UNDO .

        IF ClassHelper:LogfileName > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = ClassHelper:LogfileName .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                OS-DELETE VALUE (ClassHelper:LogfileName) .

            ClassHelper:WriteLogMessage (SUBSTITUTE ("GetClassNamesInClassPath &1 &2":U, pcBaseType, plIncludeAbstract)) .
        END.

        EMPTY TEMP-TABLE return_ttClassNames .

        FILE-INFORMATION:FILE-NAME = ".classpath":U .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN DO:
            LogManager:WriteMessage (SUBSTITUTE ("Using .classpath file: &1":U, FILE-INFORMATION:FULL-PATHNAME), "ClassHelper":U) .

            TEMP-TABLE ttClassPath:READ-XML ("FILE":U, FILE-INFORMATION:FULL-PATHNAME, "EMPTY":U, ?, ?) NO-ERROR .
        END.
        ELSE
            LogManager:WriteMessage ("No .classpath file found.":U, "ClassHelper":U) .

        IF NOT CAN-FIND (FIRST ttClassPath) THEN DO:
            ClassHelper:WriteLogMessage ("No .classpath entries are being used.":U) .
            LogManager:WriteMessage (SUBSTITUTE ("Searching working directory for: &1":U, pcBaseType), "ClassHelper":U) .

            ClassHelper:GetClassNames (pcBaseType, ".":U, plIncludeAbstract, "":U, OUTPUT TABLE return_ttClassNames) .
            RETURN.
        END.

        classPathLoop:
        FOR EACH ttClassPath ON ERROR UNDO, THROW:
            FILE-INFORMATION:FILE-NAME = ttClassPath.Directory .

            ClassHelper:WriteLogMessage (SUBSTITUTE ("Processing Directory: &1 Full-Pathname: &2":U,
                                                     ttClassPath.Directory,
                                                     FILE-INFORMATION:FULL-PATHNAME)) .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
                NEXT classPathLoop .

            LogManager:WriteMessage (SUBSTITUTE ("Searching directory &2 for: &1":U,
                                                 pcBaseType,
                                                 FILE-INFORMATION:FULL-PATHNAME), "ClassHelper":U) .

            ClassHelper:GetClassNames (pcBaseType,
                                       FILE-INFORMATION:FULL-PATHNAME,
                                       plIncludeAbstract,
                                       ttClassPath.Prefix,
                                       OUTPUT TABLE return2_ttClassNames) .

            return2_ttClassNamesLoop:
            FOR EACH return2_ttClassNames ON ERROR UNDO, THROW:
                IF CAN-FIND (FIRST return_ttClassNames WHERE return_ttClassNames.ClassName = return2_ttClassNames.ClassName) THEN
                    NEXT return2_ttClassNamesLoop .

                CREATE return_ttClassNames .
                BUFFER-COPY return2_ttClassNames TO return_ttClassNames .

                iCount = iCount + 1 .
            END.

            ClassHelper:WriteLogMessage (SUBSTITUTE ("&1 classes found.":U, iCount)) .
        END.

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the specified root directory
                 that match the specified base type (class or interface), optionally
                 including abstract classes
        Notes:   The routine is an alternative to GetClassNames and does not require
                 access to the .NET Framework (suitable for 10.2B AppServer and
                 UNIX or PASOE which does not support .NET on Windows as well)
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param pcPath The directory to look in (including sub folders)
        @param plIncludeAbstract Include abstract classes in the search
        @param pcPrefix The prefix (name space/package name) to add to the returned class names
        @param ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNamesNoDotNet (pcBaseType AS CHARACTER,
                                                     pcPath AS CHARACTER,
                                                     plIncludeAbstract AS LOGICAL,
                                                     pcPrefix AS CHARACTER,
                                                     OUTPUT TABLE ttClassNames):

        DEFINE VARIABLE cFile            AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oClass           AS Progress.Lang.Class NO-UNDO .
        DEFINE VARIABLE cClassName       AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cRootDir         AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE lExtendedLogging AS LOGICAL             NO-UNDO .
        DEFINE VARIABLE lDotNetType      AS LOGICAL             NO-UNDO .

        ASSIGN lExtendedLogging = LogManager:IsCustomLogEntryTypeActive("ClassHelperExt":U) .

        EMPTY TEMP-TABLE ttClassNames .

        FILE-INFORMATION:FILE-NAME = pcPath .

        ASSIGN cRootDir = FILE-INFORMATION:FULL-PATHNAME .

        Consultingwerk.Util.FileHelper:GetFileListNoDotNet (cRootDir,
                                                            "*.r":U,
                                                            OUTPUT TABLE ttFileNames) .

        IF lExtendedLogging THEN DO:
            LogManager:WriteMessage (SUBSTITUTE ("Found &1 .r files in &2":U,
                                                 BufferHelper:NumRecords(BUFFER ttFileNames:HANDLE),
                                                 cRootDir),
                                     "ClassHelperExt":U) .
        END.

        fileLoop: FOR EACH ttFileNames ON ERROR UNDO, THROW:

            ASSIGN cFile = ttFileNames.FileName .

            RCODE-INFORMATION:FILE-NAME = cFile .

            IF RCODE-INFORMATION:IS-CLASS THEN DO:

                ASSIGN cClassName = SUBSTRING (cFile, LENGTH (cRootDir, "CHARACTER":U) + 2, -1, "CHARACTER":U)
                       cClassName = REPLACE (SUBSTRING (cClassName, 1, LENGTH (cClassName, "CHARACTER":U) - 2, "CHARACTER":U), "~\":U, ".":U)
                       cClassName = REPLACE (cClassName, "/":U, ".":U)
                       NO-ERROR .

                IF pcPrefix > "":U THEN
                    ASSIGN cClassName = SUBSTITUTE ("&1.&2":U,
                                                    pcPrefix,
                                                    cClassName) .

                IF lExtendedLogging THEN
                    LogManager:WriteMessage (SUBSTITUTE ("Processing class: &1":U,
                                                         cClassName),
                                             "ClassHelperExt":U) .

                /* Exclude packages from scanning, that are known to contain no
                   Business Entities */
                IF pcBaseType = "Consultingwerk.OERA.BusinessEntity":U AND
                    cClassName BEGINS "Consultingwerk.":U AND NOT cClassName BEGINS "Consultingwerk.SmartFramework.":U THEN
                    NEXT fileLoop .

                ASSIGN lDotNetType = FileHelper:CanFindTextInBinaryFile ("System.":U, cFile) .

                IF lExtendedLogging THEN
                    LogManager:WriteMessage (SUBSTITUTE (".NET Type? : &1":U,
                                                         lDotNetType),
                                             "ClassHelperExt":U) .

                /* Mike Fechner, Consultingwerk Ltd. 04.09.2013
                   Is there a faster way to exclude .NET dependent
                   r-code on UNIX?
                   This is required as Progress.Lang.Class:GetClass (cClassName)
                   kills the 10.2B AppServer agent (probably because .NET is not
                   accessible) */
                IF lDotNetType THEN
                    NEXT fileLoop .

                /* Mike Fechner, Consultingwerk Ltd. 26.10.2011
                   Exclude SmartComponent Library from the class browser
                   We have observed crashes from the Progress.Lang.Class:GetClass
                   call with some of the SmartComponents.Implementation classes. */
                IF ClassHelper:ForceIncludeSmartComponents = FALSE AND cClassName BEGINS "Consultingwerk.SmartComponents.":U THEN
                    NEXT fileLoop .

                ASSIGN oClass     = Progress.Lang.Class:GetClass (cClassName)
                       NO-ERROR .

                IF lExtendedLogging THEN
                    LogManager:WriteMessage (SUBSTITUTE ("Valid class? : &1":U,
                                                         VALID-OBJECT (oClass)),
                                             "ClassHelperExt":U) .

                IF NOT VALID-OBJECT (oClass) THEN
                    NEXT fileLoop .

                IF lExtendedLogging THEN
                    LogManager:WriteMessage (SUBSTITUTE ("Abstract: &1 Interface: &2 TypeOf: &3":U,
                                                         oClass:IsAbstract(),
                                                         oClass:IsInterface(),
                                                         oClass:IsA (pcBaseType)),
                                             "ClassHelperExt":U) .

                IF (NOT plIncludeAbstract) AND (oClass:IsAbstract() OR oClass:IsInterface()) THEN
                    NEXT fileLoop .

                IF oClass:IsA (pcBaseType) THEN DO:
                    CREATE ttClassNames .
                    ASSIGN ttClassNames.ClassName = cClassName .
                END.
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns all class names found in the directories specified in the
                 .classpath file and that match the specified base type, optionally
                 including or excluding abstract classes
        Notes:   The routine is an alternative to GetClassNamesInClassPath and does
                 not require access to the .NET Framework (suitable for 10.2B AppServer
                 and UNIX)
        @param pcBaseType The base type (class or interface) that the returned classes need to match
        @param plIncludeAbstract Include abstract classes in the search
        @param return_ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetClassNamesInClassPathNoDotNet (pcBaseType AS CHARACTER,
                                                                plIncludeAbstract AS LOGICAL,
                                                                OUTPUT TABLE return_ttClassNames):

        EMPTY TEMP-TABLE return_ttClassNames .

        FILE-INFORMATION:FILE-NAME = ".classpath":U .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN DO:
            LogManager:WriteMessage (SUBSTITUTE ("Using .classpath file: &1":U, FILE-INFORMATION:FULL-PATHNAME), "ClassHelper":U) .

            TEMP-TABLE ttClassPath:READ-XML ("FILE":U, FILE-INFORMATION:FULL-PATHNAME, "EMPTY":U, ?, ?) NO-ERROR .
        END.
        ELSE
            LogManager:WriteMessage ("No .classpath file found.":U, "ClassHelper":U) .

        IF NOT CAN-FIND (FIRST ttClassPath) THEN DO:
            LogManager:WriteMessage (SUBSTITUTE ("Searching working directory for: &1":U, pcBaseType), "ClassHelper":U) .

            ClassHelper:GetClassNamesNoDotNet (pcBaseType,
                                               ".":U,
                                               plIncludeAbstract,
                                               "":U,
                                               OUTPUT TABLE return_ttClassNames) .
            RETURN.
        END.

        classpathentry: FOR EACH ttClassPath ON ERROR UNDO, THROW:
            FILE-INFORMATION:FILE-NAME = ttClassPath.Directory .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
                NEXT classpathentry.

            LogManager:WriteMessage (SUBSTITUTE ("Searching directory &2 for: &1":U,
                                                 pcBaseType,
                                                 FILE-INFORMATION:FULL-PATHNAME), "ClassHelper":U) .

            ClassHelper:GetClassNamesNoDotNet (pcBaseType,
                                               FILE-INFORMATION:FULL-PATHNAME,
                                               plIncludeAbstract,
                                               ttClassPath.Prefix,
                                               OUTPUT TABLE return2_ttClassNames) .

            return2_ttClassNames:
            FOR EACH return2_ttClassNames ON ERROR UNDO, THROW:
                IF CAN-FIND (FIRST return_ttClassNames WHERE return_ttClassNames.ClassName = return2_ttClassNames.ClassName) THEN
                    NEXT return2_ttClassNames .

                CREATE return_ttClassNames .
                BUFFER-COPY return2_ttClassNames TO return_ttClassNames .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the ShortClassName for the given Class object
        Notes:   The short name is the class name without the package
        @param poClass The class to return the short name for
        @return The short name of the class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ShortClassName (poClass AS Progress.Lang.Class):

        DEFINE VARIABLE cFullName AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass """Class"":U"} .

        ASSIGN cFullName = poClass:TypeName .

        RETURN ENTRY (NUM-ENTRIES (cFullName, ".":U), cFullName, ".":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Splits a full class name into package and class name
        Notes:   Performs no validation of type, just splits based on last period
        @param pcFullClassName The full class name to split
        @return The Consultingwerk.ClassName that represents the parts of the class name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ClassName SplitClassName (pcFullClassName AS CHARACTER):

        DEFINE VARIABLE oClassName AS Consultingwerk.ClassName NO-UNDO .
        DEFINE VARIABLE iIndex     AS INTEGER                  NO-UNDO .

        ASSIGN oClassName             = NEW ClassName ()
               oClassName:FullName    = pcFullClassName
               oClassName:ClassName   = ENTRY (NUM-ENTRIES (pcFullClassName, ".":U), pcFullClassName, ".":U)
               .

        ASSIGN iIndex = R-INDEX (pcFullClassName, ".":U) .

        IF iIndex > 1 THEN
            ASSIGN oClassName:PackageName = SUBSTRING (pcFullClassName, 1, iIndex - 1, "CHARACTER":U) .

        RETURN oClassName.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Writes a message to the logfile
        Notes:   Does nothing, when the ClassHelper:LogfileName is not set
        @param pcMessage The message to be written to the logfile
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID WriteLogMessage (pcMessage AS CHARACTER):

        IF ClassHelper:LogfileName > "":U THEN
            .
        ELSE
            RETURN .

        OUTPUT TO VALUE (ClassHelper:LogfileName) APPEND .

        PUT UNFORMATTED SUBSTITUTE ("&1 - &2":U,
                                    STRING (NOW, "99/99/9999 HH:MM:SS.SSS":U),
                                    pcMessage) SKIP .

        OUTPUT CLOSE .

    END METHOD .

END CLASS.
