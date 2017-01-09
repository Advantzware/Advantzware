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
    File        : SmartUnit
    Purpose     : SmartUnit core test execution
    Syntax      :
    Description : Invoked by the smartunit.p
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 22 10:46:21 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.SmartUnit.*             FROM PROPATH .
USING Consultingwerk.SmartUnit.Output.*      FROM PROPATH .
USING Consultingwerk.Studio.*                FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartUnit.SmartUnit:

    {Consultingwerk/Util/TempTables/ttFileNames.i &ACCESS=STATIC}
    {Consultingwerk/SmartUnit/dsTestsuites.i &ACCESS=STATIC}

    /*------------------------------------------------------------------------------
        Purpose: Flag indicating that code is executed from within a unit test
        Notes:   Property can only be assigned from Consultingwerk/SmartUnit/runtest.p
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Active AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET (arg AS LOGICAL):

        IF SOURCE-PROCEDURE:FILE-NAME <> "Consultingwerk/SmartUnit/runtest.p":U AND
           NOT REPLACE (SOURCE-PROCEDURE:FILE-NAME, "~\":U, "/":U) MATCHES "*Consultingwerk/SmartUnit/debugtest.p":U THEN

            UNDO, THROW NEW AppError ("Can only set the Active flag from Consultingwerk/SmartUnit/runtest.p.":U, 0) .

        SmartUnit:Active = arg .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Contains the parameters (e.g. from Annotations) for the currently
                 executed test
        Notes:   Property is typically set by the TestRunner while executing a
                 single test
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY CurrentTestParameters AS ListNameValuePair NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the procedure-handle of the PCT Library
        Notes:   This library provides access to the PCTRun parameters
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY PctLibrary AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Tells tests to produce verbose output
        Notes:   Set by the PCT environment based on the verbose attribute,
                 see https://code.google.com/p/pct/wiki/PCTRun
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Verbose AS LOGICAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the IClassAnnotationParser instance that
                 should be used bny this TestRunner instance
        Notes:   Factory method for the IClassAnnotationParser implementation
        @param pcClassFileName The name of the .cls file to parse
        @param plForceXmlXref Logical value indicating if on Windows the XmlXref based class annotation parser should be used
        @return The reference to the IClassAnnotationParser instance that should be used bny this TestRunner instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IClassAnnotationParser GetAnnotationParser (pcClassFileName AS CHARACTER,
                                                                     plForceXmlXref AS LOGICAL):

        &IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF plForceXmlXref = FALSE THEN
            RETURN NEW ClassAnnotationParser (pcClassFileName) .
        ELSE
        &ENDIF
        RETURN NEW XrefClassAnnotationParser (pcClassFileName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Private Constructor of the SmartUnit class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SmartUnit ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of a PCTRun parameter
        Notes:
        @param pcParameterName The name of the parameter to return
        @return The parameter value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetTestSuiteParameter (pcParameterName AS CHARACTER):

        RETURN DYNAMIC-FUNCTION ("getParameter":U IN SmartUnit:PctLibrary,
                                 pcParameterName) .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN ? . /* Ignore errors from above */
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses a fileset
        Notes:
        @param pcFileset The fileset such as Consultingwerk.SmartUnitTest.*,Consultingwerk.Test.SingleTest
        @param pcBaseFolder The base folder to search for class names
        @return The CharacterList with the names of the classes available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList ParseFileset (pcFileset AS CHARACTER,
                                                     pcBaseFolder AS CHARACTER):

        DEFINE VARIABLE oList      AS CharacterList       NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cEntry     AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cDirectory AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oClass     AS Progress.Lang.Class NO-UNDO .

        oList = NEW CharacterList ("":U, CHR (1)) .

        DO i = 1 TO NUM-ENTRIES (pcFileset) ON ERROR UNDO, THROW:

            ASSIGN cEntry = ENTRY (i, pcFileset) .

            IF cEntry MATCHES "*~~.~~*":U THEN DO:

                /* Remove the .* from the entry to receive the relative folder */
                ASSIGN cEntry     = SUBSTRING (cEntry, 1, LENGTH (cEntry) - 2)
                       cDirectory = REPLACE (cEntry, ".":U, "/":U) .

                IF CharacterType:IsNotNullOrEmpty (pcBaseFolder) THEN
                    FILE-INFORMATION:FILE-NAME = pcBaseFolder .
                ELSE
                    FILE-INFORMATION:FILE-NAME = ".":U .

                pcBaseFolder = FILE-INFORMATION:FILE-NAME .

                IF CharacterType:IsNullOrEmpty (cDirectory) THEN
                    ASSIGN cDirectory = pcBaseFolder .
                ELSE
                    cDirectory = SUBSTITUTE ("&1/&2":U,
                                             pcBaseFolder,
                                             cDirectory) .

                Consultingwerk.Util.FileHelper:GetFileList (cDirectory,
                                                            "*.cls":U,
                                                            OUTPUT TABLE ttFileNames) .

                FOR EACH ttFileNames ON ERROR UNDO, THROW:

                    oClass = ClassHelper:FileNameToClass (ttFileNames.FileName) .

                    IF VALID-OBJECT (oClass) THEN
                        oList:Add (oClass:TypeName) .
                END.
            END.
            ELSE
                oList:Add (cEntry) .
        END.

        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a fileset
        Notes:
        @param pcFileset The fileset such as Consultingwerk.SmartUnitTest.*,Consultingwerk.Test.SingleTest
        @return The CharacterList with the names of the classes available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList ParseFileset (pcFileset AS CHARACTER):
        RETURN SmartUnit:ParseFileset (pcFileset, ?) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the SmartUnit Parameters
        Notes:   Parameters are passed using PCTRun parameters
        @param phPctLibrary The handle of the PCT library that allows access to the PCTRun parameters
        @return The ITestSuite instance with the parameters for the test suite
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ITestSuite ParseTestParameters (phPctLibrary AS HANDLE):

        DEFINE VARIABLE oTestSuite AS TestSuite NO-UNDO.

        ASSIGN SmartUnit:PctLibrary       = phPctLibrary
               oTestSuite                 = NEW TestSuite ()
               oTestSuite:TestSuite       = DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "testSuite":U)
               oTestSuite:Tests           = DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "tests":U)
               oTestSuite:Output          = DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "output":U)
               oTestSuite:Services        = DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "services":U)
               oTestSuite:ImportClientLog = DataTypeHelper:ToLogical (DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "importClientLog":U))
               oTestSuite:ImportStdOut    = DataTypeHelper:ToLogical (DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "importStdOut":U))
               oTestSuite:HaltOnError     = DataTypeHelper:ToLogical (DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "haltOnError":U))
               oTestSuite:ForceXmlXref    = DataTypeHelper:ToLogical (DYNAMIC-FUNCTION ("getParameter":U IN phPctLibrary, "forceXmlXref":U))

               .

        /* Parameters for TestSuite, Tests and Output are requires */
        Consultingwerk.Assertion.Assert:NotNullOrEmpty (oTestSuite:TestSuite, "TestSuite":U) .
        Consultingwerk.Assertion.Assert:NotNullOrEmpty (oTestSuite:Tests,     "Tests":U) .
        Consultingwerk.Assertion.Assert:NotNullOrEmpty (oTestSuite:Output,    "Output":U) .

        RETURN oTestSuite .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Writes the test result to a JUnit compatible XML file
        Notes:
        @param pcOutputFile The output file name to write to
        @param dsTestsuites The dataset with the test result
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID WriteTestResult (pcOutputFile AS CHARACTER,
                                               DATASET dsTestsuites):

        DEFINE VARIABLE oWriter                AS JUnitOutputWriter      NO-UNDO .

        oWriter = NEW JUnitOutputWriter () .
        oWriter:WriteTo (pcOutputFile,
                         DATASET dsTestsuites BY-REFERENCE) .

        GarbageCollectorHelper:DeleteObject (oWriter) .

    END METHOD .

END CLASS.
