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
    File        : LineEndVerificationTest
    Purpose     : Performs verification of the line ending character in the
                  source code
    Syntax      :
    Description : Ensures that all ABL source code uses windows style line
                  endings
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 03 18:21:28 CEST 2013
    Notes       : Some uses may have issues with checkin in code with mixed
                  line ending styles (CR vs CR/LF). This job reports code
                  with line ending style issues as a JUnit result
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartUnit.*                  FROM PROPATH .
USING Consultingwerk.Studio.LineEndVerification.* FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Studio.LineEndVerification.LineEndVerificationTest:

    {Consultingwerk/SmartUnit/dsTestsuites.i}
    {Consultingwerk/Util/TempTables/ttFileNames.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LineEndVerificationTest class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LineEndVerificationTest ():
        SUPER ().

        CREATE eTestSuite .
        ASSIGN eTestSuite.TestsuiteName = "Line Ending Verification":U .

        THIS-OBJECT:RunTest ("*.cls":U) .
        THIS-OBJECT:RunTest ("*.i":U) .
        THIS-OBJECT:RunTest ("*.p":U) .
        THIS-OBJECT:RunTest ("*.w":U) .

        FINALLY:
            SmartUnit:WriteTestResult ("output/crlf_validation.xml":U,
                                       DATASET dsTestsuites) .
        END FINALLY.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies the line ending stlye of all files matching the file name
                 pattern
        Notes:
        @param pcFilePattern The file name pattern to verify
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RunTest (pcFilePattern AS CHARACTER):

        DEFINE VARIABLE lcText     AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE iLength    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO.

        FileHelper:GetFileList (".":U,
                                pcFilePattern,
                                OUTPUT TABLE ttFileNames BY-REFERENCE) .

        FOR EACH ttFileNames ON ERROR UNDO, THROW:

            /* Skip encrpyted source */
            IF ttFileNames.FileName MATCHES "*editsrc.i":U OR
               ttFileNames.FileName MATCHES "*peditor.i":U OR
               ttFileNames.FileName MATCHES "*_pweditor.i":U OR
               ttFileNames.FileName MATCHES "*create-erm-license.p":U THEN
                NEXT .


            ASSIGN cClassName = FileHelper:RelativeFileName (ttFileNames.FileName)
                   cClassName = REPLACE (cClassName, ".":U, "_":U)
                   cClassName = REPLACE (cClassName, "/":U, ".":U)
                   cClassName = REPLACE (cClassName, "~\":U, ".":U)
                   .

            CREATE eTestCase .
            ASSIGN eTestCase.TestsuiteName = eTestSuite.TestSuiteName
                   eTestCase.TestcaseName  = "line ending":U
                   eTestCase.ClassName     = cClassName .

            DO ON ERROR UNDO, THROW:
                COPY-LOB FROM FILE ttFileNames.FileName TO lcText .

                ASSIGN iLength    = LENGTH (lcText)
                       lcText     = StringHelper:FixLineEnding (lcText)
                       .

                IF iLength <> LENGTH (lcText) THEN
                      ASSIGN eTestCase.ErrorMessage = "Line ending verification failed":U .

                CATCH err AS Progress.Lang.Error:
                    ASSIGN eTestCase.ErrorMessage = ErrorHelper:FormattedErrorMessagesExt (err) .
                END CATCH.
            END.
        END.

    END METHOD .

END CLASS.
