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
    File        : verify-resx-files.p
    Purpose     : Verifies if all resources in a .NET resource file (.resx)
                  are accessible

    Syntax      :

    Description : Useful to verify .resx files on OpenEdge 10.2B, where the
                  .NET Framework 4.0 is not accessible

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 01 13:44:35 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.SmartUnit.*             FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .

{Consultingwerk/products.i}
{Consultingwerk/Util/TempTables/ttFileNames.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN

{Consultingwerk/SmartUnit/dsTestSuites.i}

DEFINE VARIABLE resources  AS Progress.Util.ResourceManager NO-UNDO .
DEFINE VARIABLE cClassName AS CHARACTER                     NO-UNDO .
DEFINE VARIABLE cShortName AS CHARACTER                     NO-UNDO .
DEFINE VARIABLE oElements  AS CharacterList                 NO-UNDO .

/* ***************************  Main Block  *************************** */

CREATE eTestSuite .
ASSIGN eTestSuite.TestsuiteName = ".resx File Verification":U .

FileHelper:GetFileList (".":U,
                        "*.resx":U,
                        OUTPUT TABLE ttFileNames BY-REFERENCE) .

FOR EACH ttFileNames ON ERROR UNDO, THROW:
    IF ttFileNames.Filename MATCHES "*.template.resx":U THEN
        NEXT .

    ASSIGN cClassName = SUBSTRING (ttFileNames.FileName,
                                   1,
                                   R-INDEX (ttFileNames.FileName, ".":U) - 1) .

    IF cClassName BEGINS ".~\":U OR cClassName BEGINS "./":U THEN
        cClassName = SUBSTRING (cClassName, 3) .

    CREATE eTestCase .
    ASSIGN eTestCase.TestsuiteName = eTestSuite.TestSuiteName
           eTestCase.TestcaseName  = "resx file":U
           eTestCase.ClassName     = cClassName .

    ASSIGN cShortName = ENTRY (NUM-ENTRIES (cClassName, "~\":U), cClassName, "~\":U) .

    /* Mike Fechner, Consultingwerk Ltd. 15.01.2013
       Routine cannot validate FormName.<CultureName>.resx files */
    IF NUM-ENTRIES (cShortName, ".":U) > 1 THEN
        NEXT .

    DO ON ERROR UNDO, THROW:

        resources = NEW Progress.Util.ResourceManager (cClassName).

        oElements = ResourceFileHelper:GetElementNames (ttFileNames.FileName) .

        {Consultingwerk/foreachPrimitiveList.i Character cElement in oElements}
            resources:GetObject (cElement) .

            CATCH ex AS Progress.Lang.Error:
                ASSIGN eTestCase.ErrorMessage    = ErrorHelper:FormattedErrorMessages (ex)
                       eTestCase.ErrorStacktrace = ErrorHelper:StackTrace (ex) .
            END CATCH.
        END.

        ASSIGN cElement = ? .

        CATCH ex2 AS Progress.Lang.Error:
            ASSIGN eTestCase.ErrorMessage    = ErrorHelper:FormattedErrorMessages (ex2)
                   eTestCase.ErrorStacktrace = ErrorHelper:StackTrace (ex2) .
        END CATCH.
    END.

    FINALLY:
        IF VALID-OBJECT (resources) THEN
            resources:ReleaseAllResources () NO-ERROR .

        GarbageCollectorHelper:DeleteObject (resources) .
    END FINALLY.
END.

RETURN "0":U . /* PCTRun requirement */

CATCH err AS System.Exception:
    DEFINE VARIABLE oError AS Consultingwerk.Exceptions.Exception NO-UNDO .

    MESSAGE SUBSTITUTE ("Error validating: &1 (&2)~nElement: &3 ":U,
                        ttFileNames.FileName,
                        cClassName,
                        cElement) .

    oError = NEW Consultingwerk.Exceptions.Exception (err,
                                                      SUBSTITUTE ("Error validating: &1~nElement: &2 ":U,
                                                                  ttFileNames.FileName,
                                                                  cElement),
                                                      0) .

    IF err:NumMessages > 0 THEN DO:
        MESSAGE err:GetMessage (1) .

        oError:AddMessage (err:GetMessage (1),
                           err:GetMessageNum (1)) .
    END.

    UNDO, THROW oError .
END CATCH.

FINALLY:
    SmartUnit:WriteTestResult ("output/resxfileparser.xml":U,
                               DATASET dsTestsuites) .
END FINALLY.

&ENDIF
