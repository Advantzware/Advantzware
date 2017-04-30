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
    File        : ListBusinessServicesWebHandler
    Purpose     : Abstract base class for Web Handlers that process all
                  Business Services in a given package
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 08 00:00:24 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.JsdoGenericService.WebHandler.ListBusinessServicesWebHandler
    INHERITS SmartWebHandler
    ABSTRACT:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    { Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS=STATIC}

    DEFINE PROTECTED TEMP-TABLE ttTableEntry NO-UNDO
        FIELD BEName AS CHARACTER
        FIELD CatalogURL AS CHARACTER
        FIELD ResourceURL AS CHARACTER
        FIELD PNGURL AS CHARACTER
        INDEX idxMain IS PRIMARY UNIQUE BEName
        .

    /**
     * Purpose: Find ClassNames based on Package filtering including wildcards
     * Notes:
     * @param pcPackageName Startpackage and possible wildcard ".*"
     */
    METHOD PUBLIC VOID GatherClassInformation (pcPackageName AS CHARACTER):

        DEFINE VARIABLE lWildcard     AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cStartPackage AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFilename     AS CHARACTER NO-UNDO.

        IF NOT CAN-FIND (FIRST ttClassNames) THEN DO:
            &IF DEFINED (DotNetAccessible) NE 0 &THEN
            IF OPSYS BEGINS "WIN":U THEN
                ClassHelper:GetClassNamesInClassPath ("Consultingwerk.OERA.IBusinessService":U,
                                                      FALSE,
                                                      TABLE ttClassNames).
            ELSE
            &ENDIF
                ClassHelper:GetClassNamesInClassPathNoDotNet ("Consultingwerk.OERA.IBusinessService":U,
                                                              FALSE,
                                                              TABLE ttClassNames).
        END.

        IF LENGTH (pcPackageName, "CHARACTER":U) > 0 THEN
            lWildcard = (SUBSTRING (pcPackageName, LENGTH (pcPackageName, "CHARACTER":U), 1, "CHARACTER":U) = "*":U).

        cStartPackage = TRIM (TRIM (pcPackageName, "*":U), ".":U).

        FOR EACH ttClassNames ON ERROR UNDO, THROW:

            IF NOT ttClassNames.ClassName BEGINS cStartPackage THEN
                NEXT.

            IF NOT lWildcard THEN
                IF NUM-ENTRIES (TRIM (SUBSTRING (ttClassNames.ClassName,
                                                 LENGTH (cStartPackage,
                                                         "CHARACTER":U) + 1,
                                                 -1,
                                                 "CHARACTER":U),
                                      ".":U),
                                ".":U) > 1 THEN DO:
                    NEXT.
                END.

            CREATE ttTableEntry.
            ASSIGN
                ttTableEntry.BEName        = ttClassNames.ClassName
                ttTableEntry.CatalogURL    = "../../Catalog/":U + ttClassNames.ClassName
                ttTableEntry.ResourceURL   = "../../Resource/":U + ttClassNames.ClassName

                cFileName                  = ClassHelper:ClassNameToFile (ttClassNames.ClassName)
                cFilename                  = FileHelper:ReplaceFileExtension (cFilename, "png":U)
                FILE-INFORMATION:FILE-NAME = cFileName
                ttTableEntry.PNGURL        = IF FILE-INFORMATION:FULL-PATHNAME = ? THEN "":U ELSE "../png/":U + ttClassNames.ClassName + ".png":U
                .
        END.

    END METHOD.
&ENDIF

END CLASS.
