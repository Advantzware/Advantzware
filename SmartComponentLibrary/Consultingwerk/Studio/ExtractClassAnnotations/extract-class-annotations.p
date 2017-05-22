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
    File        : extract-class-annotations.p
    Purpose     : Startup procedure (from PCT ANT Task) for invoking the
                  ClassAnnotationExtract class

    Syntax      : Executed via PCTRun / ANT

    Description :

    Author(s)   :
    Created     : Wed Jul 01 14:39:41 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Studio.ExtractClassAnnotations.* FROM PROPATH .
USING Consultingwerk.Studio.ClassDocumentation.* FROM PROPATH.

DEFINE VARIABLE oParameter AS ExtractClassAnnotationsParameter NO-UNDO .
DEFINE VARIABLE oJob       AS ExtractClassAnnotationsJob       NO-UNDO .

/* ***************************  Main Block  *************************** */

SESSION:ERROR-STACK-TRACE = TRUE .

oParameter = NEW ExtractClassAnnotationsParameter () .

ASSIGN oParameter:BuildFiles              = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "buildFiles":U)
       oParameter:Directory               = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "directory":U)
       oParameter:FileMask                = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "fileMask":U)
       oParameter:ExcludeAnnotations      = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "excludeAnnotations":U)
       oParameter:OnlyWriteModified       = DataTypeHelper:ToLogical(DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "OnlyWriteModified":U))
       oParameter:OverwriteWriteProtected = DataTypeHelper:ToLogical(DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "overwriteWriteProtected":U))
       oParameter:Verbose                 = DataTypeHelper:ToLogical(DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "verbose":U))

       PctSupport:PctLibrary = SOURCE-PROCEDURE
    .

IF oParameter:OnlyWriteModified = ? THEN
    ASSIGN oParameter:OnlyWriteModified = TRUE .

FILE-INFORMATION:FILE-NAME = ".":U .

IF oParameter:Verbose THEN
    MESSAGE "[ExtractClassParameter] Working-Directoy:       ":U FILE-INFORMATION:FULL-PATHNAME                    SKIP
            "[ExtractClassParameter] Directoy:               ":U oParameter:Directory                       SKIP
            "[ExtractClassParameter] FileMask:               ":U oParameter:FileMask                        SKIP
            "[ExtractClassParameter] Build Files:            ":U oParameter:BuildFiles                      SKIP
            "[ExtractClassParameter] # Build Files:          ":U NUM-ENTRIES (oParameter:BuildFiles, " ":U) SKIP
            "[ExtractClassParameter] ExcludeAnnotations:     ":U oParameter:ExcludeAnnotations              SKIP
            "[ExtractClassParameter] OnlyWriteModified:      ":U oParameter:OnlyWriteModified               SKIP
            "[ExtractClassParameter] OverwriteWriteProtected:":U oParameter:OverwriteWriteProtected         SKIP .


oJob = NEW ExtractClassAnnotationsJob () .

oJob:Run (oParameter) .

RETURN "0":U .

CATCH err AS Progress.Lang.Error :
    MESSAGE Consultingwerk.Util.ErrorHelper:FormattedErrorMessagesExt (err) .
END CATCH.
