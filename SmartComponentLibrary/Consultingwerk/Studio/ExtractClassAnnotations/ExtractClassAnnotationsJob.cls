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
    File        : ExtractClassAnnotationsJob
    Purpose     : Parses ABL source code using proparse and writes annotations
                  to a JSON file to provide runtime access to the annotations
                  using the Consultingwerk.Framework.TypeDescriptor classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 01 18:08:30 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.*       FROM PROPATH.
USING Consultingwerk.Studio.*                         FROM PROPATH.
USING Consultingwerk.Studio.ExtractClassAnnotations.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH.
USING Progress.Lang.*                                 FROM PROPATH .
USING System.IO.* FROM ASSEMBLY.

CLASS Consultingwerk.Studio.ExtractClassAnnotations.ExtractClassAnnotationsJob:

    {Consultingwerk/Util/TempTables/ttFileNames.i}

    /*------------------------------------------------------------------------------
        Purpose: Copies annotations from a ListAnnotations to a ListSerializableAnnotations
        Notes:
        @param poFrom The ListAnnotation to copy annotations from
        @param poTo The ListSerializableAnnotation to copy to
        @param pcExclude Annotations (including the @) to exclude from copy
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CopyAnnotations (poFrom AS ListAnnotation,
                                           poTo   AS ListSerializableAnnotation,
                                           pcExclude AS CHARACTER):

        DEFINE VARIABLE oSerializableAnnotatation AS SerializableAnnotation NO-UNDO .

        {Consultingwerk/foreachABL.i Annotation oAnnotation in poFrom}

            IF pcExclude > "":U AND ListHelper:CanDo (pcExclude, oAnnotation:Name) THEN
                NEXT .

            oSerializableAnnotatation = NEW SerializableAnnotation () .

            ASSIGN oSerializableAnnotatation:Name = oAnnotation:Name .

            {Consultingwerk/foreachABL.i NameValuePair oPair in oAnnotation:Parameters}
                oSerializableAnnotatation:Parameters:Add (oPair:Name, oPair:Value) .
            END.

            poTo:Add (oSerializableAnnotatation) .

        END.

    END METHOD .

    /**
     * Purpose: Returns if the new annotation JSON only differs by the
     *          annotation time stamp
     * Notes:
     * @param pcAnnotationFile The existing file with the annotation
     * @param plcAnnotations The new annotations JSON document
     * @return Logical value indicating if the annotations do only differ by the time stamp node
     */
    METHOD PROTECTED LOGICAL OnlyDifferByTimeStamp (pcAnnotationFile AS CHARACTER,
                                                    plcAnnotations AS LONGCHAR):

        DEFINE VARIABLE lcFile AS LONGCHAR NO-UNDO.

        COPY-LOB FROM FILE FileHelper:FindFile (pcAnnotationFile) TO lcFile .

        ASSIGN plcAnnotations = THIS-OBJECT:RemoveTimeStamp (plcAnnotations)
               lcFile         = THIS-OBJECT:RemoveTimeStamp (lcFile).

        RETURN (plcAnnotations EQ lcFile) .

    END METHOD.

    /**
     * Purpose: Removes the "TimeStamp" from the provided JSON document
     * Notes:   "TimeStamp": "2016-10-18T16:23:49.472+02:00",
     * @param plcJson The input JSON Document
     * @return The JSON Document with the time-stamp removed
     */
    METHOD PROTECTED LONGCHAR RemoveTimeStamp (plcJson AS LONGCHAR):

        DEFINE VARIABLE iPos  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iPos2 AS INTEGER NO-UNDO.

        ASSIGN iPos = INDEX (plcJson, '"TimeStamp": "':U) .

        IF iPos > 0 THEN DO:
            ASSIGN iPos2 = INDEX (plcJson, ",":U, iPos) .

            IF iPos2 > 0 THEN
                ASSIGN SUBSTRING (plcJson, iPos, iPos2 - iPos + 1) = "":U .
        END.

        RETURN REPLACE (plcJson, CHR (13), "":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Extracts class annotations into .annotation files
        Notes:
        @param poParameter The ExtractClassAnnotationsParameter with the data for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Run (poParameter AS ExtractClassAnnotationsParameter):

        DEFINE VARIABLE oParser                            AS ClassAnnotationParser             NO-UNDO .
        DEFINE VARIABLE cFileName                          AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE cClassName                         AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE oContainer                         AS ClassAnnotationContainer          NO-UNDO .
        DEFINE VARIABLE oSerializableClassMemberAnnotation AS SerializableClassMemberAnnotation NO-UNDO .
        DEFINE VARIABLE lcAnnotations                      AS LONGCHAR                          NO-UNDO .
        DEFINE VARIABLE oList                              AS ListAnnotation                    NO-UNDO .
        DEFINE VARIABLE oMemberList                        AS ListClassMember                   NO-UNDO .
        DEFINE VARIABLE cAnnotationFile                    AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE i                                  AS INTEGER                           NO-UNDO .
        DEFINE VARIABLE lFound                             AS LOGICAL                           NO-UNDO .
        DEFINE VARIABLE lcBuildFiles                       AS LONGCHAR                          NO-UNDO .

        ObjectAssert:IsValid (poParameter, "poParameter":U) .

        FIX-CODEPAGE (lcAnnotations) = Codepages:UTF-8 .

        FileHelper:GetFileList (poParameter:Directory,
                                poParameter:FileMask,
                                OUTPUT TABLE ttFileNames BY-REFERENCE) .

        IF poParameter:BuildFiles > "":U THEN
            COPY-LOB FROM FILE poParameter:BuildFiles TO lcBuildFiles .

        lcBuildFiles = TRIM (lcBuildFiles) .

        filename: FOR EACH ttFileNames ON ERROR UNDO, THROW:

            ASSIGN cFileName = FileHelper:FindFile(ttFileNames.FileName) .

            IF lcBuildFiles > "":U THEN DO:

                ASSIGN lFound = FALSE .

                buildfiles: DO i = 1 TO NUM-ENTRIES (lcBuildFiles, " ":U):
                    IF ENTRY (i, lcBuildFiles, " ":U) = cFileName THEN DO:
                        ASSIGN lFound = TRUE .
                        LEAVE buildfiles .
                    END.
                END.

                IF lFound = FALSE THEN
                    NEXT filename .
            END.

            IF poParameter:Verbose THEN
                MESSAGE "[ExtractClassParameter] Processing:":U cFileName .

            DO ON ERROR UNDO, THROW:

            ASSIGN cClassName = ClassHelper:FileNameToClassName (ttFileNames.FileName)
                   oContainer = NEW ClassAnnotationContainer (cClassName) .

                CATCH plerr AS Progress.Lang.Error:
                    MESSAGE ErrorHelper:FormattedErrorMessages (plerr) .
                    NEXT filename .
                END CATCH.
            END.

            DO ON ERROR UNDO, THROW:
                oParser = NEW ClassAnnotationParser (ttFileNames.FileName) .

                CATCH err AS System.Exception:
                    IF SESSION:BATCH-MODE THEN
                        MESSAGE ErrorHelper:FormattedErrorMessages (err) .

                    LogManager:WriteError (err) .
                    NEXT filename.
                END CATCH.
            END.

            oList = oParser:GetClassAnnotations () .

            THIS-OBJECT:CopyAnnotations (oList,
                                         oContainer:Annotations,
                                         poParameter:ExcludeAnnotations) .

            oMemberList = oParser:GetClassMemberAnnotations() .

            {Consultingwerk/foreachABL.i ClassMember oMember in oMemberList}

                oSerializableClassMemberAnnotation = NEW SerializableClassMemberAnnotation () .

                oSerializableClassMemberAnnotation:Name = oMember:Name  .

                THIS-OBJECT:CopyAnnotations (oMember:Annotations,
                                             oSerializableClassMemberAnnotation:Annotations,
                                             poParameter:ExcludeAnnotations) .

                IF oSerializableClassMemberAnnotation:Annotations:Count > 0 THEN

                    CASE oMember:MemberType:
                        WHEN ClassMemberTypeEnum:Constructor THEN
                            oContainer:Constructors:Add (oSerializableClassMemberAnnotation) .
                        WHEN ClassMemberTypeEnum:Event THEN
                            oContainer:Events:Add (oSerializableClassMemberAnnotation) .
                        WHEN ClassMemberTypeEnum:Method THEN
                            oContainer:Methods:Add (oSerializableClassMemberAnnotation) .
                        WHEN ClassMemberTypeEnum:Property THEN
                            oContainer:Properties:Add (oSerializableClassMemberAnnotation) .
                        WHEN ClassMemberTypeEnum:Destructor THEN
                            oContainer:Destructor = oSerializableClassMemberAnnotation .
                        WHEN ClassMemberTypeEnum:TempTable THEN
                            oContainer:TempTables:Add (oSerializableClassMemberAnnotation) .
                    END CASE.
            END.

            ASSIGN cAnnotationFile = FileHelper:ReplaceFileExtension (ttFileNames.FileName,
                                                                      "annotations":U) .

            FILE-INFORMATION:FILE-NAME = cAnnotationFile .

            /* Remove write protection */
            IF FILE-INFORMATION:FILE-TYPE MATCHES "*R*":U THEN
                System.IO.File:SetAttributes (FileHelper:FindFile (cAnnotationFile),
                                              FileAttributes:Normal) .

            IF oContainer:Annotations:Count > 0 OR
               oContainer:Constructors:Count > 0 OR
               oContainer:Events:Count > 0 OR
               oContainer:Methods:Count > 0 OR
               oContainer:Properties:Count > 0 OR
               oContainer:TempTables:Count > 0 OR
               VALID-OBJECT (oContainer:Destructor) THEN DO:

                lcAnnotations = oContainer:Serialize() .

                /* SCL-1507 - compare JSON documents, excluding the time stamp */
                IF poParameter:OnlyWriteModified = TRUE AND FileHelper:Exists (cAnnotationFile) THEN DO:

                    IF THIS-OBJECT:OnlyDifferByTimeStamp (cAnnotationFile, lcAnnotations) THEN DO:
                        IF poParameter:Verbose THEN
                            MESSAGE "[ExtractClassParameter] No change detected":U .

                        NEXT filename .
                    END.
                END.

                COPY-LOB FROM lcAnnotations TO FILE cAnnotationFile.
            END.
            ELSE
                IF FileHelper:Exists (cAnnotationFile) THEN
                    OS-DELETE VALUE (FileHelper:FindFile(cAnnotationFile)) .
        END.

    END METHOD .

END CLASS.
