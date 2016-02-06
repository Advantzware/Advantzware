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

    /*------------------------------------------------------------------------------
        Purpose: Extracts class annotations into .annotation files 
        Notes:   
        @param poParameter The ExtractClassAnnotationsParameter with the data for this call
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Run (poParameter AS ExtractClassAnnotationsParameter):
		
        DEFINE VARIABLE oParser                            AS ClassAnnotationParser             NO-UNDO . 
        DEFINE VARIABLE cClassName                         AS CHARACTER                         NO-UNDO.
        DEFINE VARIABLE oContainer                         AS ClassAnnotationContainer          NO-UNDO .
        DEFINE VARIABLE oSerializableClassMemberAnnotation AS SerializableClassMemberAnnotation NO-UNDO .
        DEFINE VARIABLE lcAnnotations                      AS LONGCHAR                          NO-UNDO .
        DEFINE VARIABLE oList                              AS ListAnnotation                    NO-UNDO .
        DEFINE VARIABLE oMemberList                        AS ListClassMember                   NO-UNDO .
        DEFINE VARIABLE cAnnotationFile                    AS CHARACTER                         NO-UNDO .

        ObjectAssert:IsValid (poParameter, "poParameter":U) .
        
        FIX-CODEPAGE (lcAnnotations) = Codepages:UTF-8 .

        FileHelper:GetFileList (poParameter:Directory,
                                poParameter:FileMask,
                                OUTPUT TABLE ttFileNames BY-REFERENCE) .

        filename: FOR EACH ttFileNames ON ERROR UNDO, THROW:
            
            IF poParameter:Verbose THEN 
                MESSAGE "[ExtractClassParameter] Processing:":U ttFileNames.FileName .
            
            ASSIGN cClassName = ClassHelper:FileNameToClassName (ttFileNames.FileName)  
                   oContainer = NEW ClassAnnotationContainer (cClassName) .

            DO ON ERROR UNDO, THROW:         
                oParser = NEW ClassAnnotationParser (ttFileNames.FileName) .

                CATCH err AS System.Exception:
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
                    END CASE.
            END.
        
            ASSIGN cAnnotationFile = FileHelper:ReplaceFileExtension (ttFileNames.FileName,
                                                                      "annotations":U) .
        
            FILE-INFO:FILE-NAME = cAnnotationFile .
            
            /* Remove write protection */
            IF FILE-INFO:FILE-TYPE MATCHES "*R*":U THEN
                System.IO.File:SetAttributes (FileHelper:FindFile (cAnnotationFile),
                                              FileAttributes:Normal) .  
        
            IF oContainer:Annotations:Count > 0 OR 
               oContainer:Constructors:Count > 0 OR 
               oContainer:Events:Count > 0 OR 
               oContainer:Methods:Count > 0 OR 
               oContainer:Properties:Count > 0 OR 
               VALID-OBJECT (oContainer:Destructor) THEN DO:
        
                lcAnnotations = oContainer:Serialize() .
            
                COPY-LOB FROM lcAnnotations TO FILE cAnnotationFile.
            END.
            ELSE 
                IF FileHelper:Exists (cAnnotationFile) THEN 
                    OS-DELETE VALUE (FileHelper:FindFile(cAnnotationFile)) .
        END.

	END METHOD .

END CLASS.
