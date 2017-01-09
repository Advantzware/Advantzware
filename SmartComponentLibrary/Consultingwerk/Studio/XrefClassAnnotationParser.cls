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
    File        : XrefClassAnnotationParser
    Purpose     : Parser for class and class member annotations
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 20 20:45:02 CET 2013
    Notes       : This class uses proparse and is thuse dependent on
                  windows / the .NET framework
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.Studio.*          FROM PROPATH .
USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.Studio.XrefClassAnnotationParser
    IMPLEMENTS IClassAnnotationParser:

    {Consultingwerk/Studio/Xref/dsXmlXref.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassAnnotationParser class
        Notes:
        @param pcFileName The file name to parse
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC XrefClassAnnotationParser (pcFileName AS CHARACTER):
        SUPER ().

        CompilerHelper:GenerateXmlXref (pcFileName,
                                        OUTPUT DATASET dsXmlXref BY-REFERENCE) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds an Array of Annotations to the ClassMember
        Notes:
        @param poMember The ClassMember instance to add the annotations to
        @param poAnnotations The Annotation Array to add to the ClassMember
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddAnnotations (poMember AS ClassMember,
                                          poAnnotations AS Annotation EXTENT):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (poAnnotations):
            IF VALID-OBJECT (poAnnotations[i]) THEN
                poMember:Annotations:Add (poAnnotations[i]) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets the List of Annotations of the ClassHeader
        Notes:
        @return The ListAnnotation with the list of annotations from the class header
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListAnnotation GetClassAnnotations ():

        DEFINE VARIABLE oList      AS ListAnnotation NO-UNDO .
        DEFINE VARIABLE cName      AS CHARACTER      NO-UNDO .
        DEFINE VARIABLE cParameter AS CHARACTER      NO-UNDO .
        DEFINE VARIABLE iIndex     AS INTEGER        NO-UNDO .

        oList = NEW ListAnnotation () .

        FOR EACH eReference BY RefSeq:

            IF eReference.ReferenceType = "ANNOTATION":U THEN DO:

                ASSIGN iIndex = INDEX (eReference.ObjectIdentifier, "(":U) .

                IF iIndex > 0 THEN
                    ASSIGN cName      = SUBSTRING (eReference.ObjectIdentifier, 1, iIndex - 1, "CHARACTER":U)
                           cParameter = SUBSTRING (eReference.ObjectIdentifier, iIndex, -1, "CHARACTER":U) .
                ELSE
                    ASSIGN cName      = eReference.ObjectIdentifier
                           cParameter = "":U .

                FIND FIRST eSource WHERE eSource.FileNum = eReference.FileNum .

                oList:Add (eSource.FileName,
                           cName,
                           cParameter,
                           eReference.LineNum,
                           1) .
            END.

            IF eReference.ReferenceType = "CLASS":U THEN
                LEAVE .
        END.

        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets the List of Class Members and their Annotations
        Notes:
        @return The ListClassMember with the lsit of class members and their annotations
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListClassMember GetClassMemberAnnotations ():

        DEFINE VARIABLE oList          AS ListClassMember NO-UNDO .
        DEFINE VARIABLE oAnnotations   AS Annotation      NO-UNDO EXTENT 20 .
        DEFINE VARIABLE iAnnotation    AS INTEGER         NO-UNDO INIT 0 .
        DEFINE VARIABLE oMember        AS ClassMember     NO-UNDO  .
        DEFINE VARIABLE cName          AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cParameter     AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE iIndex         AS INTEGER         NO-UNDO .

        DEFINE VARIABLE iStartSequence AS INTEGER         NO-UNDO .

        /* Throw on failure */
        FIND FIRST eReference WHERE eReference.ReferenceType = "CLASS":U .

        ASSIGN iStartSequence = eReference.RefSeq .

        oList = NEW ListClassMember () .

        FOR EACH eReference WHERE eReference.RefSeq > iStartSequence:

            IF eReference.ReferenceType = "ANNOTATION":U THEN DO:

                ASSIGN iIndex = INDEX (eReference.ObjectIdentifier, "(":U) .

                IF iIndex > 0 THEN
                    ASSIGN cName      = SUBSTRING (eReference.ObjectIdentifier, 1, iIndex - 1, "CHARACTER":U)
                           cParameter = SUBSTRING (eReference.ObjectIdentifier, iIndex, -1, "CHARACTER":U) .
                ELSE
                    ASSIGN cName      = eReference.ObjectIdentifier
                           cParameter = "":U .

                FIND FIRST eSource WHERE eSource.FileNum = eReference.FileNum .

                ASSIGN iAnnotation = iAnnotation + 1
                       oAnnotations [iAnnotation] = NEW Annotation (eSource.FileName,
                                                                    cName,
                                                                    cParameter,
                                                                    eReference.LineNum,
                                                                    1) .
            END.

            ELSE IF iAnnotation > 0 AND eReference.ReferenceType = "CONSTRUCTOR":U THEN DO:

                oMember = NEW ClassMember () .

                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Constructor
                       oMember:Name       = eReference.ObjectIdentifier
                       oMember:Public     = (eReference.AccessMode = "PUBLIC":U)
                       oMember:Protected  = (eReference.AccessMode = "PROTECTED":U)
                       oMember:Private    = (eReference.AccessMode = "PRIVATE":U)
                       oMember:Static     = eReference.IsStatic
                       oMember:Parameters = THIS-OBJECT:ParseParameterList (eReference.RefSeq).

                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .

                ASSIGN oAnnotations = ?
                       iAnnotation  = 0 .

                oList:Add (oMember) .
            END.
            ELSE IF iAnnotation > 0 AND eReference.ReferenceType = "METHOD":U THEN DO:

                oMember = NEW ClassMember () .

                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Method
                       oMember:Name       = eReference.ObjectIdentifier
                       oMember:Public     = (eReference.AccessMode = "PUBLIC":U)
                       oMember:Protected  = (eReference.AccessMode = "PROTECTED":U)
                       oMember:Private    = (eReference.AccessMode = "PRIVATE":U)
                       oMember:Static     = eReference.IsStatic
                       oMember:Parameters = THIS-OBJECT:ParseParameterList (eReference.RefSeq).
                       .

                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .

                ASSIGN oAnnotations = ?
                       iAnnotation  = 0 .

                oList:Add (oMember) .
            END.
            ELSE IF iAnnotation > 0 AND eReference.ReferenceType = "PROPERTY":U THEN DO:

                oMember = NEW ClassMember () .

                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Property
                       oMember:Name       = eReference.ObjectIdentifier
                       oMember:Public     = (eReference.AccessMode = "PUBLIC":U)
                       oMember:Protected  = (eReference.AccessMode = "PROTECTED":U)
                       oMember:Private    = (eReference.AccessMode = "PRIVATE":U)
                       oMember:Static     = eReference.IsStatic
                       .

                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .

                ASSIGN oAnnotations = ?
                       iAnnotation  = 0 .

                oList:Add (oMember) .
            END.
            ELSE IF iAnnotation > 0 AND eReference.ReferenceType = "EVENT":U THEN DO:

                oMember = NEW ClassMember () .

                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Event
                       oMember:Name       = eReference.ObjectIdentifier
                       oMember:Public     = (eReference.AccessMode = "PUBLIC":U)
                       oMember:Protected  = (eReference.AccessMode = "PROTECTED":U)
                       oMember:Private    = (eReference.AccessMode = "PRIVATE":U)
                       oMember:Static     = eReference.IsStatic
                       oMember:Parameters = THIS-OBJECT:ParseParameterList (eReference.RefSeq).
                       .

                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .

                ASSIGN oAnnotations = ?
                       iAnnotation  = 0 .

                oList:Add (oMember) .
            END.

        END.

        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the parameter list of an XREF element
        Notes:
        @param piRefSeq The eSequence.RefSeq value of the element to parse
        @return The parameter list as a single string
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ParseParameterList (piRefSeq AS INTEGER):

        DEFINE VARIABLE cList AS CHARACTER NO-UNDO.

        FOR EACH eParameterRef WHERE eParameterRef.RefSeq = piRefSeq:

            ASSIGN cList = cList + SUBSTITUTE (",&1 &2":U,
                                               eParameterRef.ParameterMode,
                                               eParameterRef.ParameterType) .
        END.

        RETURN TRIM (cList, ",":U) .

    END METHOD .

END CLASS.
