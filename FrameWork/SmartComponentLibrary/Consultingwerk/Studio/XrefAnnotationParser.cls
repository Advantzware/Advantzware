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
    File        : XrefAnnotationParser
    Purpose     : XREF based parser for annotations
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 20 20:45:02 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.Assertion.*       FROM PROPATH .
USING Consultingwerk.Studio.*          FROM PROPATH .
USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.Studio.XrefAnnotationParser:

    {Consultingwerk/Studio/Xref/dsXmlXref.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the XrefAnnotationParser class
        Notes:
        @param pcFileName The file name to parse
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC XrefAnnotationParser (pcFileName AS CHARACTER):
        SUPER ().

        CompilerHelper:GenerateXmlXref (pcFileName,
                                        OUTPUT DATASET dsXmlXref BY-REFERENCE) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Locates Annotations with the specified name
        Notes:
        @param pcName The name of the Annotation to locate
        @return The ListAnnotation with the list of the annotations
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListAnnotation FindAnnotations (pcName AS CHARACTER):

        DEFINE VARIABLE oList      AS ListAnnotation NO-UNDO .
        DEFINE VARIABLE iIndex     AS INTEGER        NO-UNDO .
        DEFINE VARIABLE cName      AS CHARACTER      NO-UNDO .
        DEFINE VARIABLE cParameter AS CHARACTER      NO-UNDO .

        Assert:NotNullOrEmpty (pcName) .

        IF pcName BEGINS "@":U THEN
            ASSIGN pcName = SUBSTRING (pcName, 2, -1, "CHARACTER":U) .

        oList = NEW ListAnnotation () .

        FOR EACH eReference WHERE eReference.ReferenceType  = "ANNOTATION":U BY RefSeq ON ERROR UNDO, THROW:

            ASSIGN iIndex = INDEX (eReference.ObjectIdentifier, "(":U) .

            IF iIndex > 0 THEN
                ASSIGN cName      = TRIM (SUBSTRING (eReference.ObjectIdentifier, 1, iIndex - 1, "CHARACTER":U))
                       cParameter = SUBSTRING (eReference.ObjectIdentifier, iIndex, -1, "CHARACTER":U) .
            ELSE
                ASSIGN cName      = TRIM (eReference.ObjectIdentifier)
                       cParameter = "":U .

            IF cName <> pcName THEN
                NEXT .

            FIND FIRST eSource WHERE eSource.FileNum = eReference.FileNum .

            oList:Add (eSource.FileName,
                       cName,
                       cParameter,
                       eReference.LineNum,
                       1) .
        END.

        RETURN oList .

    END METHOD .

END CLASS.
