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
    File        : ListAnnotation
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 27 22:33:31 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.Studio.*         FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING org.prorefactor.core.JPNode     FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Studio.ListAnnotation
    INHERITS GenericList:

    { Consultingwerk/Framework/Base/GenericList.i Annotation }

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Adds a new Annotation to the List
        Notes:
        @param pcFileName The file name containing the annotation
        @param piLine The line number of the Annotation
        @param piColumn The column number of the Annotation
        @param poNode The reference to the JPNode for the annotation
        @return The new Annotation that was added to the List
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Annotation Add (pcFileName AS CHARACTER,
                                   piLine AS INTEGER,
                                  piColumn AS INTEGER,
                                  poNode AS JPNode):

        RETURN THIS-OBJECT:Add (NEW Annotation (pcFileName,
                                                piLine,
                                                piColumn,
                                                poNode)) .

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Adds a new Annotation to the List
        Notes:
        @param pcFileName The file name containing the annotation
        @param pcName The name of the Annotation (including @)
        @param pcParameter The parameter list of the annotation (typically in parenthesis)
        @param piLine The line number of the Annotation
        @param piColumn The column number of the Annotation
        @return The new Annotation that was added to the List
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Annotation Add (pcFileName AS CHARACTER,
                                  pcName AS CHARACTER,
                                  pcParameter AS CHARACTER,
                                  piLine AS INTEGER,
                                  piColumn AS INTEGER):

        RETURN THIS-OBJECT:Add (NEW Annotation (pcFileName,
                                                pcName,
                                                pcParameter,
                                                piLine,
                                                piColumn)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Finds an Annotation in the List by it's Name
        Notes:   Performs a FIND-FIRST, does not verify if there is a second Annotation
                 with the same name
        @param pcName The Name value to search
        @return The matching Annotation or ? when no annotation has been found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Annotation FindByName (pcName AS CHARACTER):

        {Consultingwerk/foreachABL.i Annotation oAnnotation in THIS-OBJECT}

             IF oAnnotation:Name = pcName THEN
                RETURN oAnnotation .
        END.

        RETURN ? .
    END METHOD .

END CLASS.
