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
    File        : ListCaseWhen
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

CLASS Consultingwerk.Studio.ListCaseWhen 
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i CaseWhen }

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Adds a new CaseWhen to the List
        Notes:   
        @param pcExpression The expression following the WHEN keyword
        @param pcFileName The file name containing the annotation
        @param piLine The line number of the Annotation
        @param piColumn The column number of the Annotation
        @param poNode The reference to the JPNode for the annotation 
        @return The new CaseWhen that was added to the List 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CaseWhen Add (pcExpression AS CHARACTER, 
                                pcFileName AS CHARACTER,
                                piLine AS INTEGER,
                                piColumn AS INTEGER,
                                poNode AS JPNode):

        RETURN THIS-OBJECT:Add (NEW CaseWhen (pcExpression,
                                              pcFileName,
                                              piLine,
                                              piColumn,
                                              poNode)) . 
        
    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns if the list contains a CaseWhen option for the given expression 
        Notes:   
        @param pcExpression The Expression to locate
        @return Logical value indicating if the List contains a CaseWhen option for the given expression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Exists (pcExpression AS CHARACTER):
        
        {Consultingwerk/foreachABL.i CaseWhen oEntry in THIS-OBJECT}
            IF oEntry:Expression = pcExpression THEN 
                RETURN TRUE . 
        END.

        RETURN FALSE . 
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the first item from the list for the given expression 
        Notes:   
        @param pcExpression The Expression to locate
        @return The CaseWhen object for the given expression or the unknown value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CaseWhen Find (pcExpression AS CHARACTER):
        
        {Consultingwerk/foreachABL.i CaseWhen oEntry in THIS-OBJECT}
            IF oEntry:Expression = pcExpression THEN 
                RETURN oEntry . 
        END.

        RETURN ? . 
    
    END METHOD .

END CLASS.
