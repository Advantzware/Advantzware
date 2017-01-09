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
    File        : CaseWhen
    Purpose     : Value class for the result of the CaseStatementParser
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 28 10:58:57 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
USING Consultingwerk.Studio.*     FROM PROPATH .  
USING org.prorefactor.core.JPNode FROM ASSEMBLY .
USING Progress.Lang.*             FROM PROPATH .
&ENDIF

CLASS Consultingwerk.Studio.CaseWhen: 

    /*------------------------------------------------------------------------------
        Purpose: The Column number containing the Annotation 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Column AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the expression that follows the WHEN keyword  
        Notes:   A QSTRING Node
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Expression AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: The FileName containing the Annotation 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Line number containing the Annotation 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Line AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    /*------------------------------------------------------------------------------
        Purpose: The reference to the JPNode of the Annotation 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Node AS JPNode NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CaseWhen class
        Notes:   
        @param pcExpression The expression following the WHEN keyword
        @param pcFileName The file name containing the annotation
        @param piLine The line number of the Annotation
        @param piColumn The column number of the Annotation
        @param poNode The reference to the JPNode for the annotation 
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CaseWhen (pcExpression AS CHARACTER, 
	                             pcFileName AS CHARACTER,
                                 piLine AS INTEGER,
                                 piColumn AS INTEGER,
                                 poNode AS JPNode):
		SUPER ().
		
        ASSIGN THIS-OBJECT:Expression = pcExpression
               THIS-OBJECT:FileName   = pcFileName
               THIS-OBJECT:Line       = piLine
               THIS-OBJECT:Column     = piColumn 
               THIS-OBJECT:Node       = poNode . 
		
	END CONSTRUCTOR.
&ENDIF
END CLASS.
