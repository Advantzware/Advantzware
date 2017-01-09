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
    File        : UnmatchedBracketException
    Purpose     : Exception thrown from the StringHelper:FindClosingBracket()
                  method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 29 12:40:14 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.UnmatchedBracketException 
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the Bracket associated with the Exception 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Bracket AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Source String associated with the Exception 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SourceString AS LONGCHAR NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Starting Position associated with the Exception 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StartingPosition AS INTEGER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UnmatchedQuoteException class
        Notes:   
        @param pcSourceString The source string that should contain a closing quote
        @param pcBracket The character representing the current bracket type 
        @param piStartingPosition The starting position, which is the position of the opening quote
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnmatchedBracketException (pcSourceString AS LONGCHAR,
	                                              pcBracket AS CHARACTER, 
	                                              piStartingPosition AS INTEGER):
		
		SUPER (SUBSTITUTE ("The Character value ~"&1~" does not contain a closing bracket for &2 at position &3"{&TRAN},
		                   pcSourceString,
		                   pcBracket,
		                   piStartingPosition), 0).
		
		ASSIGN THIS-OBJECT:SourceString     = pcSourceString
		       THIS-OBJECT:Bracket          = pcBracket  
		       THIS-OBJECT:StartingPosition = piStartingPosition .
		
	END CONSTRUCTOR.

END CLASS.
