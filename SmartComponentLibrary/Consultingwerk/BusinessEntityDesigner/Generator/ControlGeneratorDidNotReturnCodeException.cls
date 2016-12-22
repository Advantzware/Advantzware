/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ControlGeneratorDidNotReturnCodeException
    Purpose     : Exception thrown by the ContainerControlGenerator when a
                  contained Control generator did not return source code, 
                  that is, when ? was returned or "" was returned from a 
                  routine that needs to return a value 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 24 17:11:38 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.ControlGeneratorDidNotReturnCodeException 
    INHERITS ControlGeneratorException
    {&SERIALIZABLE}:   

    /*------------------------------------------------------------------------------
        Purpose: Returns the type name fo the Control Generator 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ControlGeneratorType AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the current control
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ControlName AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the method that returned no (valid) source code 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY MethodName AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ControlGeneratorDidNotReturnCodeException class
        Notes:   
        @param pcControlName The name of the current control
        @param pcControlGeneratorType The type name of the Control generator
        @param pcMethodName The name of the method that returned no valid source code
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ControlGeneratorDidNotReturnCodeException (pcControlName AS CHARACTER,
	                                                              pcControlGeneratorType AS CHARACTER,
	                                                              pcMethodName AS CHARACTER):
	                                                                  
		SUPER (SUBSTITUTE ("The method &1 of &2 did not return source code for control &3."{&TRAN},
		                   pcMethodName,
		                   pcControlGeneratorType,
		                   pcControlName), 0).
		
		ASSIGN THIS-OBJECT:ControlName          = pcControlName
		       THIS-OBJECT:ControlGeneratorType = pcControlGeneratorType 
		       THIS-OBJECT:MethodName           = pcMethodName . 
		
	END CONSTRUCTOR.

END CLASS.
