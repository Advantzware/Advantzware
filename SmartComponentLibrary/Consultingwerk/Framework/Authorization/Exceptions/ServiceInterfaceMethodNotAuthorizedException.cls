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
    File        : ServiceInterfaceMethodNotAuthorizedException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 08 22:59:37 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Authorization.Exceptions.* FROM PROPATH . 
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Framework.Authorization.Exceptions.ServiceInterfaceMethodNotAuthorizedException 
    INHERITS NotAuthorizedException
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the Name of the Method
        Notes:                                              
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ServiceMethodName AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Name of the Business Entity or Task Class 
        Notes:                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ServiceObjectName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ServiceInterfaceMethodNotAuthorizedException Class 
        Notes:                                              
        @param pcServiceObjectName The name of the object that the user is not authorized to use
        @param pcServiceMethodName The name of the method that the user is not authorized to use
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ServiceInterfaceMethodNotAuthorizedException (pcServiceObjectName AS CHARACTER,
	                                                                 pcServiceMethodName AS CHARACTER):
		SUPER (SUBSTITUTE ("You are not authorized to invoke &2 on &1."{&TRAN}, 
		                   pcServiceObjectName, 
		                   pcServiceMethodName), 
		       0).
		
		ASSIGN THIS-OBJECT:ServiceObjectName = pcServiceObjectName 
		       THIS-OBJECT:ServiceMethodName = pcServiceMethodName .
		
	END CONSTRUCTOR.

END CLASS.
