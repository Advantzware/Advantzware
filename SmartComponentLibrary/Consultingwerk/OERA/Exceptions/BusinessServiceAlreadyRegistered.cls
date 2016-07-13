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
    File        : BusinessServiceAlreadyRegistered
    Purpose     : Exception thrown when the Business Service is already 
                  registered
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 30 00:00:08 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.BusinessServiceAlreadyRegistered 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Service that is already registered
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY BusinessServiceName AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessServiceAlreadyRegistered class
        Notes:   
        @param pcServiceName The name of the Business Service that is already registered
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BusinessServiceAlreadyRegistered (pcServiceName AS CHARACTER):
		
		SUPER (SUBSTITUTE("The service name &1 is already regsitered with the ServiceManager."{&TRAN},
                          pcServiceName), 
               0).
		
		THIS-OBJECT:BusinessServiceName = pcServiceName .
		
	END CONSTRUCTOR.

END CLASS.
