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
    File        : IContainerControlGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 24 00:01:29 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.IContainerControlGenerator:  
  
	/*------------------------------------------------------------------------------
	    Purpose: Creates a method in the container control
	    Notes:
	    @param pcMethodName The name of the method to create
	    @param pcCode The source code of the method
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID CreateMethod (pcMethodName AS CHARACTER,
	                                 pcCode AS LONGCHAR).

    /*------------------------------------------------------------------------------
        Purpose: Generates a temporary object name (e.g. appearance1) based on a 
                 provoded name (e.g. appearance)
        Notes:   
        @param pcObjectName The name to be used for a temporary object
        @return The name to be used for a temporary object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateObjectName (pcObjectName AS CHARACTER).
  
END INTERFACE.