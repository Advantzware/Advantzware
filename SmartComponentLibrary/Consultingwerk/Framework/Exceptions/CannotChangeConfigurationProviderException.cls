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
    File        : CannotChangeConfigurationProviderException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 30 10:40:14 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Exceptions.* FROM PROPATH . 
USING Consultingwerk.Exceptions.*           FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Framework.Exceptions.CannotChangeConfigurationProviderException 
    INHERITS Exception
    {&SERIALIZABLE}:  

	/*------------------------------------------------------------------------------
		Purpose: Constructor for the CannotChangeConfigurationProviderException class  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CannotChangeConfigurationProviderException ():
		SUPER ("Cannot modify the ConfigurationProvider settings once it has been set."{&TRAN}, 0).
		
	END CONSTRUCTOR.

END CLASS.