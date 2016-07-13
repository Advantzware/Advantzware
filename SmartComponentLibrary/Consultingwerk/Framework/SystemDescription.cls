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
    File        : SystemDescription
    Purpose     : Describes the current system (e.g. Dev, Test, Prod)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 26 07:57:18 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.ISystemDescription.


USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Framework.SystemDescription IMPLEMENTS ISystemDescription: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the description of the current system 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Description AS CHARACTER NO-UNDO 
	GET.
	SET.
	 
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SystemDescription class
        Notes:   
        @param pcDescription The description of the current system
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SystemDescription (pcDescription AS CHARACTER):
		SUPER ().
		
		THIS-OBJECT:Description = pcDescription . 
		
	END CONSTRUCTOR.

END CLASS.
