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
    File        : FilterStringResourceEventArgs
    Purpose     : Event argument class for the FilterStringResource event 
                  of the IUserInterfaceTranslationService interface
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 11 17:14:51 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                             FROM PROPATH .
USING Consultingwerk.SmartFramework.Localization.* FROM PROPATH .  
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Localization.FilterStringResourceEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Allows filtering out a resource by setting the value of this 
                 property to TRUE
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filtered AS LOGICAL NO-UNDO INIT ?  
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the original string
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OriginalString AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the resource key
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ResourceKey AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FilterStringResourceEventArgs class
        Notes:   
        @param pcResourceKey The resource key
        @param pcOriginalString The original string
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FilterStringResourceEventArgs (pcResourceKey AS CHARACTER,
	                                                  pcOriginalString AS CHARACTER):
		SUPER ().
		
		ASSIGN THIS-OBJECT:ResourceKey    = pcResourceKey
		       THIS-OBJECT:OriginalString = pcOriginalString .       
		
	END CONSTRUCTOR.

END CLASS.
