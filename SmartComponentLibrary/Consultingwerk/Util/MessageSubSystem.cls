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
    File        : MessageSubSystem
    Purpose     : Holder class for LogManager message sub system
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 05 20:13:39 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.Util.MessageSubSystem INHERITS CharacterHolder: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageSubSystem class
        Notes:   
        @param pcMessageSubSys The value for the Message Sub System
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC MessageSubSystem (pcMessageSubSys AS CHARACTER):
		SUPER (pcMessageSubSys).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a Character representation of the value
        Notes:   
        @return The Character representation of the value
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():
        
        RETURN THIS-OBJECT:Value .

    END METHOD .    

END CLASS.
