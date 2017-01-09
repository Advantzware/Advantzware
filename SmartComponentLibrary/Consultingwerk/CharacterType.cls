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
    File        : CharacterType
    Purpose     : Contains supporting routines for ABL CHARACTER Variables
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Sat Jun 02 10:58:12 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.CharacterType: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE CharacterType ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the passed in character string is not unknown value
                 or empty 
        Notes:   
        @param pcCharacter The character to compare
        @return True when the passed in character value is not ? or Empty 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsNotNullOrEmpty (pcCharacter AS CHARACTER):
        
        RETURN pcCharacter > "":U.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the passed in character string is the unknown value
                 or empty 
        Notes:   
        @param pcCharacter The character to compare
        @return True when the passed in character value is ? or Empty 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsNullOrEmpty (pcCharacter AS CHARACTER):
        
        RETURN pcCharacter = ? OR pcCharacter = "":U.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a String for unknown value, returning ? for "?"
        Notes:   
        @param pcValue The character string to parse
        @return The character value of ? when the string was "?"
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ParseUnknownValue (pcValue AS CHARACTER):
            
        IF pcValue = "?":U THEN 
            RETURN ? . 
        ELSE 
            RETURN pcValue . 

    END METHOD .

END CLASS.
