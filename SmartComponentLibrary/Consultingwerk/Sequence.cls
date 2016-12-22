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
    File        : Sequence
    Purpose     : Default ISequence implementation
    Syntax      : 
    Description : Provides simple counter functionality with ability to 
                  increment and return a value in a single method call
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 30 23:38:10 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH .  
USING Consultingwerk.Assertion.* FROM PROPATH . 
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Sequence 
    IMPLEMENTS ISequence: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the current sequence value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CurrentValue AS INT64 INIT 0 NO-UNDO 
    GET.
    PRIVATE SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the sequence increment 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Increment AS INTEGER INITIAL 1 NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Retusn the sequence initial value 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Initial AS INT64 INITIAL 0 NO-UNDO 
	GET.
	PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Sequence class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC Sequence ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Sequence class
        Notes:   
        @param piInitialValue The initial value to use for the sequence
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Sequence (piInitialValue AS INT64):
        THIS-OBJECT (piInitialValue, 1).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Sequence class
        Notes:   
        @param piInitialValue The initial value to use for the sequence
        @param piIncrement The increment to use for the sequence (positive, negative)
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Sequence (piInitialValue AS INT64,
                                 piIncrement AS INTEGER):
        SUPER () .
        
        Assert:NotNullOrZero (piIncrement) .
        
        IF piInitialValue = ? THEN 
            ASSIGN piInitialValue = 0 .        
        
        ASSIGN THIS-OBJECT:Initial   = piInitialValue
               THIS-OBJECT:Increment = piIncrement . 

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Increments or decrements the sequence and returns the next value
        Notes:   
        @return The new sequence value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INT64 NextValue ():
        
        ASSIGN THIS-OBJECT:CurrentValue = THIS-OBJECT:CurrentValue +
                                          THIS-OBJECT:Increment . 
    
        RETURN THIS-OBJECT:CurrentValue .
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Resets the sequence to its initial value
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Reset ():

        ASSIGN THIS-OBJECT:CurrentValue = THIS-OBJECT:Initial .

    END METHOD . 
    
END CLASS.
