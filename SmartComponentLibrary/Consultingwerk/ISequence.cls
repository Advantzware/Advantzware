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
    File        : ISequence
    Purpose     : Interface for Sequence style classes
    Syntax      : 
    Description : Provides simple counter functionality with ability to 
                  increment and return a value in a single method call
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 30 23:30:33 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

INTERFACE Consultingwerk.ISequence:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the current sequence value
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CurrentValue AS INT64 NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Increments or decrements the sequence and returns the next value
        Notes:   
        @return The new sequence value
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC INT64 NextValue ().

    /*------------------------------------------------------------------------------
        Purpose: Resets the sequence to its initial value
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Reset ().

END INTERFACE.
