/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : HandleValidationErrorEventArgs
    Purpose     : Event argument for the 
    Syntax      : 
    Description : The Error (ValidationException) provides also a reference 
                  to the RecordIdentifier (Buffer Name and ROWID)
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Wed Sep 03 22:15:24 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                FROM PROPATH . 
USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.HandleValidationErrorEventArgs
    INHERITS EventArgs: 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the validation error 
        Notes:   The Error contains a reference to the RecordIdentifier structure
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Error AS ValidationException NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the erorr associated with this event was handled 
                 by an event subscribed
        Notes:   When set to True the SmartBusinessEntityAdapter will not further 
                 handle the error itself
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Handled AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the HandleValidationErrorEventArgs class
	    Notes:
	    @param poError The reference to the ValidationException structure
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC HandleValidationErrorEventArgs (poError AS ValidationException):
		SUPER ().
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poError """poError"":U"} .
		
		THIS-OBJECT:Error = poError . 
		
	END CONSTRUCTOR.

END CLASS.