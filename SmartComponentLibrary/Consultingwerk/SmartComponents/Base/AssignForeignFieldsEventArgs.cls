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
    File        : AssignForeignFieldsEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 12 13:24:40 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*       FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .  

CLASS Consultingwerk.SmartComponents.Base.AssignForeignFieldsEventArgs
    INHERITS Consultingwerk.EventArgs: 


    /*------------------------------------------------------------------------------
        Purpose: Returns the record Buffer used to create the new record
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CreateBuffer AS HANDLE NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the ForeignFields associated with the event
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ForeignFields AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Get and sets if the assignment of the ForeignField values has been 
                 handled by an event subscribed
        Notes:   Assign to TRUE to prevend the child SmartDataAdapter from assigning
                 the key values
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Handled AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartDataSource
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SmartDataSource AS ISmartDataSource NO-UNDO 
	GET.
	PROTECTED SET. 

	/*------------------------------------------------------------------------------
	    Purpose: Constructor of the AssignForeignFieldsEventArgs class
	    Notes:
	    @param phCreateBuffer The handle of the buffer used to create the record
	    @param pcForeignFields The list of ForeignField names
	    @param poSmartDataSource The reference to the SmartDataSource
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AssignForeignFieldsEventArgs (phCreateBuffer AS HANDLE, 
	                                                 pcForeignFields AS CHARACTER,
	                                                 poSmartDataSource AS ISmartDataSource):
		SUPER ().
		
		ASSIGN THIS-OBJECT:CreateBuffer    = phCreateBuffer
		       THIS-OBJECT:ForeignFields   = pcForeignFields
		       THIS-OBJECT:SmartDataSource = poSmartDataSource 
		       .
		
	END CONSTRUCTOR.

END CLASS.
