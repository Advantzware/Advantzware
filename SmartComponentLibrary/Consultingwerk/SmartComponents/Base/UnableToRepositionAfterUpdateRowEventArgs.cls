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
    File        : UnableToRepositionAfterUpdateRowEventArgs
    Purpose     : Argument for the UnableToRepositionAfterUpdateRow event 
                  of the SmartDatasetAdapter. Allows customized handling of 
                  errors when the updated row cannot be repositioned to 
                  after update (e.g. open the query with different arguments)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 01 11:55:23 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.UnableToRepositionAfterUpdateRowEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the query   
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the event was handled by an subscriber and the 
                 error should not be shown to the user
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Handled AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the ROWID that was attempted to reposition to 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Rowid AS ROWID NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UnableToRepositionAfterUpdateRowEventArgs class
        Notes:   
        @param phQuery The handle of the QUERY
        @param proRowid The ROWID of the record that was attempted to reposition to
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToRepositionAfterUpdateRowEventArgs (phQuery AS HANDLE,
	                                                              proRowid AS ROWID):
		SUPER ().
		
		ASSIGN THIS-OBJECT:QueryHandle = phQuery
		       THIS-OBJECT:Rowid       = proRowid . 
		
	END CONSTRUCTOR.

END CLASS.
