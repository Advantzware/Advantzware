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
    File        : FindRecordResult
    Purpose     : Result class for the Consultingwerk.OERA.DataAccess FindRecord
                  method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 27 23:19:29 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH . 
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.FindRecordResult: 

    /*------------------------------------------------------------------------------
        Purpose: Returns if a previous batch is available 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PreviousBatchAvailable AS LOGICAL INITIAL TRUE NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Restart ROWID's 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RestartRowids AS IQueryRowIdentifier NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FindRecordResult class
        Notes:   
        @param proRestartRowids The IQueryRowIdentifier instance for this instance
        @param plPreviousBatchAvailable The value for the PreviousBatchAvailable property
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FindRecordResult (proRestartRowids AS IQueryRowIdentifier,
	                                     plPreviousBatchAvailable AS LOGICAL):
		SUPER ().
		
		ASSIGN THIS-OBJECT:RestartRowids          = proRestartRowids
		       THIS-OBJECT:PreviousBatchAvailable = plPreviousBatchAvailable .
		
	END CONSTRUCTOR.

END CLASS.
