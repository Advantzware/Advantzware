/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : QueryStringForKeyValueTableException
    Purpose     : Exception thrown by the DataAccess:FetchDataByKeyTable
                  method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 08 07:41:32 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.QueryStringForKeyValueTableException 
    INHERITS Exception 
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the QueryStringForKeyValueTableException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC QueryStringForKeyValueTableException ( INPUT pcErrorString AS CHARACTER, INPUT piMessageNumber AS INTEGER ):
		SUPER (INPUT pcErrorString, INPUT piMessageNumber).
		
	END CONSTRUCTOR.

END CLASS.
