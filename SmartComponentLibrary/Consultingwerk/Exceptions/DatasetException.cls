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
    File        : DatasetException
    Purpose     : Exception raised by the DatasetHelper:ThrowDatasetErrors
                  method with the ERROR-STRING's of the ProDataset records
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 21 17:23:51 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.DatasetException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatasetException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetException (pcErrorString AS CHARACTER, 
	                                     piMessageNumber AS INTEGER):
	                                         
		SUPER (INPUT pcErrorString, INPUT piMessageNumber).
		
	END CONSTRUCTOR.

END CLASS.
