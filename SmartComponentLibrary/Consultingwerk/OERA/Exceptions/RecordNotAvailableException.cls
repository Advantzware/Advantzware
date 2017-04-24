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
    File        : RecordNotAvailableException
    Purpose     : Exception thrown by the SmartTableInfoProvider:FindRecordByKeyValues
                  method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 07 22:04:28 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.Exception.


USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.Exceptions.RecordNotAvailableException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RecordNotAvailableException class
        Notes:                          
        @param pcErrorString The error message associated with this RecordNotAvailableException object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RecordNotAvailableException (pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
           
END CLASS.
