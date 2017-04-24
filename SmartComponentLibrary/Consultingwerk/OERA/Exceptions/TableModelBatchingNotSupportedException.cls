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
    File        : DatasetModelBatchingNotSupportedException
    Purpose     : Exception thrown by the DatasetModelPerformer when calling
                  into the PerformNextBatchRequest while batching is not 
                  expected 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 14 05:49:36 CEST 2015
    Notes       : Typically thrown when the previous call did not return 
                  a NextContext
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.Exceptions.TableModelBatchingNotSupportedException 
    INHERITS DatasetModelException
    {&SERIALIZABLE}: 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC TableModelBatchingNotSupportedException ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC TableModelBatchingNotSupportedException (pcErrorString AS CHARACTER, 
                                                                piMessageNumber AS INTEGER):
        
        SUPER (INPUT pcErrorString, INPUT piMessageNumber).
        
    END CONSTRUCTOR.
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TableModelBatchingNotSupportedException (pcErrorString AS CHARACTER):
        
        SUPER (INPUT pcErrorString).
        
    END CONSTRUCTOR.

END CLASS.
