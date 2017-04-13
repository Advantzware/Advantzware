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
    File        : ProparseNodeTypesException
    Purpose     : Exception thrown by the ProparseNodeTypes class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 23 07:25:01 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                FROM PROPATH .
USING Consultingwerk.Studio.Proparse.NodeTypes.* FROM PROPATH .  
USING Progress.Lang.*                            FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Studio.Proparse.NodeTypes.ProparseNodeTypesException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ProparseNodeTypesException  class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProparseNodeTypesException (poInnerException AS Progress.Lang.Error, 
                                                   pcErrorString AS CHARACTER, 
                                                   piMessageNumber AS INTEGER):
                                               
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ProparseNodeTypesException  class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProparseNodeTypesException (pcErrorString AS CHARACTER, 
                                                   piMessageNumber AS INTEGER):
                                               
        SUPER (pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ProparseNodeTypesException  class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProparseNodeTypesException ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
