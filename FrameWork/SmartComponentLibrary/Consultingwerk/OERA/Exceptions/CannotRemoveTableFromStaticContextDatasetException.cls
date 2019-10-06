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
    File        : CannotRemoveTableFromStaticContextDatasetException
    Purpose     : Exception thrown when trying to remove a temp-table 
                  from a static Context Dataset
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 02 13:01:49 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.Exceptions.CannotRemoveTableFromStaticContextDatasetException 
    INHERITS Exception
    {&SERIALIZABLE}:  
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CannotRemoveTableFromStaticContextDatasetException 
                 class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CannotRemoveTableFromStaticContextDatasetException ():
        SUPER ("Unable to remove temp-table from static context dataset!"{&TRAN}, 0).
        
    END CONSTRUCTOR.

END CLASS.
