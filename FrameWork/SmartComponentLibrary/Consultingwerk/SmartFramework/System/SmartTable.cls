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
    File        : SmartTable
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 27.11.2012 23:45:01 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartTable
    INHERITS SmartTable_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartTable class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTable ():
    
        SUPER () .
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartTable class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTable (phBuffer AS HANDLE):
    
        SUPER (phBuffer) .
    
    END CONSTRUCTOR .
    
END CLASS.