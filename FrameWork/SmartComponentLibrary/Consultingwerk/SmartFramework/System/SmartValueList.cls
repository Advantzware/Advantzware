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
    File        : SmartValueList
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 08.03.2013 15:44:40 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartValueList
    INHERITS SmartValueList_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartValueList class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartValueList ():
    
        SUPER () .
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartValueList class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartValueList (phBuffer AS HANDLE):
    
        SUPER (phBuffer) .
    
    END CONSTRUCTOR .
    
END CLASS.