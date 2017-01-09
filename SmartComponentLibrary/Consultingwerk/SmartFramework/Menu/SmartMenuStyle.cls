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
    File        : SmartMenuStyle
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 09.08.2013 08:55:32 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.SmartMenuStyle
    INHERITS SmartMenuStyle_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartMenuStyle class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuStyle ():
    
        SUPER () .
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartMenuStyle class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuStyle (phBuffer AS HANDLE):
    
        SUPER (phBuffer) .
    
    END CONSTRUCTOR .
    
END CLASS.