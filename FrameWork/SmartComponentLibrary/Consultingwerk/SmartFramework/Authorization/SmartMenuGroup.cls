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
    File        : SmartMenuGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 00:37:22 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SmartMenuGroup
    INHERITS SmartMenuGroup_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartMenuGroup class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuGroup ():
    
        SUPER () .
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartMenuGroup class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuGroup (phBuffer AS HANDLE):
    
        SUPER (phBuffer) .
    
    END CONSTRUCTOR .
    
END CLASS.