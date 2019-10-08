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
    File        : MenuFunctionLauncherException
    Purpose     : Exception for errors from the MenuFunctionLauncher
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 27 21:18:07 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*             FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                         FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Menu.MenuFunctionLauncherException 
    INHERITS Exception
    IMPLEMENTS IErrorTitle
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the Title to be used in the Error Dialog or Message Box 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuFunctionLauncherException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuFunctionLauncherException (poInnerException AS Progress.Lang.Error,
                                                      pcErrorString AS CHARACTER):
        
        SUPER (poInnerException, pcErrorString).

        THIS-OBJECT:Title = {Consultingwerk/translate.i &string="'Error':U"} . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuFunctionLauncherException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuFunctionLauncherException (poInnerException AS Progress.Lang.Error, 
                                                      pcErrorString AS CHARACTER, 
                                                      piMessageNumber AS INTEGER):
    
        SUPER (poInnerException, pcErrorString, piMessageNumber).

        THIS-OBJECT:Title = {Consultingwerk/translate.i &string="'Error':U"} . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuFunctionLauncherException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuFunctionLauncherException (poInnerException AS Progress.Lang.Error):
    
        SUPER (poInnerException).

        THIS-OBJECT:Title = {Consultingwerk/translate.i &string="'Error':U"} . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuFunctionLauncherException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuFunctionLauncherException (pcErrorString AS CHARACTER):
    
        SUPER (pcErrorString).

        THIS-OBJECT:Title = {Consultingwerk/translate.i &string="'Error':U"} . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuFunctionLauncherException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuFunctionLauncherException (pcErrorString AS CHARACTER, 
                                                      piMessageNumber AS INTEGER):
                                                          
        SUPER (pcErrorString, piMessageNumber).

        THIS-OBJECT:Title = {Consultingwerk/translate.i &string="'Error':U"} . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuFunctionLauncherException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuFunctionLauncherException ():
    
        SUPER ().

        THIS-OBJECT:Title = {Consultingwerk/translate.i &string="'Error':U"} . 
        
    END CONSTRUCTOR.

END CLASS.
