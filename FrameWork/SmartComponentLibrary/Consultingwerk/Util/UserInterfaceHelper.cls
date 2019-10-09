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
    File        : UserInterfaceHelper
    Purpose     : Library of generic helper routines for the .NET user 
                  interface 
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 25 13:32:07 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Util.UserInterfaceHelper:
     
    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED UserInterfaceHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs Process Events if -IOEverywhere 1 is set                                                                         
        Notes:   Optional startup parameter starting 10.2B02, default for OE11.0      
                 This method is particilar helpful when compiling without 
                 -IOEverywhere 1 to allow using PROCESS EVENTS in non-void methods                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ProcessEvents ():
        
        IF Consultingwerk.Util.StartupParameterHelper:IOEverywhere = 1 THEN 
            PROCESS EVENTS . 

    END METHOD.

END CLASS.