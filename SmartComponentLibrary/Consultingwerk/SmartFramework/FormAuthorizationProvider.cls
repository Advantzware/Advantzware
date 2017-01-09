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
    File        : FormAuthorizationProvider
    Purpose     : Sample IFormAuthorizationProvider implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 08 23:05:15 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Framework.Authorization.IFormAuthorizationProvider.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartFramework.FormAuthorizationProvider 
    IMPLEMENTS IFormAuthorizationProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Provides authorization for launching Forms                                                                       
        Notes:                                              
        @param poRequestor The reference to the requesting object
        @param pcClassName The Form class name to launch
        @return Logical value indicating if the Form startup is restricted                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsFormStartupRestricted (poRequestor AS Progress.Lang.Object,
                                                   pcClassName AS CHARACTER):
        
/*        IF pcClassName = "Consultingwerk.SmartComponentsDemo.CustomerExplorer.Main.CustomerExplorer":U OR          */
/*           pcClassName = "Consultingwerk.SmartComponentsDemo.CustomerExplorer.WelcomeScreen.WelcomeScreenForm":U OR*/
/*           pcClassName = "Consultingwerk.BPM.Windows.LoginForm":U OR                                               */
/*           pcClassName = "Consultingwerk.Windows.Session.LoginForm":U THEN                                         */
/*                                                                                                                   */
/*            RETURN FALSE .                                                                                               */
/*                                                                                                                           */
/*        RETURN TRUE .                                                                                                    */

        RETURN FALSE . 

    END METHOD.

END CLASS.