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
    File        : ISmtpConfiguration
    Purpose     : Interface for Holder classes that maintain SMTP configuration
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 03 08:52:01 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Framework.ISmtpConfiguration:  
  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the SMTP Host Name  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmtpHostName AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Password for the SMTP Authentication 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmtpPassword AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the SMTP Port Number 
        Notes:   Return ? to use the default port number
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmtpPortNumber AS INTEGER NO-UNDO 
    GET.
     

    /*------------------------------------------------------------------------------
        Purpose: Returns the sender of the emails 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmtpSenderName AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the User Name for the SMTP Authentication 
        Notes:   Leave empty to send without authentication
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmtpUserName AS CHARACTER NO-UNDO 
    GET.

END INTERFACE.