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
    File        : EmailConfiguration
    Purpose     : Provides a structure that provides configuration for 
                  sending emails via SMTP
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 27 01:07:16 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.Utilities.Mail.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Utilities.Mail.EmailConfiguration: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the host
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Host AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the password 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Password AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the email sender
        Notes:   Can be either an email address or in the form of "Real Name <info@consultingwerk.de>"
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Sender AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the user name
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UserName AS CHARACTER NO-UNDO 
    GET.
    SET. 

END CLASS.
