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
    File        : EmailMessage
    Purpose     : Allows sending of Emails via SMTP
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 27 00:59:00 CEST 2012
    Notes       : See http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=5702068
                  for instructions
                  See K-Base: LONGCHAR unicode containing Russian characters 
                  in email body comes out garbled when using .NET SmtpClient to send email.
                  http://knowledgebase.progress.com/articles/Article/LONGCHAR-unicode-content-in-email-body-comes-out-garbled-when-using-NET-SmtpClient-to-send-email?popup=true
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*      FROM PROPATH .  
USING Consultingwerk.Utilities.Mail.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .
USING System.Net.*                    FROM ASSEMBLY . 
USING System.Net.Mail.*               FROM ASSEMBLY . 

CLASS Consultingwerk.Utilities.Mail.EmailMessage: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Body of the email 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Body AS LONGCHAR NO-UNDO 
    GET: 
        RETURN THIS-OBJECT:Message:Body . 
    END GET . 
    SET (arg AS LONGCHAR):
        THIS-OBJECT:Message:Body = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Returns the instance of the Email Configuration object used 
                 by this email message instance 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Configuration AS EmailConfiguration NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the instance of the MailMessage object used by this email 
                 message instance
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Message AS MailMessage NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the instance of the SmtpClient class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmtpClient AS SmtpClient NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Subject of the email 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Subject AS CHARACTER NO-UNDO 
    GET: 
        RETURN THIS-OBJECT:Message:Subject . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Message:Subject = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EmailMessage class                                                                        
        Notes:                                          
        @param poConfiguration The Email configuration used to send the message                              
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EmailMessage (poConfiguration AS EmailConfiguration):
        SUPER ().
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poConfiguration) .
        
        THIS-OBJECT:Configuration = poConfiguration .
        
        ASSIGN THIS-OBJECT:SmtpClient = NEW SmtpClient (poConfiguration:Host) 
               THIS-OBJECT:Message    = NEW MailMessage ().
        
        THIS-OBJECT:SmtpClient:UseDefaultCredentials = FALSE . 
        THIS-OBJECT:SmtpClient:Credentials = NEW NetworkCredential (poConfiguration:UserName,
                                                                    poConfiguration:Password) .
        
        THIS-OBJECT:Message:From = NEW MailAddress (poConfiguration:Sender) . 
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EmailMessage class                                                                        
        Notes:   Alternative to use the SmtpConfiguration of the System. These 
                 settings can be set in the .applicationsettings or .webconfig file. 
        @param poSmtpConfiguration The Email configuration used to send the message                              
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EmailMessage (poSmtpConfiguration AS ISmtpConfiguration):
        SUPER ().
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poSmtpConfiguration) .
        
        /* Marko Rüterbories, Consultingwerk Ltd. 03.01.2014
           The settins are taken from the poSmtpConfiguration which is not 
           type compatible with the Configuration property. */
        THIS-OBJECT:Configuration = ? .
        
        ASSIGN THIS-OBJECT:SmtpClient = NEW SmtpClient (poSmtpConfiguration:SmtpHostName, 
                                                        poSmtpConfiguration:SmtpPortNumber) 
               THIS-OBJECT:Message    = NEW MailMessage ().
        
        THIS-OBJECT:SmtpClient:UseDefaultCredentials = FALSE . 
        THIS-OBJECT:SmtpClient:Credentials = NEW NetworkCredential (poSmtpConfiguration:SmtpUserName,
                                                                    poSmtpConfiguration:SmtpPassword) .
        
        THIS-OBJECT:Message:From = NEW MailAddress (poSmtpConfiguration:SmtpSenderName) . 
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Sends the email message 
        Notes:
        @param pcReceipient The receipient for the email message   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Send (pcReceipient AS CHARACTER):
        
        THIS-OBJECT:Message:To:Add (NEW MailAddress (pcReceipient)) .
        
        THIS-OBJECT:Send () .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sends the email message 
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Send ():
        
        THIS-OBJECT:SmtpClient:Send (THIS-OBJECT:Message) .
    END METHOD .
    
END CLASS.
