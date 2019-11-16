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
    File        : SmartWeb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 27 23:06:19 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .
USING Consultingwerk.Web.*       FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Web.SmartWeb: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the JavaScriptLibraries for this project 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY JavaScriptLibraries AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the JavaScriptLibraries for this project 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY StyleSheetFiles AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Initializes the SmartComponets.Web 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Initialize ():
        
        DEFINE VARIABLE oConfig AS ConfigurationProvider NO-UNDO .
        DEFINE VARIABLE cValue  AS CHARACTER             NO-UNDO .
        
        FILE-INFORMATION:FILE-NAME = ".webconfig":U . 
        
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN DO:
            
            oConfig = NEW ConfigurationProvider (FILE-INFORMATION:FULL-PATHNAME) . 
            
            cValue = oConfig:GetValue ("javaScriptLibraries":U) .
            
            IF cValue > "":U THEN 
                SmartWeb:JavaScriptLibraries = cValue .
                
            cValue = oConfig:GetValue ("styleSheetFiles":U) .

            IF cValue > "":U THEN 
                SmartWeb:StyleSheetFiles = cValue .
                
            /* Mike Fechner, Consultingwerk Ltd. 03.01.2014
               Register ISmtpConfiguration Service when the .webconfig file 
               contains an Entry for SmptHostName */
            IF oConfig:GetValue ("SmtpHostName":U) > "":U THEN DO:
               DEFINE VARIABLE oSmtpConfiguration AS SmtpConfiguration NO-UNDO . 
               
               oSmtpConfiguration = NEW SmtpConfiguration () . 
               oSmtpConfiguration:SmtpHostName   = oConfig:GetValue ("SmtpHostName":U) .
               oSmtpConfiguration:SmtpPassword   = oConfig:GetValue ("SmtpPassword":U, "":U) .
               oSmtpConfiguration:SmtpPortNumber = INTEGER (oConfig:GetValue ("SmtpPortNumber":U, ?)) .
               oSmtpConfiguration:SmtpSenderName = oConfig:GetValue ("SmtpSenderName":U, "":U) . 
               oSmtpConfiguration:SmtpUserName   = oConfig:GetValue ("SmtpUserName":U, "":U) .
            
               FrameworkSettings:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.Framework.ISmtpConfiguration":U),
                                                              oSmtpConfiguration) .
            END.                   
        END.

    END METHOD .

END CLASS.
