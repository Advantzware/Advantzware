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
    File        : SmtpConfiguration
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 03 08:54:51 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Framework.SmtpConfiguration 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS ISmtpConfiguration:
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the SMTP Host Name  
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SmtpHostName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Password for the SMTP Authentication 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SmtpPassword CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the SMTP Port Number 
        Notes:   Return ? to use the default port number
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SmtpPortNumber INTEGER "INITIAL ?"} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the sender of the emails 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SmtpSenderName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the User Name for the SMTP Authentication 
        Notes:   Leave empty to send without authentication
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SmtpUserName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmtpConfiguration class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmtpConfiguration ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
    END CONSTRUCTOR.

END CLASS.
