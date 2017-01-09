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
    File        : AuthenticationParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 16 17:00:55 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH . 
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authentication.AuthenticationParameter
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    : 

    {Consultingwerk/JsonSerializableProperty.i CompanyKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i CompanyName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Password CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i UserName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ClientPrincipal RAW} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthenticationParameter class
        Notes:   
        @param poCredentials The IUserCredential instance to populate the values of this instance from 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationParameter (poCredentials AS IUserCredentials):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:CompanyKey  = poCredentials:CompanyKey
            THIS-OBJECT:CompanyName = poCredentials:CompanyName
            THIS-OBJECT:Password    = poCredentials:Password 
            THIS-OBJECT:UserName    = poCredentials:UserName . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthenticationParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationParameter ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
