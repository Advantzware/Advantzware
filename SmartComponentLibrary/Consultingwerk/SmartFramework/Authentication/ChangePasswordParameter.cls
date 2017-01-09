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
    File        : ChangePasswordParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Apr 02 12:37:04 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH.
USING Consultingwerk.Framework.* FROM PROPATH.
USING Progress.Lang.*            FROM PROPATH.

CLASS Consultingwerk.SmartFramework.Authentication.ChangePasswordParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    : 

    {Consultingwerk/JsonSerializableProperty.i CompanyKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Password CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i UserName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i NewPassword CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ChangePasswordParameter class
        Notes:   
        @param pcUserName The name of the user to change the password for
        @param pcLoginCompanyGuid The company the user belongs too
        @param pcCurrentPassword The current password
        @param pcNewPassword The new password to be set
        @param pcNewPasswordVerified The repeated new password
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ChangePasswordParameter (pcUserName AS CHARACTER, 
                                                pcLoginCompanyGuid AS CHARACTER, 
                                                pcCurrentPassword AS CHARACTER, 
                                                pcNewPassword AS CHARACTER, 
                                                pcNewPasswordVerified AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN 
            THIS-OBJECT:CompanyKey  = pcLoginCompanyGuid
            THIS-OBJECT:Password    = pcNewPassword
            THIS-OBJECT:UserName    = pcUserName 
            THIS-OBJECT:NewPassword = pcNewPassword . 
            .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ChangePasswordParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ChangePasswordParameter ():
		SUPER ().
		
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ChangePasswordParameter class
        Notes:   
        @param poCredentials The IUserCredential instance to populate the values of this instance from 
        @param pcNewPassword The new password to be set for the user
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ChangePasswordParameter (poCredentials AS IUserCredentials,
                                                pcNewPassword AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:CompanyKey  = poCredentials:CompanyKey
            THIS-OBJECT:Password    = poCredentials:Password 
            THIS-OBJECT:UserName    = poCredentials:UserName 
            THIS-OBJECT:NewPassword = pcNewPassword . 
        
    END CONSTRUCTOR.

END CLASS.