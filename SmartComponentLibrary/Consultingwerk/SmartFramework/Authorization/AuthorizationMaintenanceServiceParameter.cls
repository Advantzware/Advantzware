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
    File        : AuthorizationMaintenanceServiceParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 21 10:24:16 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                              FROM PROPATH . 
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.AuthorizationMaintenanceServiceParameter
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    : 

    {Consultingwerk/JsonSerializableProperty.i SecurityRealmGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i OwnerGuids CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i AlternativeLoginCompanyKey CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthorizationMaintenanceServiceParameter class
        Notes:   
        @param pcSecurityRealmGuid The value for the SecurityRealmGuid property
        @param pcOwnerGuids The value for the OwnerGuids property
        @param pcAlternativeLoginCompanyKey The value for the AlternativeLoginCompanyKey property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthorizationMaintenanceServiceParameter (pcSecurityRealmGuid          AS CHARACTER,
                                                                 pcOwnerGuids                 AS CHARACTER,
                                                                 pcAlternativeLoginCompanyKey AS CHARACTER):
    
        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:SecurityRealmGuid          = pcSecurityRealmGuid 
               THIS-OBJECT:OwnerGuids                 = pcOwnerGuids 
               THIS-OBJECT:AlternativeLoginCompanyKey = pcAlternativeLoginCompanyKey.
                                                                     
    END.
                                                                         
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthorizationMaintenanceServiceParameter class
        Notes:   
        @param pcSecurityRealmGuid The value for the SecurityRealmGuid property
        @param pcOwnerGuids The value for the OwnerGuids property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthorizationMaintenanceServiceParameter (pcSecurityRealmGuid AS CHARACTER,
                                                                 pcOwnerGuids        AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:SecurityRealmGuid = pcSecurityRealmGuid 
               THIS-OBJECT:OwnerGuids        = pcOwnerGuids .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthorizationMaintenanceServiceParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AuthorizationMaintenanceServiceParameter ():
		SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
		
	END CONSTRUCTOR.

END CLASS.
