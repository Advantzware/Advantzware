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
    File        : GetUserRestrictedDefaultLevelParameter
    Purpose     : Parameter class for the GetUserRestricted method of 
                  the SecurityService when used as a business task (called 
                  from the client)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 21 21:52:01 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.GetUserRestrictedDefaultLevelParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    : 

    {Consultingwerk/JsonSerializableProperty.i SecurityRealmGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SecurityItemGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i UserGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LoginCompanyKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DefaultLevel CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetUserRestrictedDefaultLevelParameter class
        Notes:   
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcUserGuid The users security groups
        @param pcLoginCompanyKey The login company key 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetUserRestrictedDefaultLevelParameter (pcSecurityRealmGuid AS CHARACTER,
                                                               pcSecurityItemGuid AS CHARACTER,
                                                               pcUserGuid AS CHARACTER,
                                                               pcLoginCompanyKey AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:SecurityRealmGuid = pcSecurityItemGuid 
               THIS-OBJECT:SecurityItemGuid  = pcSecurityItemGuid
               THIS-OBJECT:UserGuid          = pcUserGuid  
               THIS-OBJECT:LoginCompanyKey   = pcLoginCompanyKey . 

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetUserRestrictedDefaultLevelParameter class
        Notes:   
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcUserGuid The users security groups
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetUserRestrictedDefaultLevelParameter (pcSecurityRealmGuid AS CHARACTER,
                                                               pcSecurityItemGuid AS CHARACTER,
                                                               pcUserGuid AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:SecurityRealmGuid = pcSecurityItemGuid 
               THIS-OBJECT:SecurityItemGuid  = pcSecurityItemGuid
               THIS-OBJECT:UserGuid          = pcUserGuid . 

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetUserRestrictedDefaultLevelParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetUserRestrictedDefaultLevelParameter ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.
    
END CLASS.
