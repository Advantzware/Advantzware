/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : GetGroupRestrictedDefaultLevelParameter
    Purpose     : Parameter class for the GetGroupRestricted method of 
                  the SecurityService when used as a business task (called 
                  from the client)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 18 21:52:01 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.GetGroupRestrictedDefaultLevelParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    : 

    {Consultingwerk/JsonSerializableProperty.i SecurityRealmGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SecurityItemGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i GroupGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LoginCompanyKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DefaultLevel CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetGroupRestrictedDefaultLevelParameter class
        Notes:   
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcGroupGuid The groups security guid
        @param pcLoginCompanyKey The login company key 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetGroupRestrictedDefaultLevelParameter (pcSecurityRealmGuid AS CHARACTER,
                                                               pcSecurityItemGuid AS CHARACTER,
                                                               pcGroupGuid AS CHARACTER,
                                                               pcLoginCompanyKey  AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:SecurityRealmGuid = pcSecurityItemGuid 
               THIS-OBJECT:SecurityItemGuid  = pcSecurityItemGuid
               THIS-OBJECT:GroupGuid         = pcGroupGuid 
               THIS-OBJECT:LoginCompanyKey   = pcLoginCompanyKey . 

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetGroupRestrictedDefaultLevelParameter class
        Notes:   
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcGroupGuid The groups security guid
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetGroupRestrictedDefaultLevelParameter (pcSecurityRealmGuid AS CHARACTER,
                                                               pcSecurityItemGuid AS CHARACTER,
                                                               pcGroupGuid AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:SecurityRealmGuid = pcSecurityItemGuid 
               THIS-OBJECT:SecurityItemGuid  = pcSecurityItemGuid
               THIS-OBJECT:GroupGuid         = pcGroupGuid . 

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetGroupRestrictedDefaultLevelParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetGroupRestrictedDefaultLevelParameter ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.
    
END CLASS.
