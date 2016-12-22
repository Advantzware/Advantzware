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
    File        : IsRestrictedParameter
    Purpose     : Parameter class for the IsRestricted method of the SecurityService
                  when used as a business task (called from the client)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 18 20:28:41 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH . 
USING Consultingwerk.Framework.*                    FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.IsRestrictedParameter
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    : 

    {Consultingwerk/JsonSerializableProperty.i RealmCode CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SecurityItemGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i IsRestricted LOGICAL "INIT ?"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the IsRestrictedParameter class
        Notes:   
        @param pcRealmCode The code of the security realm to verify
        @param pcSecurityItemGuid The guid of the security item to verify
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC IsRestrictedParameter (pcRealmCode AS CHARACTER, 
                                              pcSecurityItemGuid AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:RealmCode        = pcRealmCode 
               THIS-OBJECT:SecurityItemGuid = pcSecurityItemGuid .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the IsRestrictedParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC IsRestrictedParameter ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
