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
    File        : SecurityRealmLookupParameter
    Purpose     : Parameter class for the GetSecurityRealmLookupParameter method 
                  of the SecurityRealmInfoService class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 28 21:44:47 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityRealmLookupParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    : 

    {Consultingwerk/JsonSerializableProperty.i SecurityRealmGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupDescriptor Consultingwerk.Framework.RepositoryServices.SmartBusinessEntityLookupDescriptor} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityRealmLookupParameter class
        Notes:   
        @param pcSecurityRealmGuid The SecurityRealmGuid value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityRealmLookupParameter (pcSecurityRealmGuid AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:SecurityRealmGuid = pcSecurityRealmGuid .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityRealmLookupParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SecurityRealmLookupParameter ():
		SUPER ().
		
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
		
	END CONSTRUCTOR.

END CLASS.
