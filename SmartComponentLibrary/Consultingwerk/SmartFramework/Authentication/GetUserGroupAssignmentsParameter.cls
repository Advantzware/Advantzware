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
    File        : GetUserGroupAssignmentsParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 16 14:38:03 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authentication.GetUserGroupAssignmentsParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF            
    : 

    {Consultingwerk/JsonSerializableProperty.i UserName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LoginCompanyKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i GroupGuids CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i GroupNames CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetUserGroupAssignmentsParameter class
        Notes:   
        @param pcUserName The name of the Current user
        @param pcLoginCompanyKey The key of the login company
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC GetUserGroupAssignmentsParameter (pcUserName AS CHARACTER,
	                                                     pcLoginCompanyKey AS CHARACTER):
		THIS-OBJECT ().
		
		ASSIGN 
		  THIS-OBJECT:UserName        = pcUserName 
		  THIS-OBJECT:LoginCompanyKey = pcLoginCompanyKey . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetUserGroupAssignmentsParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC GetUserGroupAssignmentsParameter ():
		SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

	END CONSTRUCTOR.

END CLASS.
