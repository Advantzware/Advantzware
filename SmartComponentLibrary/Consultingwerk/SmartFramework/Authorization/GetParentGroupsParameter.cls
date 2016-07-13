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
    File        : GetParentGroupsParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 18 21:20:10 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.GetParentGroupsParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF            
    : 

    {Consultingwerk/JsonSerializableProperty.i GroupGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ParentGroupGuids CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetParentGroupsParameter class
        Notes:   
        @param pcGroupGuid The GroupGuid value for the new instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetParentGroupsParameter (pcGroupGuid AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:GroupGuid = pcGroupGuid  . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetParentGroupsParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetParentGroupsParameter ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
