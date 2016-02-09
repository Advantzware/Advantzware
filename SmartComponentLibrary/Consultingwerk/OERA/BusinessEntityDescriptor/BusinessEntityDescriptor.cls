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
    File        : BusinessEntityDescriptor
    Purpose     : Describes the capabilities and usage of a Business Entity
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 17:41:34 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.BusinessEntityDescriptor 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IBusinessEntityDescriptor
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i BusinessEntityName CHARACTER}                                                                
    {Consultingwerk/JsonSerializableProperty.i DefaultViewName CHARACTER}                                                                
    {Consultingwerk/JsonSerializableProperty.i InvokableMethods Consultingwerk.OERA.BusinessEntityDescriptor.ListInvokableMethodDescriptor} 
    {Consultingwerk/JsonSerializableProperty.i Tables Consultingwerk.OERA.BusinessEntityDescriptor.ListBusinessEntityTableDescriptor}      
    {Consultingwerk/JsonSerializableProperty.i Views Consultingwerk.OERA.BusinessEntityDescriptor.ListBusinessEntityViewDescriptor}        

    /*------------------------------------------------------------------------------
        Purpose: Returns the descriptor of the default View of the Business Entity
        Notes:   Returns ? when no default view is defined
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DefaultView AS IBusinessEntityViewDescriptor NO-UNDO 
	GET:
		IF THIS-OBJECT:DefaultViewName > "":U THEN DO:
		    
		    IF THIS-OBJECT:Views:ContainsName (THIS-OBJECT:DefaultViewName) THEN 
		        RETURN THIS-OBJECT:Views:GetItem (THIS-OBJECT:DefaultViewName) .
		    ELSE 
		        RETURN ? . 
		END.
		
		RETURN ? .
		
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityDescriptor class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityDescriptor ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:InvokableMethods = NEW ListInvokableMethodDescriptor () .
        THIS-OBJECT:Tables = NEW ListBusinessEntityTableDescriptor () . 
        THIS-OBJECT:Views = NEW ListBusinessEntityViewDescriptor () .  

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityDescriptor class
        Notes:
        @param pcBusinessEntityName The Business Entity Name to initialize the Business Entity Descriptor with
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityDescriptor (pcBusinessEntityName AS CHARACTER):
        THIS-OBJECT () . 

        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName . 

    END CONSTRUCTOR .
    
END CLASS.
