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
    File        : SerializableToolbarGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 07:58:15 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Web2.Views.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Web2.Views.SerializableToolbarGroup 
    INHERITS JsonSerializable:

    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Items ListSerializableToolbarItem} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableToolbarGroup class
        Notes:   
        @param pcName The value for the Name
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SerializableToolbarGroup (pcName AS CHARACTER):
		THIS-OBJECT ().
		
		THIS-OBJECT:Name = pcName . 
		
	END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableToolbarGroup class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableToolbarGroup ():
        SUPER ().

        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:SerializeEmptyLists = FALSE .         
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:Items = NEW ListSerializableToolbarItem () . 

    END CONSTRUCTOR. 

END CLASS.

