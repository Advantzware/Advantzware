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
    File        : SerializableToolbarsItem
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 08:00:34 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.JsonSerializable.
USING Consultingwerk.* FROM PROPATH .
USING Consultingwerk.Web2.Views.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Web2.Views.SerializableToolbarItem 
    INHERITS JsonSerializable:

    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Caption CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableToolbarItem class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableToolbarItem ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableToolbarItem class
        Notes:   
        @param pcName The value for the Name
        @param pcCaption The value for the Caption
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableToolbarItem (pcName AS CHARACTER,
                                                pcCaption AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:Name    = pcName 
               THIS-OBJECT:Caption = pcCaption .  
          
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableToolbarItem class
        Notes:   
        @param pcName The value for the Name
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SerializableToolbarItem (pcName AS CHARACTER):
		THIS-OBJECT ().
		
		ASSIGN THIS-OBJECT:Name = pcName .  
		  
	END CONSTRUCTOR.

END CLASS.
