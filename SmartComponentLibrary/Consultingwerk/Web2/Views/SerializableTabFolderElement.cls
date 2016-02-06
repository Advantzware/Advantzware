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
    File        : SerializableTabFolderElement
    Purpose     : Represents the Layout of a Viewer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 08:25:16 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web2.Views.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Web2.Views.SerializableTabFolderElement 
    INHERITS SerializableLayoutElement: 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableTabFolderElement class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableTabFolderElement ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .
        THIS-OBJECT:Type = LayoutElementType:TabFolder . 
        
    END CONSTRUCTOR . 

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Tab
        Notes:   
        @param pcTabLabel The value for the TabLabel 
        @param pcTemplate The value for the Template 
        @return The created SerializableTabElement
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC SerializableTabElement CreateTab (pcTabLabel AS CHARACTER,
                                                    pcTemplate AS CHARACTER):
		
		DEFINE VARIABLE oTab AS SerializableTabElement NO-UNDO . 
		
		oTab = NEW SerializableTabElement (pcTabLabel, pcTemplate) .
		
		THIS-OBJECT:Elements:Add (oTab) .

        RETURN oTab .
        
	END METHOD .

END CLASS.
