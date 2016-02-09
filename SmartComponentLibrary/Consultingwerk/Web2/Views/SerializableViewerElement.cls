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
    File        : SerializableViewerElement
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

CLASS Consultingwerk.Web2.Views.SerializableViewerElement 
    INHERITS SerializableLayoutElement: 
        
    {Consultingwerk/JsonSerializableProperty.i Layout CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableViewerElement class
        Notes:
        @param pcLayout The layout value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableViewerElement (pcLayout AS CHARACTER):
        
        THIS-OBJECT () . 
        
        THIS-OBJECT:Layout = pcLayout . 
        
    END CONSTRUCTOR .
         
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableViewerElement class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableViewerElement ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .
        THIS-OBJECT:Type = LayoutElementType:Viewer . 

    END CONSTRUCTOR . 

END CLASS.
