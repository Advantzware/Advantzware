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
    File        : SerializableToolbarElement
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 07:56:30 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web2.Views.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Web2.Views.SerializableToolbarElement 
    INHERITS SerializableLayoutElement: 

    {Consultingwerk/JsonSerializableProperty11.i Groups Consultingwerk.Web2.Views.ListSerializableToolbarGroup} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableLayoutElement class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableToolbarElement ():
        SUPER ().

        THIS-OBJECT:SerializeEmptyLists = FALSE .         
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:Type = LayoutElementType:Toolbar .
        THIS-OBJECT:Groups = NEW ListSerializableToolbarGroup () .

    END CONSTRUCTOR . 

END CLASS.
