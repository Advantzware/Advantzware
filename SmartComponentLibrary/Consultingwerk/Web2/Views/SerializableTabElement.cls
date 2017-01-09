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
    File        : SerializableTabElement
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

CLASS Consultingwerk.Web2.Views.SerializableTabElement 
    INHERITS SerializableLayoutElement: 
        
    {Consultingwerk/JsonSerializableProperty11.i TabLabel CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableTabElement class
        Notes:
        @param pcTabLabel The value for the TabLabel 
        @param pcTemplate The value for the Template 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableTabElement (pcTabLabel AS CHARACTER,
                                               pcTemplate AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:TabLabel = pcTabLabel 
               THIS-OBJECT:Template = pcTemplate .

    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableTabElement class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableTabElement ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .
        THIS-OBJECT:Type = LayoutElementType:Tab . 
        
    END CONSTRUCTOR . 

END CLASS.
