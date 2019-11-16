/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ViewerBindingStyleEnum
    Purpose     : Enum for the style of data bindings for a html viewer template
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 21 18:10:48 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Web2.Services.SmartViews.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

&IF DEFINED (NativeEnums) NE 0 &THEN
ENUM Consultingwerk.Web2.Services.SmartViews.ViewerBindingStyleEnum:

    DEFINE ENUM AngularJS Angular2 .

END ENUM .

&ELSE
CLASS Consultingwerk.Web2.Services.SmartViews.ViewerBindingStyleEnum
    INHERITS Consultingwerk.Enum:

    {Consultingwerk/EnumMember.i AngularJS 1 ViewerBindingStyleEnum}
    {Consultingwerk/EnumMember.i Angular2 2 ViewerBindingStyleEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthenticationModelEnum class
        Notes:
        @param piValue The integer enum value
        @param pcLabel The charcter enum label
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ViewerBindingStyleEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.Web2.Services.SmartViews.ViewerBindingStyleEnum}

END CLASS.
&ENDIF
