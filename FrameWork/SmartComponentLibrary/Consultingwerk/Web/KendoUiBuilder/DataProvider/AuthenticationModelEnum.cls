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
    File        : AuthenticationModelEnum
    Purpose     : Enum for the authenticationModel property of the DataProvider
                  class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 21 18:10:48 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Web.KendoUiBuilder.DataProvider.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

&IF DEFINED (NativeEnums) NE 0 &THEN
ENUM Consultingwerk.Web.KendoUiBuilder.DataProvider.AuthenticationModelEnum:

    DEFINE ENUM Anonymous Basic Form .

END ENUM .

&ELSE
CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.AuthenticationModelEnum
    INHERITS Consultingwerk.Enum:

    {Consultingwerk/EnumMember.i Anonymous 1 AuthenticationModelEnum}
    {Consultingwerk/EnumMember.i Basic 2 AuthenticationModelEnum}
    {Consultingwerk/EnumMember.i Form 3 AuthenticationModelEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthenticationModelEnum class
        Notes:
        @param piValue The integer enum value
        @param pcLabel The charcter enum label
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE AuthenticationModelEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.Web.KendoUiBuilder.DataProvider.AuthenticationModelEnum}

END CLASS.
&ENDIF
