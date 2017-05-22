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
    File        : OptionsOnDisconnectEnum
    Purpose     : Enum for the Options used by the Service Adapter when the
                  AppServer Connection is not available (ServiceAdapter:BehaviorOnDisconnect)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd
    Created     : Sat Oct 22 12:14:36 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

&IF DEFINED (NativeEnums) NE 0 &THEN
ENUM Consultingwerk.OERA.OptionsOnDisconnectEnum:

    DEFINE ENUM Quit QuitRetry QuitRetryIgnore Ignore.

END ENUM .

&ELSE
CLASS Consultingwerk.OERA.OptionsOnDisconnectEnum
    INHERITS Consultingwerk.Enum:

    {Consultingwerk/EnumMember.i Quit 1 OptionsOnDisconnectEnum}
    {Consultingwerk/EnumMember.i QuitRetry 2 OptionsOnDisconnectEnum}
    {Consultingwerk/EnumMember.i QuitRetryIgnore 3 OptionsOnDisconnectEnum}
    {Consultingwerk/EnumMember.i Ignore 4 OptionsOnDisconnectEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the OptionsOnDisconnectEnum class
        Notes:
        @param piValue The integer enum value
        @param pcLabel The charcter enum label
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE OptionsOnDisconnectEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.OERA.OptionsOnDisconnectEnum}

END CLASS.
&ENDIF
