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
    File        : TableIOActionEnum
    Purpose     : Enumeration describing the Tools of a TableIO Panel/Toolbar
    Syntax      :
    Description : Used by SmartToolbarController:SetTableIOActionDisabled as
                  a parameter
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 14 21:29:09 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

{&CLASS} Consultingwerk.SmartComponents.Enum.TableIOActionEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: The Add button
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Add 1 TableIOActionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The Cancel button
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Cancel 2 TableIOActionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The copy button
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Copy 3 TableIOActionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The Delete button
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Delete 4 TableIOActionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The Save button
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Save 5 TableIOActionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The Update button
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Update 6 TableIOActionEnum}

    /*------------------------------------------------------------------------------
        Purpose: All functions
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i All 7 TableIOActionEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableIOActionEnum class
        Notes:
        @param piValue The Enum member value
        @param pcLabel The Enum member label
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE TableIOActionEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Deserialization of an Enum value
        Notes:
        @param pcCharacterValue The serialized character value of an enum member
        @return The TableIOActionEnum member
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC TableIOActionEnum FromString (pcCharacterValue AS CHARACTER):

        CASE pcCharacterValue:
            WHEN "Add":U THEN
                RETURN TableIOActionEnum:Add .
            WHEN "Cancel":U THEN
                RETURN TableIOActionEnum:Cancel .
            WHEN "Copy":U THEN
                RETURN TableIOActionEnum:Copy .
            WHEN "Delete":U THEN
                RETURN TableIOActionEnum:Delete .
            WHEN "Save":U THEN
                RETURN TableIOActionEnum:Save .
            WHEN "Update":U THEN
                RETURN TableIOActionEnum:Update .
            OTHERWISE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid Enum value &1":U, pcCharacterValue), 0) .
        END CASE .

    END METHOD.

&ENDIF

END {&CLASS} .