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
    File        : QueryJoinEnum
    Purpose     : Enum with possible values for the QueryJoin property of
                  the IBufferSpec interface, see BufferDataSource

    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 11 21:13:31 CEST 2011
    Notes       : Used for the AfterInitializeBusinessEntityAdapterEventArgs
                  class
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

{&CLASS} Consultingwerk.OERA.QueryJoinEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Use Inner-Join to parent buffer
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i InnerJoin 1 QueryJoinEnum}

    /*------------------------------------------------------------------------------
        Purpose: Use Outer-Join to parent buffer
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i OuterJoin 2 QueryJoinEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryJoinEnum class
        Notes:
        @param piValue The integer enum value
        @param pcLabel The charcter enum label
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE QueryJoinEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.OERA.QueryJoinEnum}

&ENDIF

END {&CLASS} .
