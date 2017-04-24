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
    File        : FilterOperatorsNumber
    Purpose     : Represents the number filter operators of the Kendo Grid
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 20:17:52 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.FilterOperatorsNumber 
    INHERITS JsonSerializable:

    {Consultingwerk/JsonSerializableProperty11.i gte CHARACTER "INIT ""Is greater than or equal to"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i gt CHARACTER "INIT ""Is greater than"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i lte CHARACTER "INIT ""Is less than or equal to"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i lt CHARACTER "INIT ""Is less than"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i eq CHARACTER "INIT ""Is equal to"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i neq CHARACTER "INIT ""Is not equal to"":U"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FilterOperatorsNumber class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FilterOperatorsNumber ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 

    END CONSTRUCTOR. 

END CLASS.
