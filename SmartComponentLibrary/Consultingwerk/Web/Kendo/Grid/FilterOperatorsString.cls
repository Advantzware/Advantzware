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
    File        : FilterOperatorsString
    Purpose     : Represents the string filter operators of the Kendo Grid
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

CLASS Consultingwerk.Web.Kendo.Grid.FilterOperatorsString 
    INHERITS JsonSerializable:

    {Consultingwerk/JsonSerializableProperty11.i startswith CHARACTER "INIT ""Starts with"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i eq CHARACTER "INIT ""Is equal to"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i neq CHARACTER "INIT ""Is not equal to"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i contains CHARACTER "INIT ""Contains"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i doesnotcontain CHARACTER "INIT ""Does not contain"":U"} .
    {Consultingwerk/JsonSerializableProperty11.i endswith CHARACTER "INIT ""Ends with"":U"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FilterOperatorsString class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FilterOperatorsString ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Applies ABL supported filters 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyAblSupportedFilter ():

        ASSIGN THIS-OBJECT:eq             = "Is equal to":U 
               THIS-OBJECT:neq            = "Is not equal to":U 
               THIS-OBJECT:startswith     = "Starts with":U 
               THIS-OBJECT:contains       = "":U 
               THIS-OBJECT:doesnotcontain = "":U
               THIS-OBJECT:endswith       = "":U .

    END METHOD .

END CLASS.
