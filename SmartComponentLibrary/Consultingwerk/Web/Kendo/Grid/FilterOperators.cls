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
    File        : FilterOperators
    Purpose     : Represents the operators of the filterable options
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 20:11:33 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH . 
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.FilterOperators 
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty.i string FilterOperatorsString} .
    {Consultingwerk/JsonSerializableProperty.i number FilterOperatorsNumber} .
    {Consultingwerk/JsonSerializableProperty.i date FilterOperatorsDate} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Filterable class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FilterOperators ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                    
        THIS-OBJECT:string = NEW FilterOperatorsString () .                     
        THIS-OBJECT:string:ApplyAblSupportedFilter () .

        THIS-OBJECT:number = NEW FilterOperatorsNumber () .                     
        THIS-OBJECT:date = NEW FilterOperatorsDate () .                     
                        
    END CONSTRUCTOR.

END CLASS.
