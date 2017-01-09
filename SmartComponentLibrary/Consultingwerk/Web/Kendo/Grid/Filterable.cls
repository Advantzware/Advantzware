/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Filterable
    Purpose     : Represents the Filterable properties of the Kendo UI grid
    Syntax      : 
    Description : http://docs.telerik.com/kendo-ui/api/javascript/ui/grid#configuration-filterable
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 01 08:34:23 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.Filterable 
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty.i mode CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i operators Consultingwerk.Web.Kendo.Grid.FilterOperators} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Filterable class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Filterable ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                        
        THIS-OBJECT:operators = NEW FilterOperators () .                         
                        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Filterable class                                                                     
        Notes:                                        
        @param pcMode The value for the mode property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Filterable (pcMode AS CHARACTER):
        THIS-OBJECT ().
        
        THIS-OBJECT:Mode = pcMode .
                        
    END CONSTRUCTOR.

END CLASS.