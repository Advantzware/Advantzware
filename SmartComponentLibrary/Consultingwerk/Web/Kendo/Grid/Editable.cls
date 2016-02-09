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
    File        : Editable
    Purpose     : Represents the Editable configuration of the Kendo UI grid
    Syntax      : 
    Description : http://docs.telerik.com/kendo-ui/api/javascript/ui/grid#configuration-editable
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 03 00:25:39 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.Editable
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty.i mode CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Editable class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Editable ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Editable class                                                                     
        Notes:
        @param pcMode The value for the mode property                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Editable (pcMode AS CHARACTER):
        THIS-OBJECT ().
    
        ASSIGN THIS-OBJECT:mode = pcMode . 

    END CONSTRUCTOR . 

END CLASS.
