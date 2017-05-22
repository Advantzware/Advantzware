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
    File        : Pageable
    Purpose     : Represents the pageable configuration of the Kendo UI Grid
    Syntax      : 
    Description : http://docs.telerik.com/kendo-ui/api/javascript/ui/grid#configuration-pageable
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 01 09:05:08 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.Pageable 
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty11.i refresh LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i pageSizes "INTEGER EXTENT"} .
    {Consultingwerk/JsonSerializableProperty11.i pageSize INTEGER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i buttonCount INTEGER "INIT ?"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Pageable class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Pageable ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                        
    END CONSTRUCTOR.

END CLASS.
