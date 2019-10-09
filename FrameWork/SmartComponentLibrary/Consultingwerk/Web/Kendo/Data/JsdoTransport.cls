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
    File        : JsdoTransport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 07:29:31 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.JsonSerializable.
USING Consultingwerk.Web.Kendo.Data.ITransport.
USING Consultingwerk.* FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.JsdoTransport 
    IMPLEMENTS ITransport 
    INHERITS JsonSerializable:

    {Consultingwerk/JsonSerializableProperty11.i jsdo CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i tableRef CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i countFnName CHARACTER "INIT ""count"":U"} .

    /* ITransport */
    {Consultingwerk/JsonSerializableProperty11.i dataType CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i read CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the JsdoTransport class
        Notes:   
        @param pcJsdo The value for jsdo
        @param pcTableRef The value for tableRef
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC JsdoTransport (pcJsdo AS CHARACTER,
                                      pcTableRef AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:jsdo     = pcJsdo
               THIS-OBJECT:tableRef = pcTableRef .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the JsdoTransport class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC JsdoTransport ():
        SUPER ().

        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
