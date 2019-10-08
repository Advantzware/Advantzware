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
    File        : SerializableDataSourceElement
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 14:47:52 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Consultingwerk.Web2.Views.*     FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web2.Views.SerializableDataSourceElement 
    INHERITS SerializableLayoutElement: 

    {Consultingwerk/JsonSerializableProperty11.i ForeignFields CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i DataSource Consultingwerk.Web.Kendo.Data.DataSource} .
    {Consultingwerk/JsonSerializableProperty11.i CatalogUri CHARACTER} .
     
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableDataSourceElement class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableDataSourceElement ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:Type = LayoutElementType:DataSource .
        THIS-OBJECT:DataSource = NEW DataSource () .

    END CONSTRUCTOR . 
 
END CLASS.
