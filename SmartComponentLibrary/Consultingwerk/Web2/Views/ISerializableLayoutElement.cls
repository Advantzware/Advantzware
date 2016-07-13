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
    File        : ISerializableLayoutElement
    Purpose     : Interface for serializable Layout Elements
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 14:28:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web2.Services.SmartViews.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Web2.Views.ISerializableLayoutElement:  
  
    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i InstanceId CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Type Consultingwerk.Web2.Views.LayoutElementType} .
  
END INTERFACE.
