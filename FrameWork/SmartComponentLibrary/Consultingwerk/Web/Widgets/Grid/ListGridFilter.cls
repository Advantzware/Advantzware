/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ListGridFilter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 26 21:28:21 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Base.GenericList FROM PROPATH .
USING Consultingwerk.Web.Widgets.Grid.*         FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.Grid.ListGridFilter
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i GridFilter }

END CLASS.
