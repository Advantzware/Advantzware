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
    File        : ListGridColumn.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 23:28:55 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Base.GenericList FROM PROPATH .
USING Consultingwerk.Web.Widgets.Ribbon.*       FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.Ribbon.ListTaskBarMenuItem
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i TaskBarMenuItem }

END CLASS.
