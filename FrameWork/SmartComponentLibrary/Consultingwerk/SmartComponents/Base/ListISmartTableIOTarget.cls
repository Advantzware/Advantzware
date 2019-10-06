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
    File        : ListISmartTableIOTarget
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 14 12:47:16 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.GenericList .
USING Consultingwerk.SmartComponents.Interfaces.ISmartTableIOTarget .
USING Progress.Lang.* .

CLASS Consultingwerk.SmartComponents.Base.ListISmartTableIOTarget 
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i ISmartTableIOTarget }

END CLASS.
