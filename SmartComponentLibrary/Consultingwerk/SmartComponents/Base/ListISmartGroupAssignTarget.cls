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
    File        : ListISmartGroupAssignTarget
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 04 19:09:24 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.GenericList .
USING Consultingwerk.SmartComponents.Interfaces.ISmartGroupAssignTarget .
USING Progress.Lang.* .

CLASS Consultingwerk.SmartComponents.Base.ListISmartGroupAssignTarget 
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i ISmartGroupAssignTarget }

END CLASS.
