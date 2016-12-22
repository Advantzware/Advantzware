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
    File        : ListSmartBusinessEntityTableConfiguration
    Purpose     : Generic List of ISmartBusinessEntityTableConfiguration 
                  instances 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 03 19:06:40 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.System.ListSmartBusinessEntityTableConfiguration 
    INHERITS GenericList
    {&SERIALIZABLE}: 

    {Consultingwerk/Framework/Base/GenericList.i ISmartBusinessEntityTableConfiguration}

END CLASS.
