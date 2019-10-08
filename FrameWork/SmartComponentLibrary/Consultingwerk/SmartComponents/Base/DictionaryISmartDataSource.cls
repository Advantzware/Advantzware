/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DictionarySmartDataSource
    Purpose     : Dictionary of ISmartDataSource instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 20 17:05:08 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*             FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH . 
USING Progress.Lang.*                             FROM PROPATH .  

CLASS Consultingwerk.SmartComponents.Base.DictionaryISmartDataSource 
    INHERITS GenericDictionary: 

    { Consultingwerk/Framework/Base/GenericDictionary.i ISmartDataSource }

END CLASS.
