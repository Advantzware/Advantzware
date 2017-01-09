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
    File        : DictionaryIClassCache
    Purpose     : Dictionary of IClassCache instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 22 21:04:58 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Support.DictionaryIClassCache 
    INHERITS GenericDictionary: 


    {Consultingwerk/Framework/Base/GenericDictionary.i IClassCache} 

END CLASS.
