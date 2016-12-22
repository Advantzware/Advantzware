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
    File        : ListSerializableMenuItem
    Purpose     : List of SerializableMenuItem
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 13:06:52 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*           FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.SmartFramework.Menu.ListSerializableMenuItem 
    INHERITS GenericList 
    {&SERIALIZABLE}:
    
    {Consultingwerk/Framework/Base/GenericList.i SerializableMenuItem}

END CLASS.
