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
    File        : ListSerializableLayoutElement
    Purpose     : List of SerializableLayoutElement instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 08:09:48 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.Web2.Views.*     FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web2.Views.ListSerializableLayoutElement 
    INHERITS GenericList: 

    {Consultingwerk/Framework/Base/GenericList.i SerializableLayoutElement}

END CLASS.
