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
    File        : RowidDictionary
    Purpose     : A general purpose Dictionary class with a Character as 
                  the key and a Rowid as the value
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 22 16:02:41 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*        FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Framework.Collections.RowidDictionary 
    INHERITS AblPrimitiveDictionary: 

    {Consultingwerk/Framework/Base/AblPrimitiveDictionary.i RowidDictionary Rowid TO-ROWID}

END CLASS.