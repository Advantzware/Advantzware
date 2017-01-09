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
    File        : CannotMoveNextOnChangedList
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 05 10:15:13 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Framework.Exceptions.CannotMoveNextOnChangedList
    INHERITS Exception: 

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the CannotMoveNextOnChangedList class
	    Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CannotMoveNextOnChangedList ():
		SUPER ("Cannot call MoveNext() on List after List has changed."{&TRAN}, 0).
		
	END CONSTRUCTOR.

END CLASS.
