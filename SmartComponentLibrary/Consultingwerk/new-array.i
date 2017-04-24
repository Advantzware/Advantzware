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
    File        : new-array.i
    Purpose     : Simplify the creation of .NET Arrays in the ABL

    Syntax      : {Consultingwerk/new-array.i <.net typename> <length>}, 
                  {Consultingwerk/new-array.i System.Type 2},

    Description : The returned System.Array object will be casted to the 
                  a member type array

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 23 14:41:12 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

CAST (System.Array:CreateInstance (Progress.Util.TypeHelper:GetType("{1}":U), {2}), "{1}[]":U) 