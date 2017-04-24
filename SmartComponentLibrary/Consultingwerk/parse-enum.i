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
    File        : parse-enum.i
    Purpose     : Simplify the parsing of CHARACTER to Enum value

    Syntax      : {Consultingwerk/parse-enum.i <.net typename> <value>}
                  {Consultingwerk/parse-enum.i System.Windows.Forms.DockStyles cDockStyle}

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 05 21:51:38 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

CAST (System.Enum:Parse (Progress.Util.TypeHelper:GetType("{1}":U), {2}), "{1}":U) 