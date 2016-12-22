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
    File        : parse-enum-ignorecase.i
    Purpose     : Simplify the parsing of CHARACTER to Enum value and 
                  ignore the values case

    Syntax      : {Consultingwerk/parse-enum-ignorecase.i <.net typename> <value>}
                  {Consultingwerk/parse-enum-ignorecase.i System.Windows.Forms.DockStyle cDockStyle}

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 05 21:51:38 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

CAST (System.Enum:Parse (Progress.Util.TypeHelper:GetType("{1}":U), {2}, TRUE), "{1}":U) 