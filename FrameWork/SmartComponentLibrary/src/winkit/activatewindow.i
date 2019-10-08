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
    File        : activatewindow.i
    Purpose     : Gives focus to this window by activating the embedding
                  form 

    Syntax      : include file, no parameter

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 08 18:19:03 CET 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 08.07.2009
   Conditional compile rule: Will compile in OpenEdge release 10.2A and above,
   This rule will break when release 40.x will be reached. Note PROVERSION
   returns a string, so this is alpha comparison, not numeric */
&IF NOT PROVERSION GE "4" AND PROVERSION GE "10.2A" &THEN
IF VALID-OBJECT(oForm) THEN 
    CAST(CAST(oForm, Progress.Lang.Object), System.Windows.Forms.Form):Activate() .
&ENDIF
