/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ttViewerBoundControls.i
    Purpose     : Temp-Table to be returned by the SmartViewerControl method
                  GetBoundControlTable

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 26 15:45:45 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttViewerBoundControls NO-UNDO {&REFERENCE-ONLY}
    FIELD BindingSourceColumnName AS CHARACTER 
    FIELD PropertyName            AS CHARACTER 
    FIELD ControlReference        AS Progress.Lang.Object
    FIELD ViewerControl           AS Progress.Lang.Object
    
    INDEX ColumnProperty BindingSourceColumnName PropertyName 
    INDEX Viewer ViewerControl .
