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
    File        : ttBrowserProperties.i
    Purpose     : Temp-Table for Browse properties, member of dsDynamicBrowser

    Syntax      :

    Description : Temp-Table eturned by the IDynamicBrowserRepository and 
                  used by IDynamicsBrowserRenderer 

    Author(s)   : Mike Fechner
    Created     : 15.07.2013 17:55:36
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttBrowserProperties NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eTempTableBefore &ENDIF
    FIELD BrowserKey AS CHARACTER FORMAT "X(40)":U
    FIELD AllowGroupBy  AS LOGICAL FORMAT "yes/no":U
    FIELD MaxRowScrollRegions AS INTEGER FORMAT ">>9":U
    FIELD MaxColScrollRegions AS INTEGER FORMAT ">>9":U
    FIELD RowSelectors AS LOGICAL FORMAT "yes/no":U
    FIELD SelectTypeRow AS CHARACTER FORMAT "X(20)":U
    FIELD SettingsKey AS CHARACTER FORMAT "X(20)":U
    FIELD SmartFilterActive AS LOGICAL FORMAT "yes/no":U INIT "no":U
    FIELD UseFixedHeaders AS LOGICAL FORMAT "yes/no":U INIT "no":U

    /* Mike Fechner, Consultingwerk Ltd. 26.03.2014
       Support for referencing an object with custom properties for the Grid
       Supposed to be parsed by an IDynamicBrowseRendererCustomizer */
    FIELD CustomProperties AS Progress.Lang.Object 

    INDEX BrowserKey AS UNIQUE PRIMARY BrowserKey ASCENDING

    .
