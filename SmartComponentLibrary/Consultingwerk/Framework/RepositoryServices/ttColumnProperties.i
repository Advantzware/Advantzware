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
    File        : ttColumnProperties.i
    Purpose     : Temp-Table for Browse Column properties, member of 
                  dsDynamicBrowser

    Syntax      :

    Description : Temp-Table eturned by the IDynamicBrowserRepository and 
                  used by IDynamicsBrowserRenderer 
                  
    Author(s)   : Mike Fechner
    Created     : 15.07.2013 17:55:36
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttColumnProperties NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eTempTable1Before &ENDIF
    FIELD BrowserKey AS CHARACTER FORMAT "X(40)":U
    FIELD FieldOrder AS INTEGER FORMAT ">>>,>>>,>>9":U
    FIELD ColumnKey AS CHARACTER FORMAT "X(20)":U
    FIELD CalculatedColumn AS LOGICAL FORMAT "yes/no":U INIT "no":U
    FIELD Hidden AS LOGICAL FORMAT "yes/no":U INIT "no":U
    FIELD ExcludeFromColumnChooser AS LOGICAL FORMAT "yes/no":U INIT "no":U
    FIELD Width AS INTEGER FORMAT ">>>,>>>,>>9":U
    FIELD Caption AS CHARACTER FORMAT "X(50)":U
    FIELD ToolTipText AS CHARACTER FORMAT "X(50)":U
    FIELD AllowSorting AS LOGICAL FORMAT "yes/no":U INIT "yes":U
    FIELD AllowRowFiltering AS LOGICAL FORMAT "yes/no":U INIT "yes":U
    FIELD AllowRowSummaries AS CHARACTER FORMAT "x(20)":U
    FIELD AllowGroupBy AS LOGICAL FORMAT "yes/no":U INIT "yes":U
    FIELD GroupByMode AS CHARACTER FORMAT "X(40)":U
    FIELD HiddenWhenGroupBy AS LOGICAL FORMAT "yes/no":U INIT "no":U
    FIELD ValueList AS Progress.Lang.Object SERIALIZE-HIDDEN
    FIELD ValueListKey AS CHARACTER FORMAT "X(20)":U
    FIELD ValueListItems AS CHARACTER FORMAT "X(50)":U
    FIELD ValueListItemPairs AS CHARACTER FORMAT "X(50)":U
    FIELD ColumnDataType AS CHARACTER FORMAT "X(20)":U
    FIELD Style AS CHARACTER FORMAT "X(20)":U
    FIELD MaskInput  AS CHARACTER FORMAT "X(20)":U
    FIELD Nullable AS CHARACTER FORMAT "x(20)":U
    FIELD NullText AS CHARACTER FORMAT "X(20)":U
    FIELD EnabledForAdd AS LOGICAL FORMAT "yes/no":U INIT "yes":U
    FIELD EnabledForUpdate AS LOGICAL FORMAT "yes/no":U INIT "yes":U
    FIELD HeaderFixed AS LOGICAL FORMAT "yes/no":U INIT "no":U

    /* Mike Fechner, Consultingwerk Ltd. 26.03.2014
       Support for referencing an object with custom properties for the GridColumn
       Supposed to be parsed by an IDynamicBrowseRendererCustomizer */
    FIELD CustomProperties AS Progress.Lang.Object SERIALIZE-HIDDEN 

    INDEX BrowserKeyFieldOrder AS UNIQUE PRIMARY BrowserKey ASCENDING FieldOrder ASCENDING
    INDEX BrowserKeyColumnKey AS UNIQUE BrowserKey ASCENDING ColumnKey ASCENDING

    .
