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
    File        : debug-list.p
    Purpose     : Opens the selected file in OpenEdge Architect as a 
                  debug-listing in an Eclipse ABL Editor.

    Syntax      : Added to the OpenEdge Customization Options 
                  OpenEdge -> Tools -> Customization Editor
                  Then create a new Menu / Toolbar Entry:
                      Name:                                    Debug Listing
                      Program name:                            Consultingwerk/Studio/DebugList/debug-list.p
                      Configuration:                           <leave empty>
                      Send file name of the current selection: checked
                      Action appearance:                       Show on menu and toolbar
                      Run persistent:                          not checked

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 29 15:20:18 CET 2011
    Notes       : see adecomm/oeideservice.p or adecomm/oeideservice.i
                  for details on the API
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER pcFileName AS CHARACTER NO-UNDO.

{adecomm/oeideservice.i}

/* ***************************  Main Block  *************************** */

DEFINE VARIABLE cFileName AS CHARACTER          NO-UNDO .
DEFINE VARIABLE cProject  AS CHARACTER          NO-UNDO .

DEFINE VARIABLE cDebugList AS CHARACTER NO-UNDO.

ASSIGN cProject  = getProjectName () 
       cFileName = SUBSTRING (pcFileName, 2) 
       
       cDebugList = /*createLinkedFile ("":U, ".p")*/
                    SESSION:TEMP-DIRECTORY + "debuglist-":U + 
                    Consultingwerk.Util.FileHelper:ShortFileName (cFileName) .

COMPILE VALUE (cFileName)
    SAVE = FALSE 
    DEBUG-LIST VALUE (cDebugList) 
    NO-ERROR . 

openEditor  
     (?,
      cDebugList,
      "UNTITLED":U,
      ?) .
