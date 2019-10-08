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
    File        : JUnitOutputWriter
    Purpose     : Generates a JUnit/ant compatible output of test results
    Syntax      : 
    Description : Allows parsing of test results by Jenkins CI/Hudson CI
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Mar 19 09:00:59 CET 2013
    Notes       : Dataset definition is contained in Consultingwerk/SmartUnit/dsTestsuites.i 
                  See http://nelsonwells.net/2012/09/how-jenkins-ci-parses-and-displays-junit-output/
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                  FROM PROPATH .  
USING Consultingwerk.SmartUnit.Output.* FROM PROPATH .  
USING Consultingwerk.Util.*             FROM PROPATH . 
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.SmartUnit.Output.JUnitOutputWriter: 

    {Consultingwerk/SmartUnit/dsTestsuites.i &REFERENCE-ONLY=REFERENCE-ONLY}

    /*------------------------------------------------------------------------------
        Purpose: Removes invalid characters from XML output
        Notes:   
        @param plcLongchar The longchar value to cleanup
        @return The cleaned up longchar value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR ReplaceInvalidXmlCharacter (plcLongchar AS LONGCHAR):
        
        DEFINE VARIABLE iValue  AS INTEGER  NO-UNDO.
        
        DO iValue = 1 TO 31 ON ERROR UNDO, THROW:
            IF iValue <> 10 AND iValue <> 13 THEN 
                plcLongchar = REPLACE (plcLongchar, CHR (iValue), SUBSTITUTE ("&&#&1;":U, STRING (iValue, "9999":U))) .
        END.
        
        ASSIGN plcLongchar = REPLACE (plcLongchar, "<":U, "&lt;":U)
               plcLongchar = REPLACE (plcLongchar, ">":U, "&gt;":U) .
        
        RETURN plcLongchar .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Writes a dsTestsuites to an XML file using SAX  
        Notes:   
        @param pcTargetFile The target file to write to
        @param dsTestsuites INPUT The Dataset dsTestsuites
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID WriteTo (pcTargetFile AS CHARACTER,
                                INPUT DATASET dsTestsuites):
        
        DEFINE VARIABLE hSaxWriter  AS HANDLE        NO-UNDO .
        DEFINE VARIABLE hAttributes AS HANDLE        NO-UNDO . 
        DEFINE VARIABLE oFormat     AS NumericFormat NO-UNDO .
        DEFINE VARIABLE lcOutput    AS LONGCHAR      NO-UNDO . 
        
        oFormat = SessionHelper:GetNumericFormat () .

        /* Mike Fechner, Consultingwerk Ltd. 24.03.2013
           Use US numeric format while generating JUnit XML output */
        SESSION:NUMERIC-FORMAT = "AMERICAN":U . 

        CREATE SAX-WRITER hSaxWriter.
        hSaxWriter:FORMATTED = TRUE.
        hSaxWriter:ENCODING = "UTF-8":U . 

        hSaxWriter:SET-OUTPUT-DESTINATION ("file":U, pcTargetFile).

        hSaxWriter:START-DOCUMENT () . 
        hSaxWriter:START-ELEMENT ("testsuites":U).
        
        FOR EACH eTestsuite ON ERROR UNDO, THROW:

            hSaxWriter:START-ELEMENT ("testsuite":U).
            hSaxWriter:INSERT-ATTRIBUTE ("name":U,      eTestsuite.TestsuiteName) .
            hSaxWriter:INSERT-ATTRIBUTE ("tests":U,     STRING (eTestsuite.Tests)) .
            hSaxWriter:INSERT-ATTRIBUTE ("failures":U,  STRING (eTestsuite.Failures)) .
            hSaxWriter:INSERT-ATTRIBUTE ("timestamp":U, ISO-DATE (eTestsuite.TimeStamp)) .
            
            FOR EACH eTestcase OF eTestsuite ON ERROR UNDO, THROW: 

                IF eTestcase.ErrorMessage > "":U OR eTestcase.ErrorType > "":U OR eTestcase.ErrorStacktrace > "":U THEN DO:
                    hSaxWriter:START-ELEMENT ("testcase":U).
    
                    hSaxWriter:INSERT-ATTRIBUTE ("name":U,      eTestcase.TestcaseName) .
                    hSaxWriter:INSERT-ATTRIBUTE ("classname":U, eTestcase.ClassName) .
                    hSaxWriter:INSERT-ATTRIBUTE ("time":U, STRING (eTestcase.ExecutionTime)) .

                    hSaxWriter:START-ELEMENT ("error":U).
    
                    IF eTestcase.ErrorMessage > "":U THEN 
                        hSaxWriter:INSERT-ATTRIBUTE ("message":U, eTestcase.ErrorMessage) .
                    IF eTestcase.ErrorType > "":U THEN 
                        hSaxWriter:INSERT-ATTRIBUTE ("type":U, eTestcase.ErrorType) .
                    hSAXWriter:WRITE-CHARACTERS (eTestcase.ErrorStacktrace) .

                    hSaxWriter:END-ELEMENT ("error":U).
                    
                    hSaxWriter:END-ELEMENT ("testcase":U).
                END.
                ELSE IF eTestcase.FailureMessage > "":U OR eTestcase.FailureType > "":U OR eTestcase.FailureStacktrace > "":U THEN DO:
                    hSaxWriter:START-ELEMENT ("testcase":U).
    
                    hSaxWriter:INSERT-ATTRIBUTE ("name":U,      eTestcase.TestcaseName) .
                    hSaxWriter:INSERT-ATTRIBUTE ("classname":U, eTestcase.ClassName) .
                    hSaxWriter:INSERT-ATTRIBUTE ("time":U, STRING (eTestcase.ExecutionTime)) .

                    hSaxWriter:START-ELEMENT ("failure":U).
    
                    IF eTestcase.ErrorMessage > "":U THEN 
                        hSaxWriter:INSERT-ATTRIBUTE ("message":U, eTestcase.FailureMessage) .
                    IF eTestcase.ErrorType > "":U THEN 
                        hSaxWriter:INSERT-ATTRIBUTE ("type":U, eTestcase.FailureType) .
                    hSAXWriter:WRITE-CHARACTERS (eTestcase.FailureStacktrace) .

                    hSaxWriter:END-ELEMENT ("failure":U).
                    
                    hSaxWriter:END-ELEMENT ("testcase":U).
                END.
                ELSE DO ON ERROR UNDO, THROW:
                    
                    CREATE SAX-ATTRIBUTES hAttributes .

                    hAttributes:INSERT-ATTRIBUTE ("name":U,      eTestcase.TestcaseName) .
                    hAttributes:INSERT-ATTRIBUTE ("classname":U, eTestcase.ClassName) .
                    hAttributes:INSERT-ATTRIBUTE ("time":U, TRIM(STRING (eTestcase.ExecutionTime,
                                                                         ">>>>>>>>>>>>>>9.999":U))) .
                    
                    hSaxWriter:WRITE-EMPTY-ELEMENT ("testcase":U, ?, hAttributes).
                    
                    FINALLY:
                        GarbageCollectorHelper:DeleteObject (hAttributes) .        
                    END FINALLY.
                END.                
            END.

            IF LENGTH (eTestsuite.SystemOut) > 0 THEN DO ON ERROR UNDO, THROW:
                lcOutput = eTestsuite.SystemOut .
                lcOutput = THIS-OBJECT:ReplaceInvalidXmlCharacter (lcOutput) .
                 
                hSaxWriter:START-ELEMENT ("system-out":U).
                hSAXWriter:WRITE-CHARACTERS (lcOutput) .
                hSaxWriter:END-ELEMENT ("system-out":U).
            END.

            IF LENGTH (eTestsuite.SystemErr) > 0 THEN DO ON ERROR UNDO, THROW:
                lcOutput = eTestsuite.SystemErr . 
                lcOutput = THIS-OBJECT:ReplaceInvalidXmlCharacter (lcOutput) .

                hSaxWriter:START-ELEMENT ("system-err":U).
                hSAXWriter:WRITE-CHARACTERS (lcOutput) .
                hSaxWriter:END-ELEMENT ("system-err":U).
            END.

            hSaxWriter:END-ELEMENT ("testsuite":U) .
            
        END.

        hSaxWriter:END-ELEMENT ("testsuites":U) .

        hSaxWriter:END-DOCUMENT ().
        
        FINALLY:
            GarbageCollectorHelper:DeleteObject (hSaxWriter) .     
            
            Consultingwerk.Util.SessionHelper:SetNumericFormat (oFormat) .   
        END FINALLY.
        
    END METHOD .

END CLASS.
