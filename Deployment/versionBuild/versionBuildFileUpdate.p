/* Read the version number from the text file created by buildVersion.bat */
DEF VAR cNewVer AS CHAR NO-UNDO.
IF SEARCH("c:\asigui\build\newVer.txt") EQ ? THEN DO:
    MESSAGE
        "Unable to locate c:\asigui\build\newVer.txt" SKIP
        "No file updates performed."
        VIEW-AS ALERT-BOX.
    RETURN.
END.
INPUT FROM c:\asigui\build\newVer.txt.
IMPORT cNewVer.
INPUT CLOSE.

/* Create an EMPTY HotfixList.txt file in Env/Override and Oversource */
OUTPUT TO VALUE("c:\asigui\environments\" + cNewVer + "\Override\HotfixList.txt").
PUT UNFORMATTED "Hotfix List (Override directory)                                                           " + CHR(10).
PUT UNFORMATTED "-------------------------------------------------------------------------------------------" + CHR(10).
PUT UNFORMATTED "Ticket  Date        Description                                     Program                " + CHR(10).
PUT UNFORMATTED "-------------------------------------------------------------------------------------------" + CHR(10).
OUTPUT CLOSE.
IF SEARCH("c:\asigui\environments\" + cNewVer + "\Override\HotfixList.txt") EQ ? THEN DO:
    MESSAGE
        "Unable to write Oversource\HotfixList.txt"
        VIEW-AS ALERT-BOX.
    RETURN.
END.

OUTPUT TO VALUE("c:\asigui\environments\" + cNewVer + "\Oversource\HotfixList.txt").
PUT UNFORMATTED "Hotfix List (Override directory)                                                           " + CHR(10).
PUT UNFORMATTED "-------------------------------------------------------------------------------------------" + CHR(10).
PUT UNFORMATTED "Ticket  Date        Description                                     Program                " + CHR(10).
PUT UNFORMATTED "-------------------------------------------------------------------------------------------" + CHR(10).
OUTPUT CLOSE.
IF SEARCH("c:\asigui\environments\" + cNewVer + "\Oversource\HotfixList.txt") EQ ? THEN DO:
    MESSAGE
        "Unable to write Override\HotfixList.txt"
        VIEW-AS ALERT-BOX.
    RETURN.
END.

OUTPUT TO VALUE("c:\asigui\environments\" + cNewVer + "\Source\system\sysconst.i").
PUT UNFORMATTED "/* WFK - 3/25/16 - Include to store constants for system programs */" + CHR(10).
PUT UNFORMATTED "&Global-define awversion " + cNewVer + CHR(10).
PUT UNFORMATTED "&Global-define EulaFile Eula.txt" + CHR(10).
OUTPUT CLOSE.

OS-COPY VALUE("c:\asigui\environments\devel\Programs\DataDigger\DataDigger-jayfarr.ini") VALUE("c:\asigui\environments\" + cNewVer + "\DataDigger\DataDigger.ini").

/* Create fsinstaller config files for 'Normal' and 'Override' compiles */
OUTPUT TO VALUE("c:\asigui\build\fsInstaller\AW" + 
                    SUBSTRING(cNewVer,1,2) + "_" +
                    SUBSTRING(cNewVer,4,2) + "_" +
                    SUBSTRING(cNewVer,7,2) + ".cfg").
PUT UNFORMATTED
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"yes" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"C:\Asigui\Build\fsInstaller\" + CHR(10) +
"C:\ASIgui\Databases\Comp\" + CHR(10) +
"Comp" + SUBSTRING(cNewVer,1,2) + SUBSTRING(cNewVer,4,2) + SUBSTRING(cNewVer,7,2) + "d.db" + CHR(10) +
"-ld asi -1" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source\" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Programs\" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-c:\asigui\databases\comp\" + CHR(10) +
"x-Comp" + SUBSTRING(cNewVer,1,2) + SUBSTRING(cNewVer,4,2) + SUBSTRING(cNewVer,7,2) + "a.db" + CHR(10) +
"x--ld audit -1" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source," +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Resources," + 
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source\Addon," +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source\datadigger," + 
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source\querybuilder," +
"C:\Progress\OE116_64\gui,C:\Progress\OE116_64\gui\ablunit.pl,C:\Progress\OE116_64\gui\adecomm.pl,C:\Progress\OE116_64\gui\adecomp.pl," +
"C:\Progress\OE116_64\gui\adedesk.pl,C:\Progress\OE116_64\gui\adedict.pl,C:\Progress\OE116_64\gui\adeedit.pl,C:\Progress\OE116_64\gui\adeicon.pl," +
"C:\Progress\OE116_64\gui\aderes.pl,C:\Progress\OE116_64\gui\adeshar.pl,C:\Progress\OE116_64\gui\adeuib.pl,C:\Progress\OE116_64\gui\adeweb.pl," +
"C:\Progress\OE116_64\gui\adexml.pl,C:\Progress\OE116_64\gui\dataadmin.pl,C:\Progress\OE116_64\gui\OpenEdge.BusinessLogic.pl," +
"C:\Progress\OE116_64\gui\OpenEdge.Core.pl,C:\Progress\OE116_64\gui\OpenEdge.ServerAdmin.pl,C:\Progress\OE116_64\gui\prodict.pl," +
"C:\Progress\OE116_64\gui\protools.pl,C:\Progress\OE116_64,C:\Progress\OE116_64\bin" + CHR(10) +
"." + CHR(10).
OUTPUT CLOSE.

OUTPUT TO VALUE("c:\asigui\build\fsInstaller\AW" + 
                    SUBSTRING(cNewVer,1,2) + "_" +
                    SUBSTRING(cNewVer,4,2) + "_" +
                    SUBSTRING(cNewVer,7,2) + "Over.cfg").
PUT UNFORMATTED
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"yes" + CHR(10) +
"no" + CHR(10) +
"yes" + CHR(10) +
"C:\Asigui\Build\fsInstaller\" + CHR(10) +
"C:\ASIgui\Databases\Comp\" + CHR(10) +
"Comp" + SUBSTRING(cNewVer,1,2) + SUBSTRING(cNewVer,4,2) + SUBSTRING(cNewVer,7,2) + "d.db" + CHR(10) +
"-ld asi -1" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Oversource\" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Override\" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-c:\asigui\databases\comp\" + CHR(10) +
"x-Comp" + SUBSTRING(cNewVer,1,2) + SUBSTRING(cNewVer,4,2) + SUBSTRING(cNewVer,7,2) + "a.db" + CHR(10) +
"x--ld audit -1" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"x-" + CHR(10) +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Oversource," +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Oversource\Addon," +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source," +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Resources," + 
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source\Addon," +
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source\datadigger," + 
"C:\ASIgui\Environments\" + SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + "\Source\querybuilder," +
"C:\Progress\OE116_64\gui,C:\Progress\OE116_64\gui\ablunit.pl,C:\Progress\OE116_64\gui\adecomm.pl,C:\Progress\OE116_64\gui\adecomp.pl," +
"C:\Progress\OE116_64\gui\adedesk.pl,C:\Progress\OE116_64\gui\adedict.pl,C:\Progress\OE116_64\gui\adeedit.pl,C:\Progress\OE116_64\gui\adeicon.pl," +
"C:\Progress\OE116_64\gui\aderes.pl,C:\Progress\OE116_64\gui\adeshar.pl,C:\Progress\OE116_64\gui\adeuib.pl,C:\Progress\OE116_64\gui\adeweb.pl," +
"C:\Progress\OE116_64\gui\adexml.pl,C:\Progress\OE116_64\gui\dataadmin.pl,C:\Progress\OE116_64\gui\OpenEdge.BusinessLogic.pl," +
"C:\Progress\OE116_64\gui\OpenEdge.Core.pl,C:\Progress\OE116_64\gui\OpenEdge.ServerAdmin.pl,C:\Progress\OE116_64\gui\prodict.pl," +
"C:\Progress\OE116_64\gui\protools.pl,C:\Progress\OE116_64,C:\Progress\OE116_64\bin" + CHR(10) +
"." + CHR(10).
OUTPUT CLOSE.

/* Read advantzware.ini file to update envList and dbList variables */
DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cPortList AS CHAR NO-UNDO.
DEF VAR iMinPort AS INT NO-UNDO INITIAL 2700.
DEF VAR iMaxPort AS INT NO-UNDO INITIAL 2500.
DEF VAR iUsePort AS INT NO-UNDO.

DEF TEMP-TABLE ttFile
    FIELD iseq AS INT
    FIELD cvalue AS CHAR.
DEF TEMP-TABLE ttUsedPorts
    FIELD iPortNo AS INT.
INPUT FROM c:\asigui\admin\advantzware.ini.
REPEAT:
    IMPORT UNFORMATTED cLine.
    ASSIGN iCtr = iCtr + 1.
    CREATE ttFile.
    ASSIGN
        ttFile.iseq = iCtr
        ttFile.cvalue = cLine.
END.
INPUT CLOSE.

DO:
    /* If already updated, skip this whole operation */
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "envList".
    IF INDEX(ttFile.cValue,cNewVer) NE 0 THEN DO:
        QUIT.
    END.

    /* Find an available port number */
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "dbPortList".
    ASSIGN
        cPortList = ENTRY(2,ttFile.cValue,"=").
    DO iCtr = 1 TO NUM-ENTRIES(cPortList):
        CREATE ttUsedPorts.
        ASSIGN
            ttUsedPorts.iPortNo = integer(ENTRY(iCtr,cPortList,",")).
    END.
    FOR EACH ttUsedPorts:
        IF ttUsedPorts.iPortNo LT iMinPort THEN ASSIGN iMinPort = ttUsedPorts.iPortNo.
        IF ttUsedPorts.iPortNo GT iMaxPort THEN ASSIGN iMaxPort = ttUsedPorts.iPortNo.
    END.    
    DO iCtr = iMinPort TO iMaxPort:
        IF NOT CAN-FIND(FIRST ttUsedPorts WHERE
                        ttUsedPorts.iPortNo EQ iCtr) THEN DO:
            ASSIGN
                iUsePort = iCtr.
            LEAVE.
        END.
    END.
    MESSAGE
        "Pre-assigning ports for these databases in advantzware.ini:" SKIP
        "  Data DB = " + STRING(iUsePort,"9999") SKIP
        "  Audit DB = "  + STRING(iUsePort + 100,"9999") SKIP
        "Please make a note of them."
        VIEW-AS ALERT-BOX.
    
    /* Update envList vars */
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "envList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," +
                        cNewVer + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "envVerList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," +
                        cNewVer + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    
    /* Update dbList vars */
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "dbList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," +
                        "TEST" + SUBSTRING(cNewVer,1,2) + SUBSTRING(cNewVer,4,2) + SUBSTRING(cNewVer,7,2) + "d" +
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "dbVerList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," + 
                        SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "dbDirList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," + 
                        "TEST" + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "dbPortList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," + 
                        STRING(iUsePort,"9999") + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    
    
    /* Update audList vars */
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "audDbList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," + 
                        "TEST" + SUBSTRING(cNewVer,1,2) + SUBSTRING(cNewVer,4,2) + SUBSTRING(cNewVer,7,2) + "a" +
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "audVerList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," +
                        SUBSTRING(cNewVer,1,2) + "." + SUBSTRING(cNewVer,4,2) + "." + SUBSTRING(cNewVer,7,2) + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "audDirList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," +
                        "TEST" + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").
    FIND FIRST ttFile WHERE 
        INDEX(ttFile.cValue,"=") NE 0 AND
        ENTRY(1,ttFile.cValue,"=") EQ "audPortList".
    ASSIGN
        ttFile.cValue = ENTRY(1,ttFile.cValue,",") + "," +
                        STRING(iUsePort + 100,"9999") + 
                        REPLACE(ttFile.cValue,ENTRY(1,ttFile.cValue,","),"").

    /* Now write out the changed file */
    OUTPUT TO c:\asigui\admin\advantzware.ini.
    FOR EACH ttFile BY ttFile.iSeq:
        PUT UNFORMATTED ttFile.cValue + CHR(10).
    END.
    OUTPUT CLOSE.

END.
QUIT.    
    
                                                    

