/* deleteSmartMenu.p */

DEFINE VARIABLE cRootMenuGuid   AS CHARACTER NO-UNDO .

FIND SmartModule NO-LOCK WHERE SmartModule.ModuleName EQ "legacy" NO-ERROR .
IF NOT AVAILABLE SmartModule THEN RETURN .

/* root menu structrue */
FIND SmartMenu NO-LOCK
     WHERE SmartMenu.ParentMenuGuid EQ ""
       AND SmartMenu.MenuName       EQ "Advantzware"
     NO-ERROR .
IF NOT AVAILABLE SmartMenu THEN RETURN .

ASSIGN cRootMenuGuid = SmartMenu.MenuGuid .

for each smartfunction
    where smartfunction.functiondescription eq "imported menu function"
    :
    delete smartfunction.
end.

run menuItem (crootmenuguid).

procedure menuItem:
    def input param ipcguid as char no-undo.
    
    def buffer bsmartmenu for smartmenu.
    
    for each bsmartmenu 
        where bsmartmenu.parentmenuguid eq ipcguid
        :
        if bsmartmenu.menustructuretype eq "menu" then
        run menuItem (bsmartmenu.menuguid).
        delete bsmartmenu.
    end.
end procedure.
