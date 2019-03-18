FOR EACH _file WHERE _file-name < "_" AND _owner = "pub".
    DISP _file-name _owner.                              
                                    
    RUN r:\asi_gui9\SOURCE\util\deltable.p _file-name.
                                      
END. 
