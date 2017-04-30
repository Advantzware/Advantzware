set DUMP_INC_DFFILE=.\wrk\delta.df 
set DUMP_INC_CODEPAGE=1252 
set DUMP_INC_INDEXMODE=active 
set DUMP_INC_RENAMEFILE=.\wrk\renamefile.rf 
set DUMP_INC_DEBUG=0 
set PROPATH=.\prodict
%DLC%/bin/_progres -b -db emptrack -1 -db Q:\Data\Progress\asi10test\db1010\addon\emptrack -ld slave -p prodict/dump_inc.p > dump_inc.log 