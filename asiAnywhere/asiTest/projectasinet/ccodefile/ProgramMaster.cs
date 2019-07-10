using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;
using Progress.Open4GL.Proxy;

/// <summary>
/// Summary description for ProgramMaster
/// </summary>
[System.ComponentModel.DataObject]
public class ProgramMaster : AppServerConnect.AppServer
{
	public ProgramMaster()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsProgDataSet SelectProgramMasterlist(string prmAction, string programName, string title, string viewId, string addId, string updateId, string deleteId)
    {
        
        dsProgDataSet dsProg = new dsProgDataSet();
        dsProg = null;
        AppServerConnect();
        aoObject.ProgramMaster(prmAction, "",programName, title, viewId, addId, updateId, deleteId, ref dsProg);
        AppServerDisconnect();
        
        return dsProg;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsProgDataSet SelectProgramMaster(string prmAction, string prmItem)
    {
        
        dsProgDataSet dsProg = new dsProgDataSet();
        dsProg = null;
        
        AppServerConnect();
        aoObject.ProgramMaster(prmAction, prmItem, "", "", "", "", "", "", ref dsProg);
        AppServerDisconnect();
        HttpContext.Current.Cache.Insert("ProgData", dsProg, null, DateTime.Now.AddMinutes(10), System.Web.Caching.Cache.NoSlidingExpiration);
        return dsProg;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
    public dsProgDataSet UpdateProgramMaster(ProgData itData)
    {
        dsProgDataSet dsProgChange = new dsProgDataSet();
        dsProgDataSet dsProg = new dsProgDataSet();
        dsProg = null;
        dsProg = (dsProgDataSet)HttpContext.Current.Cache.Get("ProgData");
        dsProg.ttProg[0].prgtitle = itData.prgtitle;
        dsProg.ttProg[0].dir_group = itData.dir_group;
        dsProg.ttProg[0].prgm_ver = itData.prgm_ver;
        dsProg.ttProg[0].menu_item = itData.menu_item;
        dsProg.ttProg[0].popup = itData.popup;
        dsProg.ttProg[0].run_persistent = itData.run_persistent;
        dsProg.ttProg[0].track_usage = itData.track_usage;
        dsProg.ttProg[0].can_run = itData.can_run;
        dsProg.ttProg[0].can_create = itData.can_create;
        dsProg.ttProg[0].can_update = itData.can_update;
        dsProg.ttProg[0].can_delete = itData.can_delete;
        dsProg.ttProg[0].mfgroup = itData.mfgroup;
        dsProgChange = (dsProgDataSet)dsProg.GetChanges();
        AppServerConnect();
        aoObject.ProgramMaster("Update", "", "", "", "", "", "", "", ref dsProg);
        AppServerDisconnect();

        return dsProg;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Delete, true)]
    public dsProgDataSet DeleteProgramMaster(ProgData itData)
    {
        dsProgDataSet dsProgChange = new dsProgDataSet();
        dsProgDataSet dsProg = new dsProgDataSet();
        dsProg = null;
        dsProg = (dsProgDataSet)HttpContext.Current.Cache.Get("ProgData");
        dsProg.ttProg[0].Delete();
        dsProgChange = (dsProgDataSet)dsProg.GetChanges();
        AppServerConnect();
        aoObject.ProgramMaster("Delete", "", "", "", "", "", "", "", ref dsProgChange);
        AppServerDisconnect();

        return dsProg;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Insert, true)]
    public dsProgDataSet InsertProgramMaster(ProgData itData)
    {
        dsProgDataSet dsProg = new dsProgDataSet();
        dsProgDataSet.ttProgRow dsProgRow;

        //dsProg = null;
        dsProgRow = dsProg.ttProg.NewttProgRow();
        dsProgRow.prgmname = itData.prgmname;
        dsProgRow.prgtitle = itData.prgtitle;
        dsProgRow.dir_group = itData.dir_group;
        dsProgRow.prgm_ver = itData.prgm_ver;
        dsProgRow.menu_item = itData.menu_item;
        dsProgRow.popup = itData.popup;
        dsProgRow.run_persistent = itData.run_persistent;
        dsProgRow.track_usage = itData.track_usage;
        dsProgRow.can_run = itData.can_run;
        dsProgRow.can_create = itData.can_create;
        dsProgRow.can_update = itData.can_update;
        dsProgRow.can_delete = itData.can_delete;
        dsProgRow.mfgroup = itData.mfgroup;
        dsProg.ttProg.AddttProgRow(dsProgRow);
        
        AppServerConnect();
        aoObject.ProgramMaster("Insert", "", "", "", "", "", "", "", ref dsProg);
        AppServerDisconnect();

        return dsProg;
    }
}

public class ProgData
{
    private string _prgmname;
    private string _prgtitle;
    private string _dir_group;
    private string _prgm_ver;
    private bool _menu_item;
    private bool _popup;
    private bool _run_persistent;
    private bool _track_usage;
    private string _can_run;
    private string _can_create;
    private string _can_update;
    private string _can_delete;
    private string _mfgroup;

    public string prgmname
    {
        get { return _prgmname; }
        set { _prgmname = value; }
    }

    public string prgtitle
    {
        get { return _prgtitle; }
        set { _prgtitle = value; }
    }

    public string dir_group
    {
        get { return _dir_group; }
        set { _dir_group = value; }
    }
    public string prgm_ver
    {
        get { return _prgm_ver; }
        set { _prgm_ver = value; }
    }
    public bool menu_item
    {
        get { return _menu_item; }
        set { _menu_item = value; }
    }
    public bool popup
    {
        get { return _popup; }
        set { _popup = value; }
    }
    public bool run_persistent
    {
        get { return _run_persistent; }
        set { _run_persistent = value; }
    }
    public bool track_usage
    {
        get { return _track_usage; }
        set { _track_usage = value; }
    }



    public string can_run
    {
        get { return _can_run; }
        set { _can_run = value; }
    }

    public string can_create
    {
        get { return _can_create; }
        set { _can_create = value; }
    }

    public string can_update
    {
        get { return _can_update; }
        set { _can_update = value; }
    }

    public string can_delete
    {
        get { return _can_delete; }
        set { _can_delete = value; }
    }

    public string mfgroup
    {
        get { return _mfgroup; }
        set { _mfgroup = value; }
    }

    
    public ProgData()
    {
    }
}
