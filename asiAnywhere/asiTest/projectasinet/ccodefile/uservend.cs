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
/// Summary description for UserVend
/// </summary>
[System.ComponentModel.DataObject]
public class UserVend : AppServerConnect.AppServer
{
	public UserVend()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserVendDataSet SelectUserVend(string prmUser, string prmAction)
    {
        
        dsUserVendDataSet dsUserVend = new dsUserVendDataSet();
        dsUserVend = null;
        AppServerConnect();
        aoObject.UserVend(prmUser, prmAction, "",  ref dsUserVend);
        AppServerDisconnect();
        
        return dsUserVend;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUserVendDataSet SelectUserVend(string prmUser, string prmAction, string prmItem )
    {
        
        dsUserVendDataSet dsUserVend = new dsUserVendDataSet();
        dsUserVend = null;
        
        AppServerConnect();
        aoObject.UserVend(prmUser, prmAction, prmItem,  ref dsUserVend);
        AppServerDisconnect();
        HttpContext.Current.Cache.Insert("VendData", dsUserVend, null, DateTime.Now.AddMinutes(10), System.Web.Caching.Cache.NoSlidingExpiration);
        return dsUserVend;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
    public dsUserVendDataSet UpdateUserVend(string prmUser, VendData itData)
    {
        dsUserVendDataSet dsUserVendChange = new dsUserVendDataSet();
        dsUserVendDataSet dsUserVend = new dsUserVendDataSet();
        dsUserVend = null;
        dsUserVend = (dsUserVendDataSet)HttpContext.Current.Cache.Get("VendData");
        
        dsUserVend.ttUserVend[0].vend_default = itData.vend_default;
        dsUserVendChange = (dsUserVendDataSet)dsUserVend.GetChanges();
        AppServerConnect();
        aoObject.UserVend(prmUser, "Update", "",  ref dsUserVend);
        AppServerDisconnect();

        return dsUserVend;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Delete, true)]
    public dsUserVendDataSet DeleteUserVend(string prmUser, VendData itData)
    {
        dsUserVendDataSet dsUserVendChange = new dsUserVendDataSet();
        dsUserVendDataSet dsUserVend = new dsUserVendDataSet();
        dsUserVend = null;
        dsUserVend = (dsUserVendDataSet)HttpContext.Current.Cache.Get("VendData");
        dsUserVend.ttUserVend[0].Delete();
        dsUserVendChange = (dsUserVendDataSet)dsUserVend.GetChanges();
        AppServerConnect();
        aoObject.UserVend(prmUser, "Delete", "",  ref dsUserVendChange);
        AppServerDisconnect();

        return dsUserVend;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Insert, true)]
    public dsUserVendDataSet InsertUserVend(string prmUser, VendData itData)
    {
        dsUserVendDataSet dsUserVend = new dsUserVendDataSet();
        dsUserVendDataSet.ttUserVendRow dsUserVendRow;

        //dsUserVend = null;
        dsUserVendRow = dsUserVend.ttUserVend.NewttUserVendRow();
        dsUserVendRow.vend_no = itData.vend_no;
        dsUserVendRow.vend_default = itData.vend_default;
        dsUserVend.ttUserVend.AddttUserVendRow(dsUserVendRow);
        
        AppServerConnect();
        aoObject.UserVend(prmUser, "Insert", "",  ref dsUserVend);
        AppServerDisconnect();

        return dsUserVend;
    }
}

public class VendData
{
        
    private string _vend_no;
    private bool _vend_default;
    
    public string vend_no
    {
        get { return _vend_no; }
        set { _vend_no = value; }
    }
    
    public bool vend_default
    {
        get { return _vend_default; }
        set { _vend_default = value; }
    }
public VendData()
    {
    }
}
