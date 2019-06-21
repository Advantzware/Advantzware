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
/// Summary description for shipnotes
/// </summary>
[System.ComponentModel.DataObject]
public class shipnotes : AppServerConnect.AppServer
{
    public shipnotes()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsshipnotesDataSet SelectShipNotes(string prmUser, string prmAction, string prmOrderNum, string prmItemNum, string prmNote1, string prmNote2, string prmNote3, string prmNote4)
    {

        dsshipnotesDataSet dsshipnotes = new dsshipnotesDataSet();
        dsshipnotes = null;
        AppServerConnect();
        aoObject.shipnotes(prmUser, prmAction, prmOrderNum, prmItemNum, prmNote1, prmNote2, prmNote3, prmNote4, ref dsshipnotes);
        AppServerDisconnect();

        return dsshipnotes;
    }


//    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
//    public dsshipnotesDataSet Updateshipnotes(ShipNoteData itData)
//    {
//        dsshipnotesDataSet dsshipnotesChange = new dsshipnotesDataSet();
//        dsshipnotesDataSet dsshipnotes = new dsshipnotesDataSet();
//        dsshipnotes = null;
//        dsshipnotes = (dsshipnotesDataSet)HttpContext.Current.Cache.Get("ShipNoteData");
//        dsshipnotes.ttshipnotes[0].SNote1 = itData.SNote1;
//        dsshipnotes.ttshipnotes[0].SNote2 = itData.SNote2;
//        dsshipnotes.ttshipnotes[0].SNote3 = itData.SNote3;
//        dsshipnotes.ttshipnotes[0].SNote4 = itData.SNote4;

//        dsshipnotesChange = (dsshipnotesDataSet)dsshipnotes.GetChanges();
//        AppServerConnect();
//        aoObject.shipnotes("", "", "", "update", "", ref dsshipnotes);
//        AppServerDisconnect();

//        return dsshipnotes;
//    }
   
//    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Delete, true)]
//    //public dsshipnotesDataSet DeleteProgramMaster(ShipNote itData)
//    //{
//    //    dsshipnotesDataSet dsshipnotesChange = new dsshipnotesDataSet();
//    //    dsshipnotesDataSet dsshipnotes = new dsshipnotesDataSet();
//    //    dsshipnotes = null;
//    //    dsshipnotes = (dsshipnotesDataSet)HttpContext.Current.Cache.Get("ShipNote");
//    //    dsshipnotes.ttProg[0].Delete();
//    //    dsshipnotesChange = (dsshipnotesDataSet)dsshipnotes.GetChanges();
//    //    AppServerConnect();
//    //    aoObject.ProgramMaster("Delete", "", "", "", "", "", "", "", ref dsshipnotesChange);
//    //    AppServerDisconnect();

//    //    return dsshipnotes;
//    //}

//    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Insert, true)]
//    //public dsshipnotesDataSet InsertProgramMaster(ShipNote itData)
//    //{
//    //    dsshipnotesDataSet dsshipnotes = new dsshipnotesDataSet();
//    //    dsshipnotesDataSet.ttProgRow dsshipnotesRow;

//    //    //dsshipnotes = null;
//    //    dsshipnotesRow = dsshipnotes.ttProg.NewttProgRow();
//    //    dsshipnotesRow.prgmname = itData.prgmname;
//    //    dsshipnotesRow.prgtitle = itData.prgtitle;
//    //    dsshipnotesRow.dir_group = itData.dir_group;
//    //    dsshipnotesRow.prgm_ver = itData.prgm_ver;
//    //    dsshipnotesRow.menu_item = itData.menu_item;
//    //    dsshipnotesRow.popup = itData.popup;
//    //    dsshipnotesRow.run_persistent = itData.run_persistent;
//    //    dsshipnotesRow.track_usage = itData.track_usage;
//    //    dsshipnotesRow.can_run = itData.can_run;
//    //    dsshipnotesRow.can_create = itData.can_create;
//    //    dsshipnotesRow.can_update = itData.can_update;
//    //    dsshipnotesRow.can_delete = itData.can_delete;
//    //    dsshipnotesRow.mfgroup = itData.mfgroup;
//    //    dsshipnotes.ttProg.AddttProgRow(dsshipnotesRow);

//    //    AppServerConnect();
//    //    aoObject.ProgramMaster("Insert", "", "", "", "", "", "", "", ref dsshipnotes);
//    //    AppServerDisconnect();

//    //    return dsshipnotes;
//    //}

//}
//    public class ShipNoteData
//    {
       

//        private string _SNote1;
//        private string _SNote2;
//        private string _SNote3;
//        private string _SNote4;
       


//        public string SNote1
//        {
//            get { return _SNote1; }
//            set { _SNote1 = value; }
//        }
//        public string SNote2
//        {
//            get { return _SNote2; }
//            set { _SNote2 = value; }
//        }
//        public string SNote3
//        {
//            get { return _SNote3; }
//            set { _SNote3 = value; }
//        }
//        public string SNote4
//        {
//            get { return _SNote4; }
//            set { _SNote4 = value; }
//        }

        
//        public ShipNoteData()
//        {
//        }
    }

