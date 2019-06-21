using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Text;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfqitem : System.Web.UI.Page
{

    public rfqitem()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        
        UserClass.CheckLogin(Page);

        //ImageButton brwsorder = (ImageButton)Master.FindControl("rfq_item");
        //brwsorder.ImageUrl = "~/Images/view item 1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Rfq Item";

        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "";

        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;


        // FormView1.ChangeMode(FormViewMode.ReadOnly);   
        

        if (!Page.IsPostBack)
        {
            if (Request.QueryString["rfqitmmod"] == "insert")
            {
                FormView1.ChangeMode(FormViewMode.Insert);
            }
            if (Session["User"] != null)
            {

                string vUserId = UserLogin.UserName;
                string vPage = "rfqitem.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();

                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                if (vCanRun == true)
                {
                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }

        }
    }

    protected void Delete_RfqItem(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "rfqitem.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanDelete == false)
            {
                Response.Write("<script>alert('Sorry! You do not have permission Delete Record');</script>");
                Response.Write("<script>window.location.href = 'rfqitem.aspx';</script>");

            }
        }

        
        Label seqno = (Label)FormView1.FindControl("RfqSeqNoLabel");
        Label rfqno = (Label)FormView2.FindControl("aRfqNoLabel");             

        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "delete";
        ObjectDataSource2.SelectParameters["RfqSeqNo"].DefaultValue = seqno.Text.Trim();
        ObjectDataSource2.SelectParameters["prmRfqNo"].DefaultValue = rfqno.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);  
    }



    protected void updateRfqitem(object sender, EventArgs e)
    {

        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "UpdateRfqItem";
        Session["rfqseq"] = Session["rfqseq"];

        TextBox RfqQty = (TextBox)FormView1.FindControl("RfqQtyTextBox");
        TextBox RfqStock = (TextBox)FormView1.FindControl("RfqStockTextBox");
        TextBox RfqName = (TextBox)FormView1.FindControl("RfqNameTextBox");
        TextBox RfqPartno = (TextBox)FormView1.FindControl("RfqPartnoTextBox");
        TextBox Rfqstyle = (TextBox)FormView1.FindControl("RfqstyleTextBox");
        TextBox RfqProcat = (TextBox)FormView1.FindControl("RfqProcatTextBox");
        TextBox RfqCol = (TextBox)FormView1.FindControl("RfqColTextBox");
        TextBox RfqCoat = (TextBox)FormView1.FindControl("RfqCoatTextBox");
        TextBox RfqLength = (TextBox)FormView1.FindControl("RfqLengthTextBox");
        TextBox RfqWidth = (TextBox)FormView1.FindControl("RfqWidthTextBox");
        TextBox RfqDepth = (TextBox)FormView1.FindControl("RfqDepthTextBox");
        TextBox RfqBoard = (TextBox)FormView1.FindControl("RfqBoardTextBox");
        TextBox RfqCal = (TextBox)FormView1.FindControl("RfqCalTextBox");

        TextBox lv_qty_1 = (TextBox)FormView1.FindControl("lv_qty_1");
        TextBox lv_qty_2 = (TextBox)FormView1.FindControl("lv_qty_2");
        TextBox lv_qty_3 = (TextBox)FormView1.FindControl("lv_qty_3");
        TextBox lv_qty_4 = (TextBox)FormView1.FindControl("lv_qty_4");
        TextBox lv_qty_5 = (TextBox)FormView1.FindControl("lv_qty_5");
        TextBox lv_qty_6 = (TextBox)FormView1.FindControl("lv_qty_6");
        TextBox lv_qty_7 = (TextBox)FormView1.FindControl("lv_qty_7");
        TextBox lv_qty_8 = (TextBox)FormView1.FindControl("lv_qty_8");
        TextBox lv_qty_9 = (TextBox)FormView1.FindControl("lv_qty_9");
        TextBox lv_qty_10 = (TextBox)FormView1.FindControl("lv_qty_10");

        TextBox lv_delivery_1 = (TextBox)FormView1.FindControl("lv_delivery_1");
        TextBox lv_delivery_2 = (TextBox)FormView1.FindControl("lv_delivery_2");
        TextBox lv_delivery_3 = (TextBox)FormView1.FindControl("lv_delivery_3");
        TextBox lv_delivery_4 = (TextBox)FormView1.FindControl("lv_delivery_4");
        TextBox lv_delivery_5 = (TextBox)FormView1.FindControl("lv_delivery_5");
        TextBox lv_delivery_6 = (TextBox)FormView1.FindControl("lv_delivery_6");
        TextBox lv_delivery_7 = (TextBox)FormView1.FindControl("lv_delivery_7");
        TextBox lv_delivery_8 = (TextBox)FormView1.FindControl("lv_delivery_8");
        TextBox lv_delivery_9 = (TextBox)FormView1.FindControl("lv_delivery_9");
        TextBox lv_delivery_10 = (TextBox)FormView1.FindControl("lv_delivery_10");

        TextBox lv_price_1 = (TextBox)FormView1.FindControl("lv_price_1");
        TextBox lv_price_2 = (TextBox)FormView1.FindControl("lv_price_2");
        TextBox lv_price_3 = (TextBox)FormView1.FindControl("lv_price_3");
        TextBox lv_price_4 = (TextBox)FormView1.FindControl("lv_price_4");
        TextBox lv_price_5 = (TextBox)FormView1.FindControl("lv_price_5");
        TextBox lv_price_6 = (TextBox)FormView1.FindControl("lv_price_6");
        TextBox lv_price_7 = (TextBox)FormView1.FindControl("lv_price_7");
        TextBox lv_price_8 = (TextBox)FormView1.FindControl("lv_price_8");
        TextBox lv_price_9 = (TextBox)FormView1.FindControl("lv_price_9");
        TextBox lv_price_10 = (TextBox)FormView1.FindControl("lv_price_10");

        TextBox lv_uom_1 = (TextBox)FormView1.FindControl("lv_uom_1");
        TextBox lv_uom_2 = (TextBox)FormView1.FindControl("lv_uom_2");
        TextBox lv_uom_3 = (TextBox)FormView1.FindControl("lv_uom_3");
        TextBox lv_uom_4 = (TextBox)FormView1.FindControl("lv_uom_4");
        TextBox lv_uom_5 = (TextBox)FormView1.FindControl("lv_uom_5");
        TextBox lv_uom_6 = (TextBox)FormView1.FindControl("lv_uom_6");
        TextBox lv_uom_7 = (TextBox)FormView1.FindControl("lv_uom_7");
        TextBox lv_uom_8 = (TextBox)FormView1.FindControl("lv_uom_8");
        TextBox lv_uom_9 = (TextBox)FormView1.FindControl("lv_uom_9");
        TextBox lv_uom_10 = (TextBox)FormView1.FindControl("lv_uom_10");

        TextBox lv_date_1 = (TextBox)FormView1.FindControl("lv_date_1");
        TextBox lv_date_2 = (TextBox)FormView1.FindControl("lv_date_2");
        TextBox lv_date_3 = (TextBox)FormView1.FindControl("lv_date_3");
        TextBox lv_date_4 = (TextBox)FormView1.FindControl("lv_date_4");
        TextBox lv_date_5 = (TextBox)FormView1.FindControl("lv_date_5");
        TextBox lv_date_6 = (TextBox)FormView1.FindControl("lv_date_6");
        TextBox lv_date_7 = (TextBox)FormView1.FindControl("lv_date_7");
        TextBox lv_date_8 = (TextBox)FormView1.FindControl("lv_date_8");
        TextBox lv_date_9 = (TextBox)FormView1.FindControl("lv_date_9");
        TextBox lv_date_10 = (TextBox)FormView1.FindControl("lv_date_10");

        ObjectDataSource2.SelectParameters["RfqQty"].DefaultValue = RfqQty.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqName"].DefaultValue = RfqName.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqPartno"].DefaultValue = RfqPartno.Text.Trim();
        ObjectDataSource2.SelectParameters["Rfqstyle"].DefaultValue = Rfqstyle.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqProcat"].DefaultValue = RfqProcat.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqCol"].DefaultValue = RfqCol.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqCoat"].DefaultValue = RfqCoat.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqLength"].DefaultValue = RfqLength.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqWidth"].DefaultValue = RfqWidth.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqBoard"].DefaultValue = RfqBoard.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqDepth"].DefaultValue = RfqDepth.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqCal"].DefaultValue = RfqCal.Text.Trim();

        ObjectDataSource2.SelectParameters["lv_qty2"].DefaultValue = lv_qty_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty3"].DefaultValue = lv_qty_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty4"].DefaultValue = lv_qty_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty5"].DefaultValue = lv_qty_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty6"].DefaultValue = lv_qty_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty7"].DefaultValue = lv_qty_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty8"].DefaultValue = lv_qty_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty9"].DefaultValue = lv_qty_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_1"].DefaultValue = lv_delivery_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_2"].DefaultValue = lv_delivery_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_3"].DefaultValue = lv_delivery_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_4"].DefaultValue = lv_delivery_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_5"].DefaultValue = lv_delivery_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_6"].DefaultValue = lv_delivery_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_7"].DefaultValue = lv_delivery_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_8"].DefaultValue = lv_delivery_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_9"].DefaultValue = lv_delivery_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_10"].DefaultValue = lv_delivery_10.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_1"].DefaultValue = lv_price_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_2"].DefaultValue = lv_price_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_3"].DefaultValue = lv_price_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_4"].DefaultValue = lv_price_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_5"].DefaultValue = lv_price_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_6"].DefaultValue = lv_price_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_7"].DefaultValue = lv_price_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_8"].DefaultValue = lv_price_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_9"].DefaultValue = lv_price_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_10"].DefaultValue = lv_price_10.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_1"].DefaultValue = lv_uom_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_2"].DefaultValue = lv_uom_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_3"].DefaultValue = lv_uom_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_4"].DefaultValue = lv_uom_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_5"].DefaultValue = lv_uom_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_6"].DefaultValue = lv_uom_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_7"].DefaultValue = lv_uom_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_8"].DefaultValue = lv_uom_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_9"].DefaultValue = lv_uom_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_10"].DefaultValue = lv_uom_10.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_1"].DefaultValue = lv_date_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_2"].DefaultValue = lv_date_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_3"].DefaultValue = lv_date_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_4"].DefaultValue = lv_date_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_5"].DefaultValue = lv_date_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_6"].DefaultValue = lv_date_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_7"].DefaultValue = lv_date_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_8"].DefaultValue = lv_date_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_9"].DefaultValue = lv_date_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_10"].DefaultValue = lv_date_10.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    

    protected void rfq_estimate(object sender, EventArgs e)
    {

        Session["rfqestno"] = Session["rfqsnos"];
        Response.Redirect("rfq_estimate.aspx");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "rfqitem.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;
            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
        }
    }

    protected void Insertbutton_Click(object sender, EventArgs e)
    {      
        TextBox RfqQty = (TextBox)FormView1.FindControl("RfqQtyTextBox");
        TextBox RfqStock = (TextBox)FormView1.FindControl("RfqStockTextBox");
        TextBox RfqName = (TextBox)FormView1.FindControl("RfqNameTextBox");
        TextBox RfqPartno = (TextBox)FormView1.FindControl("RfqPartnoTextBox");
        TextBox Rfqstyle = (TextBox)FormView1.FindControl("RfqstyleTextBox");
        TextBox RfqProcat = (TextBox)FormView1.FindControl("RfqProcatTextBox");
        TextBox RfqCol = (TextBox)FormView1.FindControl("RfqColTextBox");
        TextBox RfqCoat = (TextBox)FormView1.FindControl("RfqCoatTextBox");
        TextBox RfqLength = (TextBox)FormView1.FindControl("RfqLengthTextBox");
        TextBox RfqWidth = (TextBox)FormView1.FindControl("RfqWidthTextBox");
        TextBox RfqDepth = (TextBox)FormView1.FindControl("RfqDepthTextBox");
        TextBox RfqBoard = (TextBox)FormView1.FindControl("RfqBoardTextBox");
        TextBox RfqCal = (TextBox)FormView1.FindControl("RfqCalTextBox");

        TextBox lv_qty_1 = (TextBox)FormView1.FindControl("lv_qty_1");
        TextBox lv_qty_2 = (TextBox)FormView1.FindControl("lv_qty_2");
        TextBox lv_qty_3 = (TextBox)FormView1.FindControl("lv_qty_3");
        TextBox lv_qty_4 = (TextBox)FormView1.FindControl("lv_qty_4");
        TextBox lv_qty_5 = (TextBox)FormView1.FindControl("lv_qty_5");
        TextBox lv_qty_6 = (TextBox)FormView1.FindControl("lv_qty_6");
        TextBox lv_qty_7 = (TextBox)FormView1.FindControl("lv_qty_7");
        TextBox lv_qty_8 = (TextBox)FormView1.FindControl("lv_qty_8");
        TextBox lv_qty_9 = (TextBox)FormView1.FindControl("lv_qty_9");
        TextBox lv_qty_10 = (TextBox)FormView1.FindControl("lv_qty_10");

        TextBox lv_delivery_1 = (TextBox)FormView1.FindControl("lv_delivery_1");
        TextBox lv_delivery_2 = (TextBox)FormView1.FindControl("lv_delivery_2");
        TextBox lv_delivery_3 = (TextBox)FormView1.FindControl("lv_delivery_3");
        TextBox lv_delivery_4 = (TextBox)FormView1.FindControl("lv_delivery_4");
        TextBox lv_delivery_5 = (TextBox)FormView1.FindControl("lv_delivery_5");
        TextBox lv_delivery_6 = (TextBox)FormView1.FindControl("lv_delivery_6");
        TextBox lv_delivery_7 = (TextBox)FormView1.FindControl("lv_delivery_7");
        TextBox lv_delivery_8 = (TextBox)FormView1.FindControl("lv_delivery_8");
        TextBox lv_delivery_9 = (TextBox)FormView1.FindControl("lv_delivery_9");
        TextBox lv_delivery_10 = (TextBox)FormView1.FindControl("lv_delivery_10");

        TextBox lv_price_1 = (TextBox)FormView1.FindControl("lv_price_1");
        TextBox lv_price_2 = (TextBox)FormView1.FindControl("lv_price_2");
        TextBox lv_price_3 = (TextBox)FormView1.FindControl("lv_price_3");
        TextBox lv_price_4 = (TextBox)FormView1.FindControl("lv_price_4");
        TextBox lv_price_5 = (TextBox)FormView1.FindControl("lv_price_5");
        TextBox lv_price_6 = (TextBox)FormView1.FindControl("lv_price_6");
        TextBox lv_price_7 = (TextBox)FormView1.FindControl("lv_price_7");
        TextBox lv_price_8 = (TextBox)FormView1.FindControl("lv_price_8");
        TextBox lv_price_9 = (TextBox)FormView1.FindControl("lv_price_9");
        TextBox lv_price_10 = (TextBox)FormView1.FindControl("lv_price_10");

        TextBox lv_uom_1 = (TextBox)FormView1.FindControl("lv_uom_1");
        TextBox lv_uom_2 = (TextBox)FormView1.FindControl("lv_uom_2");
        TextBox lv_uom_3 = (TextBox)FormView1.FindControl("lv_uom_3");
        TextBox lv_uom_4 = (TextBox)FormView1.FindControl("lv_uom_4");
        TextBox lv_uom_5 = (TextBox)FormView1.FindControl("lv_uom_5");
        TextBox lv_uom_6 = (TextBox)FormView1.FindControl("lv_uom_6");
        TextBox lv_uom_7 = (TextBox)FormView1.FindControl("lv_uom_7");
        TextBox lv_uom_8 = (TextBox)FormView1.FindControl("lv_uom_8");
        TextBox lv_uom_9 = (TextBox)FormView1.FindControl("lv_uom_9");
        TextBox lv_uom_10 = (TextBox)FormView1.FindControl("lv_uom_10");

        TextBox lv_date_1 = (TextBox)FormView1.FindControl("lv_date_1");
        TextBox lv_date_2 = (TextBox)FormView1.FindControl("lv_date_2");
        TextBox lv_date_3 = (TextBox)FormView1.FindControl("lv_date_3");
        TextBox lv_date_4 = (TextBox)FormView1.FindControl("lv_date_4");
        TextBox lv_date_5 = (TextBox)FormView1.FindControl("lv_date_5");
        TextBox lv_date_6 = (TextBox)FormView1.FindControl("lv_date_6");
        TextBox lv_date_7 = (TextBox)FormView1.FindControl("lv_date_7");
        TextBox lv_date_8 = (TextBox)FormView1.FindControl("lv_date_8");
        TextBox lv_date_9 = (TextBox)FormView1.FindControl("lv_date_9");
        TextBox lv_date_10 = (TextBox)FormView1.FindControl("lv_date_10");

        HiddenField hf1 = (HiddenField)FormView1.FindControl("HiddenField1");
        if (hf1.Value != "" && hf1.Value != "&nbsp;")
        {
            Session["my_new_est_no"] = "1";
        }
        else
        {
            Session["my_new_est_no"] = null;
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];


        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "AddRfqItem";
        ObjectDataSource2.SelectParameters["RfqQty"].DefaultValue = RfqQty.Text.Trim();
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["RfqName"].DefaultValue = RfqName.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqPartno"].DefaultValue = RfqPartno.Text.Trim();
        ObjectDataSource2.SelectParameters["Rfqstyle"].DefaultValue = Rfqstyle.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqProcat"].DefaultValue = RfqProcat.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqCol"].DefaultValue = RfqCol.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqCoat"].DefaultValue = RfqCoat.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqLength"].DefaultValue = RfqLength.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqWidth"].DefaultValue = RfqWidth.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqBoard"].DefaultValue = RfqBoard.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqDepth"].DefaultValue = RfqDepth.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqCal"].DefaultValue = RfqCal.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqStock"].DefaultValue = RfqStock.Text.Trim();

        ObjectDataSource2.SelectParameters["lv_qty2"].DefaultValue = lv_qty_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty3"].DefaultValue = lv_qty_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty4"].DefaultValue = lv_qty_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty5"].DefaultValue = lv_qty_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty6"].DefaultValue = lv_qty_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty7"].DefaultValue = lv_qty_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty8"].DefaultValue = lv_qty_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_qty9"].DefaultValue = lv_qty_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_1"].DefaultValue = lv_delivery_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_2"].DefaultValue = lv_delivery_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_3"].DefaultValue = lv_delivery_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_4"].DefaultValue = lv_delivery_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_5"].DefaultValue = lv_delivery_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_6"].DefaultValue = lv_delivery_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_7"].DefaultValue = lv_delivery_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_8"].DefaultValue = lv_delivery_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_9"].DefaultValue = lv_delivery_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_delivery_10"].DefaultValue = lv_delivery_10.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_1"].DefaultValue = lv_price_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_2"].DefaultValue = lv_price_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_3"].DefaultValue = lv_price_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_4"].DefaultValue = lv_price_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_5"].DefaultValue = lv_price_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_6"].DefaultValue = lv_price_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_7"].DefaultValue = lv_price_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_8"].DefaultValue = lv_price_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_9"].DefaultValue = lv_price_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_price_10"].DefaultValue = lv_price_10.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_1"].DefaultValue = lv_uom_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_2"].DefaultValue = lv_uom_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_3"].DefaultValue = lv_uom_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_4"].DefaultValue = lv_uom_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_5"].DefaultValue = lv_uom_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_6"].DefaultValue = lv_uom_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_7"].DefaultValue = lv_uom_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_8"].DefaultValue = lv_uom_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_9"].DefaultValue = lv_uom_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_uom_10"].DefaultValue = lv_uom_10.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_1"].DefaultValue = lv_date_1.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_2"].DefaultValue = lv_date_2.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_3"].DefaultValue = lv_date_3.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_4"].DefaultValue = lv_date_4.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_5"].DefaultValue = lv_date_5.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_6"].DefaultValue = lv_date_6.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_7"].DefaultValue = lv_date_7.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_8"].DefaultValue = lv_date_8.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_9"].DefaultValue = lv_date_9.Text.Trim();
        ObjectDataSource2.SelectParameters["lv_date_10"].DefaultValue = lv_date_10.Text.Trim();
        ObjectDataSource2.SelectParameters["RfqEstNo"].DefaultValue = hf1.Value;

        FormView1.ChangeMode(FormViewMode.ReadOnly);
          
        
    }
    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label seqno = (Label)FormView1.FindControl("RfqSeqNoLabel");
            Session["my_new_seq_no_gen"] = seqno.Text;

            Session["list_rfq_grid_seqno"] = Session["my_new_seq_no_gen"];
        }
        catch
        {
        }

    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        Label seqno = (Label)FormView1.FindControl("RfqSeqNoLabel");
        try
        {
            Session["list_rfq_grid_seqno"] = seqno.Text;
            Session["rfqcustpart"] = Session["list_rfq_grid_seqno"];


            if (seqno.Text == "0")
            {
                AddNewButton.Visible = true;
                FormView1.Visible = false;
            }
            else
            {
                AddNewButton.Visible = false;
            }
        }
        catch { }
    }

    protected void AddNewButton_Click(object sender, EventArgs e)
    {
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.Insert);
        AddNewButton.Visible = false;
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.ReadOnly)
            {
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];                
            }

            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                Image image4 = (Image)FormView1.FindControl("Image4");
               // image4.Visible = false;

                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];

                TextBox reqdate = (TextBox)FormView1.FindControl("req_dateTextBox");
                TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");
                TextBox color = (TextBox)FormView1.FindControl("RfqColTextBox");
                color.Text = "1";

                AddNewButton.Visible = false;

                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "rfqitem.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    if (aUsers == "external")
                    {                     
                        for (int i = 1; i <= 10; i++)
                        {
                            int dateimgval = i + 15;
                            TextBox price = (TextBox)FormView1.FindControl("lv_price_" + i);
                            TextBox date = (TextBox)FormView1.FindControl("lv_date_" + i);
                            Image dateimg = (Image)FormView1.FindControl("Image" + dateimgval);
                                 
                            price.Enabled = false;
                            date.Enabled = false;
                            dateimg.Visible = false;
                        }
                    }                 
                }
            }

            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                TextBox RfqQty = (TextBox)FormView1.FindControl("RfqQtyTextBox");
                RfqQty.Focus();
                Image image4 = (Image)FormView1.FindControl("Image4");
                //image4.Visible = false;

                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];

                TextBox reqdate = (TextBox)FormView1.FindControl("req_dateTextBox");
                TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");                

                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "rfqitem.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    if (aUsers == "external")
                    {
                        for (int i = 1; i <= 10; i++)
                        {
                            int dateimgval = i + 15;
                            TextBox price = (TextBox)FormView1.FindControl("lv_price_" + i);
                            TextBox date = (TextBox)FormView1.FindControl("lv_date_" + i);
                            Image dateimg = (Image)FormView1.FindControl("Image" + dateimgval);

                            price.Enabled = false;
                            date.Enabled = false;
                            dateimg.Visible = false;
                        }
                    }
                }
            }
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                TextBox RfqQty = (TextBox)FormView1.FindControl("RfqQtyTextBox");
                RfqQty.Focus();
            }

                       
        }
        catch { return; }
    }


    protected void StyleTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];        

        TextBox Rfqstyle = (TextBox)FormView1.FindControl("RfqstyleTextBox");
        TextBox RfqLength = (TextBox)FormView1.FindControl("RfqLengthTextBox");  

        rfqs rfq = new rfqs();
        string cerror = rfq.ValidateRfq("", UserLogin.UserName, "ValidateRfq", "", Rfqstyle.Text.Trim(), "", "");

        if (cerror != "")
        {           
            Rfqstyle.Text = "";
            HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");
            
            Rfqstyle.Focus();  
        }
        else
        {            
            RfqLength.Focus();
        }
    }

    protected void CategoryTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];        

        TextBox Rfqprocat = (TextBox)FormView1.FindControl("RfqProcatTextBox");
        TextBox Rfqcolor = (TextBox)FormView1.FindControl("RfqColTextBox");
        Table QHelp = (Table)FormView1.FindControl("Qtyhelp");
        

        /*HiddenField procathidden = (HiddenField)FormView1.FindControl("HiddenField2");
        string procatval = procathidden.Value; */

        rfqs rfq = new rfqs();
        string cerror = rfq.ValidateRfq("", UserLogin.UserName, "ValidateRfq", "", "", Rfqprocat.Text.Trim(), "");

        if (cerror != "")
        {
            Rfqprocat.Text = "";
            HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");

            //Page.ClientScript.RegisterStartupScript
                        //(this.GetType(), "showqty", "showQty();", true);
            Rfqprocat.Focus();               
        }
        else
        {
            //Page.ClientScript.RegisterStartupScript
                       // (this.GetType(), "showqty", "showQty();", true);
            Rfqcolor.Focus();            
        }
    }
    protected void BoardTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Rfqboard = (TextBox)FormView1.FindControl("RfqBoardTextBox");
        TextBox Qtytxt = (TextBox)FormView1.FindControl("TextBox1");
        TextBox Calipar = (TextBox)FormView1.FindControl("RfqCalTextBox");        

        rfqs rfq = new rfqs();
        string cerror = rfq.ValidateRfq("", UserLogin.UserName, "ValidateRfq", "", "", "", Rfqboard.Text.Trim());

        if (cerror != "")
        {
            Rfqboard.Text = "";
            HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");

            //Page.ClientScript.RegisterStartupScript
            //            (this.GetType(), "showqty", "showQty();", true);
            Rfqboard.Focus(); 
        }
        else
        {
            LookUp lookup = new LookUp();
            DataSet ds = new DataSet();           

            ds = lookup.BoardLook("search", UserLogin.UserName, "", "i-no", "EQUAL", Rfqboard.Text);
            Calipar.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);

            //Page.ClientScript.RegisterStartupScript
            //            (this.GetType(), "showqty", "showQty();", true);
            Qtytxt.Focus();
        }
    }

    //protected void cancelbutton_click(object sender, EventArgs e)
    //{
    //    Response.Write("<script>javascript:window.location.href='rfqitem.aspx'</script>");
    //}
}
