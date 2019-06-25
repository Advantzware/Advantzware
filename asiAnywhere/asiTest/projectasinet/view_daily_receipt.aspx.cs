using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for vieworder
/// </summary>
public partial class view_receipt : System.Web.UI.Page
{

    protected void Page_Load(object sender, EventArgs e)
    {
       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";

       
        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_daily_receipt.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
                lblComp.Text = PrmComp;
                lblUser.Text = UserLogin.UserName;

		
            }
		if (FormView1.DataItemCount < 0)
                {
                    addnewbutton.Visible = true;
			//Response.Write(FormView1.DataItemCount);
                }
                else
                {
                    addnewbutton.Visible = false;
			
                } 
        }

    }


    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void lnk_list_click(object sender, EventArgs e)
    {
        Response.Redirect("item_daily_receipt.aspx");
    }

    protected void lnk_view_click(object sender, EventArgs e)
    {
        Response.Redirect("view_daily_receipt.aspx");
    }
    protected void Insert_Button_Click(object sender, EventArgs e)
    {
        Label SeqNum = (Label)FormView1.FindControl("vSeqNumTextBox");
        TextBox VendorBolNum = (TextBox)FormView1.FindControl("vVendBolNoTextBox");
        TextBox UsgDate = (TextBox)FormView1.FindControl("vUsgDateTextBox");
        TextBox QtyUsed = (TextBox)FormView1.FindControl("vQtyUsedTextBox");
        TextBox CustPoNum = (TextBox)FormView1.FindControl("vCustPoNumTextBox");
        TextBox CustPoLineNum = (TextBox)FormView1.FindControl("vCustPoLineNumTextBox");
        TextBox CustPartNum = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox FgItmNum = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox CustVenCode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox CustPlantId = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
        TextBox CustDptCod = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
        TextBox VenOrdNum = (TextBox)FormView1.FindControl("vVenOrdNumTextBox");
        TextBox VenJobNum = (TextBox)FormView1.FindControl("vVenJobNumTextBox");
        TextBox VenJob2Num = (TextBox)FormView1.FindControl("vVenJob2NumTextBox");
        TextBox ItmSelPrice = (TextBox)FormView1.FindControl("vItmSelPriceTextBox");
        TextBox CustHandQty = (TextBox)FormView1.FindControl("vCustHandQtyTextBox");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        
        custitem cust = new custitem();
        DataSet ds = new DataSet();
        bool check = cust.ValidateUpdateReceipt(UserLogin.UserName, "ValidateAdd", 0, Convert.ToInt32(VendorBolNum.Text.Trim()), "", Convert.ToDateTime(UsgDate.Text.Trim()), Convert.ToDecimal(QtyUsed.Text), CustPoNum.Text.Trim(), Convert.ToInt32(CustPoLineNum.Text.Trim()), CustPartNum.Text.Trim(), FgItmNum.Text.Trim(), CustVenCode.Text.Trim(), CustPlantId.Text.Trim(), CustDptCod.Text.Trim(), Convert.ToInt32(VenOrdNum.Text.Trim()), VenJobNum.Text.Trim(), Convert.ToInt32(VenJob2Num.Text.Trim()), Convert.ToDecimal(ItmSelPrice.Text.Trim()), Convert.ToDecimal(CustHandQty.Text.Trim()), "");

        string value = Convert.ToString(check);
        if (value == "True")
        {
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
            ObjectDataSource1.SelectParameters["prmVendBolNo"].DefaultValue = VendorBolNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTranDate"].DefaultValue = UsgDate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQtyUsed"].DefaultValue = QtyUsed.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustPoNum"].DefaultValue = CustPoNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustLineNum"].DefaultValue = CustPoLineNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartNum"].DefaultValue = CustPartNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFgItmNum"].DefaultValue = FgItmNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustVnCode"].DefaultValue = CustVenCode.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustPlantId"].DefaultValue = CustPlantId.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustDptCode"].DefaultValue = CustDptCod.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustOrdNum"].DefaultValue = VenOrdNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustJobNum"].DefaultValue = VenJobNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustJob2Num"].DefaultValue = VenJob2Num.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSellingPrice"].DefaultValue = ItmSelPrice.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOnHandQty"].DefaultValue = CustHandQty.Text.Trim();
            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
        

    }
    protected void Update_Button_Click(object sender, EventArgs e)
    {
        Label SeqNum = (Label)FormView1.FindControl("vSeqNumTextBox");
        TextBox VendorBolNum = (TextBox)FormView1.FindControl("vVendBolNoTextBox");
        TextBox UsgDate = (TextBox)FormView1.FindControl("vUsgDateTextBox");
        TextBox QtyUsed = (TextBox)FormView1.FindControl("vQtyUsedTextBox");
        TextBox CustPoNum = (TextBox)FormView1.FindControl("vCustPoNumTextBox");
        TextBox CustPoLineNum = (TextBox)FormView1.FindControl("vCustPoLineNumTextBox");
        TextBox CustPartNum = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox FgItmNum = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox CustVenCode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox CustPlantId = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
        TextBox CustDptCod = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
        TextBox VenOrdNum = (TextBox)FormView1.FindControl("vVenOrdNumTextBox");
        TextBox VenJobNum = (TextBox)FormView1.FindControl("vVenJobNumTextBox");
        TextBox VenJob2Num = (TextBox)FormView1.FindControl("vVenJob2NumTextBox");
        TextBox ItmSelPrice = (TextBox)FormView1.FindControl("vItmSelPriceTextBox");
        TextBox CustHandQty = (TextBox)FormView1.FindControl("vCustHandQtyTextBox");
        Label TransType = (Label)FormView1.FindControl("vTransTypeTextBox");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

         custitem cust = new custitem();
        DataSet ds = new DataSet();
        bool check = cust.ValidateUpdateReceipt(UserLogin.UserName, "ValidateUpdate", 0, Convert.ToInt32(VendorBolNum.Text.Trim()), "", Convert.ToDateTime(UsgDate.Text.Trim()), Convert.ToDecimal(QtyUsed.Text), CustPoNum.Text.Trim(), Convert.ToInt32(CustPoLineNum.Text.Trim()), CustPartNum.Text.Trim(), FgItmNum.Text.Trim(), CustVenCode.Text.Trim(), CustPlantId.Text.Trim(), CustDptCod.Text.Trim(), Convert.ToInt32(VenOrdNum.Text.Trim()), VenJobNum.Text.Trim(), Convert.ToInt32(VenJob2Num.Text.Trim()), Convert.ToDecimal(ItmSelPrice.Text.Trim()), Convert.ToDecimal(CustHandQty.Text.Trim()), "");

        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmSeqNum"].DefaultValue = SeqNum.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = AdjustDate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmVendBolNo"].DefaultValue = VendorBolNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTranDate"].DefaultValue = UsgDate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQtyUsed"].DefaultValue = QtyUsed.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustPoNum"].DefaultValue = CustPoNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustLineNum"].DefaultValue = CustPoLineNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartNum"].DefaultValue = CustPartNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFgItmNum"].DefaultValue = FgItmNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustVnCode"].DefaultValue = CustVenCode.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustPlantId"].DefaultValue = CustPlantId.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustDptCode"].DefaultValue = CustDptCod.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustOrdNum"].DefaultValue = VenOrdNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustJobNum"].DefaultValue = VenJobNum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustJob2Num"].DefaultValue = VenJob2Num.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSellingPrice"].DefaultValue = ItmSelPrice.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOnHandQty"].DefaultValue = CustHandQty.Text.Trim();
            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }

    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmSeqNum"].DefaultValue = Convert.ToString(Session["itemreceipt_seq_no"]);

    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label sql = (Label)FormView1.FindControl("vSeqNumLabel");
            Session["itemreceipt_seq_no"] = sql.Text;
            
        }
        catch { }
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox qty = (TextBox)FormView1.FindControl("vQtyUsedTextBox");
            qty.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox seq = (TextBox)FormView1.FindControl("vVendBolNoTextBox");
            seq.Focus();
        }
    }
    protected void newbutton_click(object sender, EventArgs e)
    {
	    FormView1.ChangeMode(FormViewMode.Insert);
        addnewbutton.Visible = false;
	 }

    protected void bolno_textchange(object sender, EventArgs e)
    {
        Label SeqNum = (Label)FormView1.FindControl("vSeqNumTextBox");
        TextBox VendorBolNum = (TextBox)FormView1.FindControl("vVendBolNoTextBox");
        TextBox UsgDate = (TextBox)FormView1.FindControl("vUsgDateTextBox");
        TextBox QtyUsed = (TextBox)FormView1.FindControl("vQtyUsedTextBox");
        TextBox CustPoNum = (TextBox)FormView1.FindControl("vCustPoNumTextBox");
        TextBox CustPoLineNum = (TextBox)FormView1.FindControl("vCustPoLineNumTextBox");
        TextBox CustPartNum = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox FgItmNum = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox CustVenCode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox CustPlantId = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
        TextBox CustDptCod = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
        TextBox VenOrdNum = (TextBox)FormView1.FindControl("vVenOrdNumTextBox");
        TextBox VenJobNum = (TextBox)FormView1.FindControl("vVenJobNumTextBox");
        TextBox VenJob2Num = (TextBox)FormView1.FindControl("vVenJob2NumTextBox");
        TextBox ItmSelPrice = (TextBox)FormView1.FindControl("vItmSelPriceTextBox");
        TextBox CustHandQty = (TextBox)FormView1.FindControl("vCustHandQtyTextBox");
        Label error = (Label)FormView1.FindControl("Errorlabel");
        try
        {
            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.VendorBolLook(Convert.ToString(Session["User"]), "search", "BolNum", "EQUAL", VendorBolNum.Text.Trim());
            // Response.Write(ds.Tables[0].Rows[0][1].ToString());

            VendorBolNum.Text = ds.Tables[0].Rows[0][0].ToString();
            FgItmNum.Text = ds.Tables[0].Rows[0][1].ToString();
            VenJobNum.Text = ds.Tables[0].Rows[0][3].ToString();
            VenJob2Num.Text = ds.Tables[0].Rows[0][4].ToString();
            VenOrdNum.Text = ds.Tables[0].Rows[0][5].ToString();
            CustPoLineNum.Text = ds.Tables[0].Rows[0][6].ToString();
            CustPoNum.Text = ds.Tables[0].Rows[0][7].ToString();
            QtyUsed.Text = ds.Tables[0].Rows[0][8].ToString();
            CustVenCode.Text = ds.Tables[0].Rows[0][9].ToString();
            CustDptCod.Text = ds.Tables[0].Rows[0][10].ToString();
            CustPlantId.Text = ds.Tables[0].Rows[0][11].ToString();
            CustPartNum.Text = ds.Tables[0].Rows[0][12].ToString();
            CustHandQty.Text = ds.Tables[0].Rows[0][13].ToString();

            string getdate = ds.Tables[0].Rows[0][14].ToString();
            string[] val = getdate.Split(new char[] { ' ' });
            UsgDate.Text = val[0].ToString();
                        
            ItmSelPrice.Text = ds.Tables[0].Rows[0][15].ToString();
            error.Text = "";
        }
        catch
        {
            //HttpContext.Current.Response.Write("<script>alert('Invalid Bol Number')</script>");
            error.Text = "Invalid Bol Number";
        }
        
    }
    protected void Cancel_button_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
}

