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
public partial class view_daily_usag : System.Web.UI.Page
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
                string vPage = "view_daily_usag.aspx";
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
        Response.Redirect("item_daily_usag.aspx");
    }

    protected void lnk_view_click(object sender, EventArgs e)
    {
        Response.Redirect("view_daily_usag.aspx");
    }
    protected void FormView1_DataBound(object sender, EventArgs e )
    {
        
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox usg = (TextBox)FormView1.FindControl("vUsgDateTextBox");
            usg.Focus();

        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label fgitem = (Label)FormView1.FindControl("vFgItmNumLabel");
                Label vandercode = (Label)FormView1.FindControl("vCustVenCodeLabel");
                Label deptcode = (Label)FormView1.FindControl("vCustDptCodLabel");
                Label line = (Label)FormView1.FindControl("vCustPoLineNumLabel");
                HiddenFGitem.Value = fgitem.Text;
                HiddenVanderCode.Value = vandercode.Text;
                HiddenDeptCode.Value = deptcode.Text;
                HiddenLine.Value = line.Text;
            }
            catch { }
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            HiddenFGitem.Value = "";
            HiddenVanderCode.Value = "";
            HiddenDeptCode.Value = "";
            HiddenLine.Value = "";
            TextBox usg = (TextBox)FormView1.FindControl("vUsgDateTextBox");
            usg.Focus();
        }
    }
   
    protected void Insert_Button_Click(object sender, EventArgs e)
    {
       
        Label SeqNum = (Label)FormView1.FindControl("vSeqNumTextBox");
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
        Label CustHandQty = (Label)FormView1.FindControl("vCustHandQtyTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";        
        ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = UsgDate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmQtyUsed"].DefaultValue = QtyUsed.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPoNum"].DefaultValue = CustPoNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustLineNum"].DefaultValue = HiddenLine.Value;
        ObjectDataSource1.SelectParameters["prmCustPartNum"].DefaultValue = CustPartNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItemNum"].DefaultValue = HiddenFGitem.Value;
        ObjectDataSource1.SelectParameters["prmCustVnCode"].DefaultValue = HiddenVanderCode.Value;
        ObjectDataSource1.SelectParameters["prmCustPlantId"].DefaultValue = CustPlantId.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustDptCode"].DefaultValue = HiddenDeptCode.Value;
        ObjectDataSource1.SelectParameters["prmCustOrdNum"].DefaultValue = VenOrdNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustJobNum"].DefaultValue = VenJobNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustJob2Num"].DefaultValue = VenJob2Num.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSellingPrice"].DefaultValue = ItmSelPrice.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOnHandQty"].DefaultValue = CustHandQty.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        

    }
    protected void Update_Button_Click(object sender, EventArgs e)
    {
        Label SeqNum = (Label)FormView1.FindControl("vSeqNumTextBox");
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
        Label CustHandQty = (Label)FormView1.FindControl("vCustHandQtyTextBox");
        Label TransType = (Label)FormView1.FindControl("vTransTypeTextBox");

       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmSeqNum"].DefaultValue = SeqNum.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = AdjustDate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = UsgDate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmQtyUsed"].DefaultValue = QtyUsed.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPoNum"].DefaultValue = CustPoNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustLineNum"].DefaultValue = HiddenLine.Value.Trim();
        ObjectDataSource1.SelectParameters["prmCustPartNum"].DefaultValue = CustPartNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItemNum"].DefaultValue = HiddenFGitem.Value;
        ObjectDataSource1.SelectParameters["prmCustVnCode"].DefaultValue = HiddenVanderCode.Value;
        ObjectDataSource1.SelectParameters["prmCustPlantId"].DefaultValue = CustPlantId.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustDptCode"].DefaultValue = HiddenDeptCode.Value;
        ObjectDataSource1.SelectParameters["prmCustOrdNum"].DefaultValue = VenOrdNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustJobNum"].DefaultValue = VenJobNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustJob2Num"].DefaultValue = VenJob2Num.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSellingPrice"].DefaultValue = ItmSelPrice.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOnHandQty"].DefaultValue = CustHandQty.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmSeqNum"].DefaultValue = Convert.ToString(Session["itemdaily_seq_no"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label sql = (Label)FormView1.FindControl("vSeqNumLabel");
            Session["itemdaily_seq_no"] = sql.Text;
            
        }
        catch { }
    }
    protected void newbutton_click(object sender, EventArgs e)
    {
	FormView1.ChangeMode(FormViewMode.Insert);
        addnewbutton.Visible = false;
	 }

    protected void cust_part_txt_Click(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
            TextBox item = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
            TextBox apcode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
            TextBox plantid = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
            TextBox deptcode = (TextBox)FormView1.FindControl("vCustDptCodTextBox");

            
            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.UsagCustPartLook("search", UserLogin.UserName,"","Part-no","EQUAL", custpart.Text,custpart.Text);
            custpart.Text = ds.Tables[0].Rows[0][0].ToString();
            item.Text = ds.Tables[0].Rows[0][1].ToString();
            apcode.Text = ds.Tables[0].Rows[0][2].ToString();
            plantid.Text = ds.Tables[0].Rows[0][3].ToString();
            deptcode.Text = ds.Tables[0].Rows[0][4].ToString();
           
            HiddenFGitem.Value = item.Text;
            HiddenVanderCode.Value = apcode.Text;
            HiddenDeptCode.Value = deptcode.Text;
            plantid.Focus();
                      
        }
        catch
        {
            TextBox item = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
            TextBox apcode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
            TextBox plantid = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
            TextBox deptcode = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
            item.Text = "";
            apcode.Text = "";
            plantid.Text = "";
            deptcode.Text = "";
        }
    }
    protected void custplantid_txt_Click(object sender, EventArgs e)
    {

        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
            TextBox item = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
            TextBox apcode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
            TextBox plantid = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
            TextBox deptcode = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
            TextBox order = (TextBox)FormView1.FindControl("vVenOrdNumTextBox");

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.UsagPlantLook("search", UserLogin.UserName, "", "Plantid", "EQUAL", plantid.Text, item.Text, apcode.Text);
            
            plantid.Text = ds.Tables[0].Rows[0][1].ToString();
            deptcode.Text = ds.Tables[0].Rows[0][2].ToString();                        
            HiddenDeptCode.Value = deptcode.Text;
            order.Focus();

        }
        catch
        {
           
            TextBox plantid = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
            TextBox deptcode = (TextBox)FormView1.FindControl("vCustDptCodTextBox");          
            plantid.Text = "";
            deptcode.Text = "";
        }
    }

    protected void Custpo_Text_Click(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
            TextBox item = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
            TextBox apcode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
            TextBox plantid = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
            TextBox deptcode = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
            TextBox po = (TextBox)FormView1.FindControl("vCustPoNumTextBox");
            TextBox line = (TextBox)FormView1.FindControl("vCustPoLineNumTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vVenJobNumTextBox");
            TextBox job2 = (TextBox)FormView1.FindControl("vVenJob2NumTextBox");
            TextBox order = (TextBox)FormView1.FindControl("vVenOrdNumTextBox");
            TextBox price = (TextBox)FormView1.FindControl("vItmSelPriceTextBox");

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.UsagPoLook("search", UserLogin.UserName, "", "pono", "EQUAL", po.Text);
            po.Text = ds.Tables[0].Rows[0][1].ToString();
            item.Text = ds.Tables[0].Rows[0][0].ToString();
            order.Text = ds.Tables[0].Rows[0][2].ToString();
            line.Text = ds.Tables[0].Rows[0][3].ToString();
            custpart.Text = ds.Tables[0].Rows[0][5].ToString();            
            apcode.Text = ds.Tables[0].Rows[0][6].ToString();

            plantid.Text = ds.Tables[0].Rows[0][7].ToString();
            deptcode.Text = ds.Tables[0].Rows[0][8].ToString();
            job.Text = ds.Tables[0].Rows[0][9].ToString();
            job2.Text = ds.Tables[0].Rows[0][10].ToString();
            price.Text = ds.Tables[0].Rows[0][11].ToString();

            HiddenFGitem.Value = item.Text;
            HiddenVanderCode.Value = apcode.Text;
            HiddenDeptCode.Value = deptcode.Text;
            HiddenLine.Value = line.Text;
            custpart.Focus();

        }
        catch
        {
            TextBox item = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
            TextBox apcode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
            TextBox plantid = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
            TextBox deptcode = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
            TextBox po = (TextBox)FormView1.FindControl("vCustPoNumTextBox");
            item.Text = "";
            apcode.Text = "";
            plantid.Text = "";
            deptcode.Text = "";
            po.Focus();
        }
    }
}

