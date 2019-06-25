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
public partial class view_cross_reference : System.Web.UI.Page
{

    protected void Page_Load(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";       

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_cross_reference.aspx";
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

    protected void lnk_browse_click(object sender, EventArgs e)
    {
        Response.Redirect("vendor_cross_reference.aspx");
    }

    protected void lnk_view_click(object sender, EventArgs e)
    {
        Response.Redirect("view_cross_reference.aspx");
    }
    protected void FormView1_DataBound(object sender, EventArgs e )
    {
        
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox code = (TextBox)FormView1.FindControl("vCustNoTextBox");
            code.Focus();

        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox code = (TextBox)FormView1.FindControl("vCustNoTextBox");
            code.Focus();

        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
               
            }
            catch { }
        }
    }
   
    protected void Insert_Button_Click(object sender, EventArgs e)
    {
        TextBox CustNo = (TextBox)FormView1.FindControl("vCustNoTextBox");
        TextBox VendorCode = (TextBox)FormView1.FindControl("vVendorCodeTextBox");
        TextBox CustNum = (TextBox)FormView1.FindControl("vCustNameTextBox");
        Session["crossref_arcode_txt_view"] = CustNo.Text.Trim();
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmCustNo"].DefaultValue = Convert.ToString(Session["crossref_arcode_txt_view"]);
        ObjectDataSource1.SelectParameters["prmVendorCode"].DefaultValue = VendorCode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = CustNum.Text.Trim();                          
    }
    protected void Update_Button_Click(object sender, EventArgs e)
    {

        TextBox CustNo = (TextBox)FormView1.FindControl("vCustNoTextBox");
        TextBox VendorCode = (TextBox)FormView1.FindControl("vVendorCodeTextBox");
        TextBox CustNum = (TextBox)FormView1.FindControl("vCustNameTextBox");
        Session["crossref_arcode_txt_view"] = CustNo.Text.Trim();  
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmCustNo"].DefaultValue = Convert.ToString(Session["crossref_arcode_txt_view"]);
        ObjectDataSource1.SelectParameters["prmVendorCode"].DefaultValue = VendorCode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = CustNum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUpdateCustno"].DefaultValue = Convert.ToString(Session["crossref_arcode_txt_view_update"]);

    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmCustNo"].DefaultValue = Convert.ToString(Session["crossref_arcode_txt_view"]);

    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label cust = (Label)FormView1.FindControl("vCustNoLabel");
            Session["crossref_arcode_txt_view"] = cust.Text;
            Session["crossref_arcode_txt_view_update"] = cust.Text;
            
        }
        catch { }
    }
    protected void newbutton_click(object sender, EventArgs e)
    {
	FormView1.ChangeMode(FormViewMode.Insert);
        addnewbutton.Visible = false;
	 }
    protected void Cancil_button_click(object sender, EventArgs e)
    {
        Session["crossref_arcode_txt_view"] = Session["crossref_arcode_txt_view_update"];
    }
}

