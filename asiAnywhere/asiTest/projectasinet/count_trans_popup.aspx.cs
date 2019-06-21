
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class count_trans_popup : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_count_receipt.aspx";
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
                //Image14.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!Page.IsPostBack)
        {
            

            Label loc = (Label)FormView1.FindControl("vLocLabel");
            Label locbin = (Label)FormView1.FindControl("vLocBinLabel");
            TextBox1.Text = loc.Text;
            TextBox2.Text = locbin.Text;
            TextBox3.Text = loc.Text;
            TextBox4.Text = locbin.Text;                   
                        
            
        }
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
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {

        if (HiddenFieldPost.Value == "Yes")
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Transfer";
            ObjectDataSource1.SelectParameters["prmloc"].DefaultValue = TextBox1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmlocbin"].DefaultValue = TextBox2.Text.Trim(); ;
            ObjectDataSource1.SelectParameters["prmllSetParts"].DefaultValue = TextBox3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = TextBox4.Text.Trim();

            Response.Write("<script language=javascript>window.close();</script>");
        }
        
    }
    
}

