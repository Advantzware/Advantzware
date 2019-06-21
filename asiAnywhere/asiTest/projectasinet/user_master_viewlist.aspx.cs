using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class user_master_viewlist : System.Web.UI.Page
{
    private string userid = "";
    
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);

        if (!func.CheckUserPermissions("[dbo].[user_master]", "SA"))
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");


            Response.End();
        }
        
        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new 

System.Globalization.CultureInfo(nCulture, false);
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "user_master_viewlist.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);



            //lblComp.Text = PrmComp;
            if (aUsers == "external")
            {

                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;

            }
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
    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        //ClearSession();
        Response.Redirect(sMenuURL);
    }


    protected void lnk_Listview_Click(object sender, EventArgs e)
    {
        Response.Redirect("user_master_list.aspx");
    }
    protected void lnk_viewuser_Click(object sender, EventArgs e)
    {
        Response.Redirect("user_master_viewlist.aspx");
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            //Label id = (Label)FormView1.FindControl("UserIDLabel");
            //int value = Convert.ToInt32(userid.ToString());
            //id.Text =Convert.ToString(value+1);
            TextBox code = (TextBox)FormView1.FindControl("UsernameTextBox");
            code.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox code = (TextBox)FormView1.FindControl("nameTextBox");
            code.Focus();
        }
    }
    protected void Addnew_click(object sender, EventArgs e)
    {
        //Label userid = (Label)FormView1.FindControl("UserIDLabel");
        TextBox name = (TextBox)FormView1.FindControl("UsernameTextBox");
        TextBox mail = (TextBox)FormView1.FindControl("emailTextBox");
        TextBox pwd = (TextBox)FormView1.FindControl("PasswordTextBox");
        TextBox vname = (TextBox)FormView1.FindControl("nameTextBox");
        
        string prmAction = "Add";
        string vCompany = Convert.ToString(Session["cust_type_comp"]);
        string vUserId =  Convert.ToString(name.Text);
        string vUname = Convert.ToString(vname.Text);
             
        string vEmail = Convert.ToString(mail.Text);
        string vPassword = Convert.ToString(pwd.Text);
       
        
        contact_list c1 = new contact_list();
        
        c1.ViewUser(prmAction, vCompany, vUserId, vUname, vEmail, vPassword);

        Session["user_master_index"] = Session["tot_user_master_row"];
        Response.Write("<script>window.location.href='user_master_list.aspx'</script>");

    }
    protected void UpdateNew_Click(object sender, EventArgs e)
    {
        //Label userid = (Label)FormView1.FindControl("UserIDLabel1");
        Label name = (Label)FormView1.FindControl("UsernameTextBox");
        TextBox mail = (TextBox)FormView1.FindControl("emailTextBox");
        TextBox pwd = (TextBox)FormView1.FindControl("PasswordTextBox");
        TextBox vname = (TextBox)FormView1.FindControl("nameTextBox");

        string prmAction = "Update";
        string vCompany = Convert.ToString(Session["cust_type_comp"]);
        string vUserId = Convert.ToString(name.Text);
        string vUname = Convert.ToString(vname.Text);
        string vEmail = Convert.ToString(mail.Text);
        string vPassword = Convert.ToString(pwd.Text);
        
        contact_list c1 = new contact_list();
        
        c1.ViewUser(prmAction, vCompany, vUserId, vUname, vEmail, vPassword);
    }

    protected void AddButton_Click(object sender, EventArgs e)
    {
        //Label id = (Label)FormView1.FindControl("UserId2Label");
        //userid = id.Text;
       
    }
    protected void lnk_company_Click(object sender, EventArgs e)
    {
        Response.Redirect("user_master_company.aspx");
    }

    protected void FormView1_ItemInserted(object sender, FormViewInsertedEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record updated" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        Label name = (Label)FormView1.FindControl("UsernameLabel");
        Label mail = (Label)FormView1.FindControl("emailLabel");
        Label pwd = (Label)FormView1.FindControl("PasswordLabel");
        Label vname = (Label)FormView1.FindControl("nameLabel");

        string prmAction = "Delete";
        string vCompany = Convert.ToString(Session["cust_type_comp"]);
        string vUserId = Convert.ToString(name.Text);
        string vUname = Convert.ToString(vname.Text);
        string vEmail = Convert.ToString(mail.Text);
        string vPassword = Convert.ToString(pwd.Text);

        contact_list c1 = new contact_list();

        c1.ViewUser(prmAction, vCompany, vUserId, vUname, vEmail, vPassword);

        Response.Write("<script>window.location.href='user_master_list.aspx'</script>");
    }
}




