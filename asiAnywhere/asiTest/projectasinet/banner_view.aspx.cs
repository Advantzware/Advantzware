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

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class banner_view: System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "banner_view.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            //lblComp.Text = PrmComp;

            
        }
        if (!Page.IsPostBack)
        {
            lblUser.Text = UserLogin.UserName;
            if (Session["banner_url_add"] == null)
            {
                AddNewButton.Visible = true;
            }
            else
            {
                AddNewButton.Visible = false;
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
    protected void lnk_bannerlist_click(object sender, EventArgs e)
    {
        Response.Redirect("banner_list.aspx");
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            SqlCommand cmd = new SqlCommand("delete from setup where url_address='" + Session["banner_url_add"] + "'", conn);
            conn.Open();
            cmd.ExecuteNonQuery();
            conn.Close();
        }
        catch
        {
            return;
        }
        finally
        {
            //Response.Write("<script> alert('Record Deleted')</script>");
            Response.Write("<script>window.location.href='banner_list.aspx'</script>");
        }
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        TextBox address = (TextBox)FormView1.FindControl("url_addressTextBox");
        TextBox banner = (TextBox)FormView1.FindControl("bannerTextBox");
        CheckBox dloc = (CheckBox)FormView1.FindControl("DefaultCheckBox");
        
        
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            SqlCommand cmd = new SqlCommand("update setup set url_address='" + address.Text + "' , banner='" + banner.Text + "' ,default_val='"+ dloc.Checked +"' where url_address='" + Session["banner_url_add"] + "'", conn);
            conn.Open();
            cmd.ExecuteNonQuery();
            conn.Close();
        }
        catch
        {
            return;
        }
        
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        TextBox address = (TextBox)FormView1.FindControl("url_addressTextBox");
        TextBox banner = (TextBox)FormView1.FindControl("bannerTextBox");
        CheckBox dloc = (CheckBox)FormView1.FindControl("DefaultCheckBox");

        
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            SqlCommand cmd = new SqlCommand("insert into setup (url_address, banner,default_val) values('" + address.Text + "','" + banner.Text + "','" +  dloc.Checked + "')",conn);
            conn.Open();
            cmd.ExecuteNonQuery();
            conn.Close();
        }
        catch
        {
            return;
        }
        finally
        {
            //Response.Write("<script> alert('Record Deleted')</script>");
            Response.Write("<script>window.location.href='banner_list.aspx'</script>");
        }     

    }
    
    protected void AddNewButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        AddNewButton.Visible = false;
    }


   }
