
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

public partial class purgecontact : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "purgecontact.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
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

      
        Response.Redirect(sMenuURL);
    }
     

    protected void DeleteButton_Click(object sender, EventArgs e)
    {

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {            
            conn.Open();
            if (firstnameTextBox.Text != "")
            {
                SqlCommand cmd = new SqlCommand("delete from contact where first_name = '" + firstnameTextBox.Text + "' ", conn);
                cmd.ExecuteNonQuery();
            }
            if (Company_TextBox.Text != "")
            {
                SqlCommand cmd = new SqlCommand("delete from contact where company = '" + Company_TextBox.Text + "' ", conn);
                cmd.ExecuteNonQuery();
            }
            if (firstnameTextBox.Text != "" && Company_TextBox.Text != "")
            {
                SqlCommand cmd = new SqlCommand("delete from contact where first_name='" + firstnameTextBox.Text + "' and cust_no = '" + Company_TextBox.Text + "' ", conn);
                cmd.ExecuteNonQuery();
            }
            


            if (firstnameTextBox.Text != "" && LastnameTextBox.Text != "")
            {
                int diff=firstnameTextBox.Text.CompareTo(LastnameTextBox.Text);
                //Response.Write(diff);
                if (diff > 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from contact where ( first_name <='" + firstnameTextBox.Text + "' and first_name>= '" + LastnameTextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff < 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from contact where ( first_name >='" + firstnameTextBox.Text + "' and first_name<= '" + LastnameTextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff ==0)
                {
                    SqlCommand cmd = new SqlCommand("delete from contact where ( first_name ='" + firstnameTextBox.Text + "' and first_name= '" + LastnameTextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
            }
            if (Company_TextBox.Text != "" && Companyto_TextBox.Text != "")
            {
                Int32 diff = Company_TextBox.Text.CompareTo(Companyto_TextBox.Text);
                if (diff > 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from contact where (cust_no <='" + Company_TextBox.Text + "' and cust_no>= '" + Companyto_TextBox.Text + "' )", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff < 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from contact where (cust_no >='" + Company_TextBox.Text + "' and cust_no<= '" + Companyto_TextBox.Text + "' )", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff == 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from contact where (cust_no ='" + Company_TextBox.Text + "' and cust_no= '" + Companyto_TextBox.Text + "' )", conn);
                    cmd.ExecuteNonQuery();
                }
            }
            if (firstnameTextBox.Text!="" && LastnameTextBox.Text!="" && Company_TextBox.Text != "" )
            {
                int diff = firstnameTextBox.Text.CompareTo(LastnameTextBox.Text);
                if(diff >0)
                {
                SqlCommand cmd = new SqlCommand("delete from contact where (first_name <='" + firstnameTextBox.Text + "' and first_name>= '" + LastnameTextBox.Text + "') and cust_no='"+ Company_TextBox.Text +"' ", conn);
                cmd.ExecuteNonQuery();
                }
                if (diff < 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from contact where (first_name >='" + firstnameTextBox.Text + "' and first_name<= '" + LastnameTextBox.Text + "') and cust_no='" + Company_TextBox.Text + "' ", conn);
                    cmd.ExecuteNonQuery();
                }
            }
            if (firstnameTextBox.Text != "" && LastnameTextBox.Text != "" && Company_TextBox.Text != "" && Companyto_TextBox.Text != "")
            {
                int diff = firstnameTextBox.Text.CompareTo(LastnameTextBox.Text);
                Int32 diff2 =Company_TextBox.Text.CompareTo(Companyto_TextBox.Text);
                if (diff > 0 && diff2 >0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name <='" + firstnameTextBox.Text + "' and first_name>= '" + LastnameTextBox.Text + "') and (cust_no <='" + Company_TextBox.Text + "' and cust_no>= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff < 0 && diff2 > 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name >='" + firstnameTextBox.Text + "' and first_name<= '" + LastnameTextBox.Text + "') and (cust_no <='" + Company_TextBox.Text + "' and cust_no>= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff > 0 && diff2 < 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name <='" + firstnameTextBox.Text + "' and first_name>= '" + LastnameTextBox.Text + "') and (cust_no >='" + Company_TextBox.Text + "' and cust_no<= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff < 0 && diff2 < 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name >='" + firstnameTextBox.Text + "' and first_name<= '" + LastnameTextBox.Text + "') and (cust_no >='" + Company_TextBox.Text + "' and cust_no<= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff == 0 && diff2 < 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name ='" + firstnameTextBox.Text + "' and first_name= '" + LastnameTextBox.Text + "') and (cust_no >='" + Company_TextBox.Text + "' and cust_no<= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff == 0 && diff2 > 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name ='" + firstnameTextBox.Text + "' and first_name= '" + LastnameTextBox.Text + "') and (cust_no <='" + Company_TextBox.Text + "' and cust_no>= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff > 0 && diff2 == 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name <='" + firstnameTextBox.Text + "' and first_name>= '" + LastnameTextBox.Text + "') and (cust_no ='" + Company_TextBox.Text + "' and cust_no= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff < 0 && diff2 == 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name >='" + firstnameTextBox.Text + "' and first_name<= '" + LastnameTextBox.Text + "') and (cust_no ='" + Company_TextBox.Text + "' and cust_no= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
                if (diff == 0 && diff2 == 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where (first_name ='" + firstnameTextBox.Text + "' and first_name= '" + LastnameTextBox.Text + "') and (cust_no ='" + Company_TextBox.Text + "' and cust_no= '" + Companyto_TextBox.Text + "') ", conn);
                    cmd.ExecuteNonQuery();
                }
            }
                
            conn.Close();
        }
        catch
        {
            return;
        }

        Label1.Text = "Record Deleted";
    }

}
