
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

public partial class purge_notes_list : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "purge_notes.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
           
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
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }
            
            if (func.CheckUserPermissions("[dbo].[cust]", "s"))
            {
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
            if (ContactTextBox.Text != "")
            {
                SqlCommand cmd = new SqlCommand("delete from notes where rec_key=(select rec_key from contact where first_name='"+ ContactTextBox.Text +"')", conn);
                cmd.ExecuteNonQuery();
            }
            if (ContactTextBox.Text != "" && ToContactTextBox.Text!="")
            {
                SqlCommand fc = new SqlCommand("select rec_key from contact where first_name ='" + ContactTextBox.Text + "'", conn);
                Int64 fv =Convert.ToInt64(fc.ExecuteScalar());
                //Response.Write(fv);
                SqlCommand sc = new SqlCommand("select rec_key from contact where first_name ='" + ToContactTextBox.Text + "'", conn);
                Int64 sv = Convert.ToInt64(sc.ExecuteScalar());
                //Response.Write(sv);
                if (fv > sv)
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where (rec_key<=(select rec_key from contact where first_name ='" + ContactTextBox.Text + "') and rec_key>=(select rec_key from contact where first_name='" + ToContactTextBox.Text + "'))", conn);
                    cmd.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where (rec_key>=(select rec_key from contact where first_name ='" + ContactTextBox.Text + "') and rec_key<=(select rec_key from contact where first_name='" + ToContactTextBox.Text + "'))", conn);
                    cmd.ExecuteNonQuery();
                }
                if (fv == sv)
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where rec_key=(select rec_key from contact where first_name='" + ContactTextBox.Text + "')", conn);
                    cmd.ExecuteNonQuery();
                }
            }
            //if (ContactTextBox.Text != "" && ToContactTextBox.Text != "" && DateTextBox.Text!="")
            //{
            //    SqlCommand cmd = new SqlCommand("delete from notes where rec_key=(select rec_key from contact where (first_name >='" + ContactTextBox.Text + "' and first_name<='" + ToContactTextBox.Text + "')) and note_date='" + DateTextBox.Text + "'", conn);
            //    cmd.ExecuteNonQuery();
            //}
            //if (ContactTextBox.Text != "" && ToContactTextBox.Text != "" && DateTextBox.Text != "" && ToDateTextBox.Text!="")
            //{
            //    SqlCommand cmd = new SqlCommand("delete from notes where rec_key=(select rec_key from contact where (first_name >='" + ContactTextBox.Text + "' and first_name<='" + ToContactTextBox.Text + "')) and (note_date>='" + DateTextBox.Text + "' and note_date<='"+ ToDateTextBox.Text +"')", conn);
            //    cmd.ExecuteNonQuery();
            //}

            if (FromcustomerTextBox.Text != "")
            {
                //SqlCommand fc = new SqlCommand("select rec_key from contact where cust_no ='" + FromcustomerTextBox.Text + "'", conn);
                //Int64 fv = Convert.ToInt64(fc.ExecuteScalar());
                //Response.Write(fv);
                SqlCommand cmd = new SqlCommand("delete from notes where rec_key=(select rec_key from contact where cust_no ='" + FromcustomerTextBox.Text + "')", conn);
                cmd.ExecuteNonQuery();
            }
            if (FromcustomerTextBox.Text != "" && TocustomerTextBox.Text != "")
            {

                SqlCommand fc = new SqlCommand("select rec_key from contact where cust_no ='" + FromcustomerTextBox.Text + "'", conn);
                Int64 fv = Convert.ToInt64(fc.ExecuteScalar());
                //Response.Write(fv);
                SqlCommand sc = new SqlCommand("select rec_key from contact where cust_no ='" + TocustomerTextBox.Text + "'", conn);
                Int64 sv = Convert.ToInt64(sc.ExecuteScalar());
                //Response.Write(sv);
                if (fv > sv)
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where (rec_key<=(select rec_key from contact where cust_no ='" + FromcustomerTextBox.Text + "') and rec_key>=(select rec_key from contact where cust_no='" + TocustomerTextBox.Text + "'))", conn);
                    cmd.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where (rec_key>=(select rec_key from contact where cust_no ='" + FromcustomerTextBox.Text + "') and rec_key<=(select rec_key from contact where cust_no='" + TocustomerTextBox.Text + "'))", conn);
                    cmd.ExecuteNonQuery();
                }
                if (fv == sv)
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where rec_key=(select rec_key from contact where cust_no='" + FromcustomerTextBox.Text + "')", conn);
                    cmd.ExecuteNonQuery();
                }
            }


            if (DateTextBox.Text != "")
            {
                SqlCommand cmd = new SqlCommand("delete from notes where note_date='" + DateTextBox.Text + "'", conn);
                cmd.ExecuteNonQuery();
            }
            if (DateTextBox.Text != "" && ToDateTextBox.Text!="")
            {
                DateTime fd=Convert.ToDateTime(DateTextBox.Text);
                DateTime sd=Convert.ToDateTime(ToDateTextBox.Text);
                TimeSpan dt =fd.Subtract(sd) ;
                //Response.Write(dt.Days);
                if (dt.Days < 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where (note_date >='" + DateTextBox.Text + "'  and note_date<= '" + ToDateTextBox.Text + "')", conn);
                    cmd.ExecuteNonQuery();
                }
                if (dt.Days > 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where (note_date <='" + DateTextBox.Text + "'  and note_date>= '" + ToDateTextBox.Text + "')", conn);
                    cmd.ExecuteNonQuery();
                }
                if (dt.Days == 0)
                {
                    SqlCommand cmd = new SqlCommand("delete from notes where (note_date ='" + DateTextBox.Text + "'  and note_date= '" + ToDateTextBox.Text + "')", conn);
                    cmd.ExecuteNonQuery();
                }
            }
            conn.Close();
        }
        catch
        {
            return;
        }
        finally
        {
            conn.Close();
        }
        Label1.Text = "Record Deleted";
    }

}
