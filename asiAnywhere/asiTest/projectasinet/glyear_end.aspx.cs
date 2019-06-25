
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

public partial class glyear_end : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "glyear_end.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
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
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;            



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'glyear_end.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    whyearTextBox.Text = dr["field1"].ToString();                    
                    
                   
                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;                  

                                                 
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }            
            

            
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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        if (CheckBox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }                                     

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "yrclo";
        ObjectDataSource1.SelectParameters["prmyear"].DefaultValue = whyearTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmmove"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenFieldPost.Value;
        

       
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'glyear_end.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, chk_field1) values ('" + UserLogin.UserName + "','glyear_end.aspx' , '" + whyearTextBox.Text.Trim() + "', '" + CheckBox1.Checked + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + whyearTextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'glyear_end.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();

            
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }
        try
        {

            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("yrcloLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='glyear_end.aspx';</script>");
            }
        }
        catch { }         
    }
    
}

