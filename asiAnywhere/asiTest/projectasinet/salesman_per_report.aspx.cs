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

public partial class salesman_report_list : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "salesman_per_report.aspx";
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


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!Page.IsPostBack)
        {
            endsmanTextBox.Text = "zzz";
            HyperLink1.Visible = false;
            OutPutFile.Visible = false;
            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }

            if (TextBox1.Text == "")
            {
                TextBox1.Text = System.DateTime.Today.ToString("MM/dd/yyyy"); 
            }
            RadioButtonList_out.SelectedIndex = 0;

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'salesman_per_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    besmanTextBox.Text = dr["field1"].ToString();
                    endsmanTextBox.Text = dr["field2"].ToString();                    
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

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmSalesAct"].DefaultValue = "Salesman";
        ObjectDataSource1.SelectParameters["prmBeDate"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeSales"].DefaultValue = besmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSales"].DefaultValue = endsmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'salesman_per_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2) values ('" + UserLogin.UserName + "','salesman_per_report.aspx' , '" + besmanTextBox.Text.Trim() + "', '" + endsmanTextBox.Text.Trim()+"')" , conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + besmanTextBox.Text.Trim() + "', field2 = '" + endsmanTextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'salesman_per_report.aspx' ", conn);
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

        OutPutFile.Visible = true;
        HyperLink1.Visible = true;
        Label path = (Label)FormView1.FindControl("vsalesmanLabel");
        HyperLink1.Text = path.Text;
        HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

        if (path.Text == "")
        {
            OutPutFile.Text = "No CSV Exists";
            Response.Write("<script>window.location.href'salesman_per_report.aspx'</script>");
        }


    }

}
