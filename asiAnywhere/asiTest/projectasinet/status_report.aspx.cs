
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

public partial class StatusReport : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {


       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "status_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["Trans_login"] = PrmComp;
            if (aUsers == "external")
            {
                endcust_TextBox.ReadOnly = false;
                Image12.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutputLabel.Visible = false;
            HyperLink1.Visible = false;

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Trans_login"]);


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'status_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);



                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begincust_TextBox.Text = dr["field1"].ToString();
                    endcust_TextBox.Text = dr["field2"].ToString();
                    begitem_TextBox.Text = dr["field3"].ToString();
                    enditem_TextBox.Text = dr["field4"].ToString();                                                                    
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

            try
            {
                if (begincust_TextBox.Text == "")
                {
                    Label custn = (Label)FormView2.FindControl("CustLabel");
                    begincust_TextBox.Text = custn.Text;
                    endcust_TextBox.Text = custn.Text;
                }

                if (enditem_TextBox.Text == "")
                    enditem_TextBox.Text = "zzzzzzzzzzzzzzz";                        

                RadioButtonList8.SelectedIndex = 0;
            }
            catch { }           
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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {        
        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField1.Value = "no";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField1.Value = "yes";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "statusrep";
        ObjectDataSource1.SelectParameters["prmBegCust"].DefaultValue = begincust_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = endcust_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = begitem_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = enditem_TextBox.Text.Trim();                       
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField1.Value;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'status_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4) values ('" + UserLogin.UserName + "','status_report.aspx' , '" + begincust_TextBox.Text.Trim() + "', '" + endcust_TextBox.Text.Trim() + "', '" + begitem_TextBox.Text.Trim() + "', '" + enditem_TextBox.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begincust_TextBox.Text.Trim() + "', field2 = '" + endcust_TextBox.Text.Trim() + "', field3 = '" + begitem_TextBox.Text.Trim() + "', field4 = '" + enditem_TextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'status_report.aspx' ", conn);
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
            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            Label path = (Label)FormView1.FindControl("vStatRepFileLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                Response.Write("<script>window.location.href='status_report.aspx'</script>");
            }
        }
        catch { }        
      
    }    
}